package parser

import (
	"regexp"
	"strconv"
	"strings"

	"c20/internal/ast"
	"c20/internal/loader"
)

// ParseUnits 會把多個來源（可能是 .REP 或 .CBL 文本）轉成多個 AST Program。
// - 這層只做「輕量 parser」：把 Data Division 的平面資料項抓出、把 Procedure Division 的簡化指令抓出。
// - 更完整（如 SECTION/PARAGRAPH、PERFORM、CALL/READ/OPEN/…）目前先略過；遇到未支援語句會被忽略。
// - 產出中包含 lineIndex（語句索引→原始行號），方便之後報告/對照。
func ParseUnits(sources []loader.Source) []ast.Program {
	var out []ast.Program
	for _, src := range sources {
		p := parseOne(src)
		out = append(out, p)
	}
	return out
}

// parseOne 對單一來源檔做解析。
// 步驟：
//   1) normalizeLines：先把（loader 已處理過的）文字再做一次保守規則的行清理/醒目。
//   2) 切出 PROCEDURE DIVISION 的分界，前半給 Data Division、後半給 Procedure Division。
//   3) parseDataDivision：平面抓取 77/01/… 的項目（忽略 group/88-level，僅保留 PIC）
//   4) parseProcedureDivision：解析支援子集（IF/MOVE/DISPLAY/ACCEPT/SUBTRACT/STOP RUN 等）
//   5) 估出 Program ID（PROGRAM-ID. ...），沒有就回退為 "MAIN"
func parseOne(src loader.Source) ast.Program {
	lines := normalizeLines(src.Text)

	// 找 PROCEDURE DIVISION 的起始（大小寫不敏感）
	procIdx := indexOf(lines, func(s string) bool {
		return hasPrefixI(s, "PROCEDURE") && strings.Contains(strings.ToUpper(s), "DIVISION")
	})

	// 若找不到，就整份當成 Data Division；找得到就切兩段
	dataLines := lines
	procLines := []string{}
	if procIdx >= 0 {
		dataLines = lines[:procIdx]
		procLines = lines[procIdx:]
	}

	data := parseDataDivision(dataLines)
	stmts, lineIndex := parseProcedureDivision(procLines)

	return ast.Program{
		ID:        guessProgramID(lines),
		Source:    src.Name, // 來源檔名（由 loader 傳入）
		Data:      data,
		Stmts:     stmts,
		LineIndex: lineIndex,
	}
}

// ───────────────────────────────────────────────────────────────────────────────
// DATA DIVISION
// ───────────────────────────────────────────────────────────────────────────────

// reLevelPic: 解析一行「<層級> <名稱> PIC <描述>.」的資料項。
//   e.g. "01  CUSTOMER-NO     PIC X(10)."
//   e.g. "77  AMOUNT          PIC S9(6)V9(2) COMP-3."
//   允許 "REDEFINES <xxx>" 夾在名稱與 PIC 之間（直接略過，不建群組）。
var (
	reLevelPic  = regexp.MustCompile(`^\s*(\d{2})\s+([A-Z0-9\-]+)\s+(?:REDEFINES\s+[A-Z0-9\-]+\s+)?PIC\s+(.+?)\.\s*$`)
	// reLevelOnly: 僅有層級與名稱（如 group header）："01 GROUP-NAME."
	reLevelOnly = regexp.MustCompile(`^\s*(\d{2})\s+([A-Z0-9\-]+)\s*\.\s*$`)
	// rePicNumParts: 數值 PIC 的拆解：
	//   - 可含 S 前綴（符號）
	//   - 主位數 9(d)
	//   - 小數位 V9(s)
	//   - COMP-3（packed decimal）只是記在欄位上，實作交給後端
	//   範例："9(8)"、"S9(6)V9(2)"、"9(3) COMP-3"
	rePicNumParts = regexp.MustCompile(`(?i)S?9(?:\((\d+)\))?(?:V9(?:\((\d+)\))?)?(?:\s+COMP-3)?`)
	reHasComp3    = regexp.MustCompile(`(?i)\bCOMP-3\b`)
)

// parsePicSpec 把 PIC 描述轉成 ast.PicSpec。
// 支援：X(n) / X(n) COMP-X / S?9(d)[V9(s)] [COMP-3]
// 回傳 (spec, ok)。不支援的 PIC 會回 false（呼叫端通常略過該欄位）。
func parsePicSpec(picText string) (ast.PicSpec, bool) {
	txt := strings.TrimSpace(strings.ToUpper(picText))

	// 字母型：X(n) 或 X(n) COMP-X
	if m := regexp.MustCompile(`^[X]\((\d+)\)(?:\s+COMP-X)?$`).FindStringSubmatch(txt); m != nil {
		n, _ := strconv.Atoi(m[1])
		return ast.PicSpec{Kind: ast.PicAlpha, Len: n}, true
	}

	// 數值型：S?9(d)[V9(s)] [COMP-3]
	if m := rePicNumParts.FindStringSubmatch(txt); m != nil {
		d := 1
		if m[1] != "" {
			d, _ = strconv.Atoi(m[1])
		}
		scale := 0
		if m[2] != "" {
			scale, _ = strconv.Atoi(m[2])
		}
		return ast.PicSpec{
			Kind:   ast.PicNumeric,
			Digits: d + scale,                    // 總位數（含小數位）
			Scale:  scale,                        // 小數位數
			Signed: strings.HasPrefix(txt, "S9"), // 是否帶號
			Comp3:  reHasComp3.MatchString(txt),  // 是否出現 COMP-3 標記
		}, true
	}

	return ast.PicSpec{}, false
}

// parseDataDivision：以「扁平」方式擷取 Data Items。
// - 跳過：空行、註解（* 開頭）、DIVISION 標頭、FD/FILE/SELECT/COPY、88-level、group header。
// - 僅保留有 PIC 的項目（reLevelPic），產出 ast.DataItem。
// - Group/REDEFINES/88 等語意目前不建樹；僅提供基本欄位資訊給 emitter。
func parseDataDivision(lines []string) []ast.DataItem {
	var out []ast.DataItem
	for _, ln := range lines {
		u := strings.ToUpper(strings.TrimSpace(ln))
		if u == "" || strings.HasPrefix(u, "*") {
			continue
		}
		// 跳過明顯非資料定義的區塊（或我們暫不處理的段落）
		if strings.Contains(u, "DIVISION.") || strings.HasPrefix(u, "FD ") || strings.HasPrefix(u, "FILE ") || strings.HasPrefix(u, "SELECT ") || strings.HasPrefix(u, "COPY ") {
			continue
		}
		// 88-level（條件名稱）不視為具體資料欄位
		if strings.HasPrefix(u, "88 ") {
			continue
		}

		// 解析「層級+名稱+PIC」
		if m := reLevelPic.FindStringSubmatch(u); m != nil {
			lv, _ := strconv.Atoi(m[1])
			name := m[2]
			picText := m[3]
			pic, ok := parsePicSpec(picText)
			if !ok {
				continue
			}
			out = append(out, ast.DataItem{
				Level: lv,
				Name:  name,
				Pic:   pic,
			})
			continue
		}

		// 純層級（group header），目前不建 group，直接忽略。
		if m := reLevelOnly.FindStringSubmatch(u); m != nil {
			_ = m
			continue
		}
	}
	return out
}

// ───────────────────────────────────────────────────────────────────────────────
// PROCEDURE DIVISION（支援子集）
// ───────────────────────────────────────────────────────────────────────────────

// parseProcedureDivision 解析 Procedure Division 的語句，回傳：
// - stmts：線性語句列（包含 if-block 的 then/else 內嵌）
// - lineIndex：語句索引 → 原始行號（便於報告/錯誤定位）
//
// 支援語句：IF/ELSE/END-IF（含單行 THEN ... END-IF）、MOVE、DISPLAY、ACCEPT、SUBTRACT、UNSTRING、STOP RUN。
// 暫不支援：OPEN/CLOSE/READ/PERFORM/CALL/GO TO…（會被忽略）。
func parseProcedureDivision(lines []string) ([]ast.Stmt, map[int]int) {
	var stmts []ast.Stmt
	lineIndex := make(map[int]int)
	if len(lines) == 0 {
		return stmts, lineIndex
	}

	// 若第一行就是 "PROCEDURE DIVISION." 就略過它
	i := 0
	if hasPrefixI(lines[0], "PROCEDURE") {
		i++
	}

	// 小工具：把語句加到目前目標（可能是頂層 stmts 或某個 IF 的 then/else 區）並記錄原始行號
	appendStmt := func(dst *[]ast.Stmt, s ast.Stmt, srcLine int) {
		*dst = append(*dst, s)
		lineIndex[len(lineIndex)] = srcLine
	}

	// 以堆疊管理巢狀 IF 區塊
	type ifFrame struct {
		cond    ast.Bool    // IF 條件
		parent  *[]ast.Stmt // 上一層容器（頂層或外層 IF 的 then/else）
		thenB   []ast.Stmt  // THEN 區塊
		elseB   []ast.Stmt  // ELSE 區塊
		inElse  bool        // 目前是否在 else 區
		srcLine int         // IF 起始行號（給 lineIndex）
	}
	var (
		cur   *[]ast.Stmt = &stmts // 目前寫入目標（預設指向頂層 stmts）
		ifstk []ifFrame           // IF 堆疊
	)

	inDecl := false // DECLARATIVES 區塊：整段跳過

	for idx := i; idx < len(lines); idx++ {
		raw := lines[idx]
		srcLine := idx

		// cleanStmtLine：移除尾部行內 * 註解、合併空白、拿掉行首/尾空白；若整行是註解，回空字串。
		s := cleanStmtLine(raw)
		if s == "" {
			continue
		}
		u := strings.ToUpper(s)

		// DECLARATIVES 區塊整段忽略（直到 END DECLARATIVES）
		if hasPrefixI(u, "DECLARATIVES") {
			inDecl = true
			continue
		}
		if inDecl {
			if hasPrefixI(u, "END DECLARATIVES") {
				inDecl = false
			}
			continue
		}
		if hasPrefixI(u, "END DECLARATIVES") {
			// 安全性：理論上上面已處理
			continue
		}

		// 純標籤/段落名稱/SECTION 標頭（如 "MAIN-RTN SECTION.", "0000-EXIT."）直接略過
		if looksLikeLabel(u) {
			continue
		}

		// UNSTRING（可能橫跨多行直到 END-UNSTRING.）
		if strings.HasPrefix(u, "UNSTRING ") {
			block := s
			j := idx + 1
			for ; j < len(lines); j++ {
				part := cleanStmtLine(lines[j])
				if part == "" {
					continue
				}
				block += " " + part
				if strings.Contains(strings.ToUpper(part), "END-UNSTRING") {
					break
				}
			}
			idx = j // 跳過用過的行

			if st := parseUnstring(block); st != nil {
				appendStmt(cur, st, srcLine)
			}
			continue
		}

		// IF 語句
		if strings.HasPrefix(u, "IF ") {
			// 解析條件；parseCond 會回傳 (條件, THEN 後剩餘字串 tail)
			cond, tail := parseCond(strings.TrimSpace(s[3:]))
			if cond == nil {
				continue
			}
			// 單行 IF ... THEN <stmt; stmt; ...> END-IF. 的情況
			utail := strings.ToUpper(tail)
			if strings.Contains(utail, "END-IF") {
				bodyText := strings.TrimSpace(strings.ReplaceAll(strings.TrimSuffix(tail, "."), "END-IF", ""))
				body := parseInlineStmtList(bodyText)
				appendStmt(cur, ast.StIf{Cond: cond, Then: body}, srcLine)
				continue
			}
			// 區塊式 IF：推入堆疊，之後直到 ELSE/END-IF 都寫入 thenB
			ifstk = append(ifstk, ifFrame{
				cond:    cond,
				parent:  cur,
				thenB:   []ast.Stmt{},
				elseB:   []ast.Stmt{},
				inElse:  false,
				srcLine: srcLine,
			})
			cur = &ifstk[len(ifstk)-1].thenB
			continue
		}

		// ELSE：切換寫入目標到當前 IF 的 elseB
		if strings.HasPrefix(u, "ELSE") {
			if len(ifstk) == 0 {
				continue
			}
			ifstk[len(ifstk)-1].inElse = true
			cur = &ifstk[len(ifstk)-1].elseB
			continue
		}

		// END-IF：彈出堆疊，把組好的 IF 語句附回上一層
		if strings.HasPrefix(u, "END-IF") {
			if len(ifstk) == 0 {
				continue
			}
			top := ifstk[len(ifstk)-1]
			ifstk = ifstk[:len(ifstk)-1]
			ifStmt := ast.StIf{Cond: top.cond, Then: top.thenB, Else: top.elseB}
			appendStmt(top.parent, ifStmt, top.srcLine)
			cur = top.parent
			continue
		}

		// ACCEPT …
		if strings.HasPrefix(u, "ACCEPT ") {
			if st := parseAccept(s); st != nil {
				appendStmt(cur, st, srcLine)
			}
			continue
		}

		// SUBTRACT …
		if strings.HasPrefix(u, "SUBTRACT ") {
			if st := parseSubtract(s); st != nil {
				appendStmt(cur, st, srcLine)
			}
			continue
		}

		// MOVE …
		if strings.HasPrefix(u, "MOVE ") {
			if st := parseMove(s); st != nil {
				appendStmt(cur, st, srcLine)
			}
			continue
		}

		// DISPLAY …
		if strings.HasPrefix(u, "DISPLAY ") {
			if st := parseDisplay(s); st != nil {
				appendStmt(cur, st, srcLine)
			}
			continue
		}

		// STOP RUN.
		if strings.HasPrefix(u, "STOP RUN") {
			appendStmt(cur, ast.StStopRun{}, srcLine)
			continue
		}

		// 其他關鍵字（OPEN/CLOSE/READ/PERFORM/CALL/GO TO…）目前忽略
	}

	// 若 IF 未正常以 END-IF 收尾（.REP 斷頁或其他格式），這裡做個保守收斂：視為沒有 else。
	for len(ifstk) > 0 {
		top := ifstk[len(ifstk)-1]
		ifstk = ifstk[:len(ifstk)-1]
		ifStmt := ast.StIf{Cond: top.cond, Then: top.thenB, Else: top.elseB}
		appendStmt(top.parent, ifStmt, top.srcLine)
	}

	return stmts, lineIndex
}

// UNSTRING <src> DELIMITED BY ALL " " INTO a, b, c [, d] [TALLYING ct] END-UNSTRING.
func parseUnstring(block string) ast.Stmt {
	// 先把尾端的 END-UNSTRING. 去掉，只保留主體
	U := strings.ToUpper(block)
	if !strings.Contains(U, "END-UNSTRING") {
		return nil
	}
	// 保留原大小寫以利 parseExpr，但用索引尋找切割點
	iDelim := strings.Index(U, " DELIMITED BY ALL ")
	iInto := strings.Index(U, " INTO ")
	if iDelim < 0 || iInto < 0 || iInto <= iDelim {
		return nil
	}
	srcText := strings.TrimSpace(block[len("UNSTRING "):iDelim])
	src := parseExpr(srcText)

	rest := block[iInto+len(" INTO "):]
	// 刪掉 END-UNSTRING 後面的東西
	if j := strings.Index(strings.ToUpper(rest), "END-UNSTRING"); j >= 0 {
		rest = rest[:j]
	}
	// 解析 TALLYING
	tally := ""
	UR := strings.ToUpper(rest)
	if k := strings.Index(UR, " TALLYING "); k >= 0 {
		tallyPart := strings.TrimSpace(rest[k+len(" TALLYING "):])
		rest = strings.TrimSpace(rest[:k])
		// 取第一個 token 當 tally 名
		tt := tokenize(tallyPart)
		if len(tt) > 0 {
			tally = strings.ToUpper(strings.Trim(tt[0], ",; "))
		}
	}
	// 目的地清單（以逗號分隔）
	var dsts []ast.Expr
	for _, tok := range strings.Split(rest, ",") {
		tok = strings.TrimSpace(strings.Trim(tok, "; "))
		if tok == "" {
			continue
		}
		dsts = append(dsts, parseExpr(tok))
	}
	if len(dsts) == 0 {
		return nil
	}
	return ast.StUnstring{Src: src, Dsts: dsts, Tally: tally}
}

// parseAccept 支援的語法（子集）：
//   ACCEPT <dst> [FROM TIME|CENTURY-DATE|ENVIRONMENT "<env>"|COMMAND-LINE].
//
// - <dst> 允許一般識別字，也允許 RefMod（視下方 parseExpr 支援）。
// - FROM 省略時，From 欄位為 nil（emitter 端會處理 UNKNOWN 或預設行為）。
func parseAccept(line string) ast.Stmt {
	u := strings.TrimSuffix(line, ".")
	tok := tokenize(u)
	if len(tok) < 2 {
		return nil
	}
	dstTok := tok[1]

	var from ast.AcceptFrom = nil
	// 尋找 FROM 之後的來源描述
	for i := 2; i < len(tok); i++ {
		if strings.EqualFold(tok[i], "FROM") {
			rest := tok[i+1:]
			if len(rest) > 0 {
				switch strings.ToUpper(rest[0]) {
				case "TIME":
					from = ast.AcceptTime{}
				case "CENTURY-DATE":
					from = ast.AcceptCenturyDate{}
				case "ENVIRONMENT":
					// ENVIRONMENT "<name>"
					if len(rest) >= 2 {
						from = ast.AcceptEnv{Name: trimQuotes(rest[1])}
					} else {
						from = ast.AcceptEnv{Name: ""}
					}
				case "COMMAND-LINE":
					from = ast.AcceptCommandLine{}
				default:
					from = nil
				}
			}
			break
		}
	}

	return ast.StAccept{
		Dst:  parseExpr(dstTok),
		From: from,
	}
}

// parseSubtract 支援語法（子集）：
//   SUBTRACT <數值或表達式> FROM <dst>.
// - 左邊 amount 允許是常數（LitNumber）或簡單識別字（parseExpr 會處理）；
// - 右邊 <dst> 是單一變數名稱（這裡轉為大寫存字串，交給 emitter 執行）。
func parseSubtract(line string) ast.Stmt {
	u := strings.TrimSuffix(line, ".")
	tok := tokenize(u)
	// SUBTRACT <num> FROM <dst>
	if len(tok) < 4 {
		return nil
	}
	// 尋找 FROM
	iFrom := -1
	for i, t := range tok {
		if strings.EqualFold(t, "FROM") {
			iFrom = i
			break
		}
	}
	if iFrom < 0 || iFrom+1 >= len(tok) {
		return nil
	}
	amount := parseExpr(strings.Join(tok[1:iFrom], " "))
	dst := tok[iFrom+1]
	return ast.StSubFrom{Amount: amount, Dst: strings.ToUpper(dst)}
}

// parseMove 支援語法（子集）：
//   MOVE <src> TO <dst>.
// - <src> 支援字串/數字/識別字，也支援 "ALL 'x'"（這裡簡化為把 "ALL " 拿掉，真正的填充交給 runtime）。
// - <dst> 支援識別字或 RefMod（e.g., FOO(1:3)）。
func parseMove(line string) ast.Stmt {
	u := strings.TrimSuffix(line, ".")
	tok := tokenize(u)
	if len(tok) < 4 {
		return nil
	}
	// 找 "TO"
	iTo := -1
	for i, t := range tok {
		if strings.EqualFold(t, "TO") {
			iTo = i
			break
		}
	}
	if iTo < 0 || iTo+1 >= len(tok) {
		return nil
	}
	srcTok := strings.Join(tok[1:iTo], " ")
	// "ALL '<x>'" → 先砍掉 "ALL "，讓 runtime 決定如何填充
	if strings.HasPrefix(strings.ToUpper(srcTok), "ALL ") {
		srcTok = strings.TrimSpace(srcTok[4:])
	}
	src := parseExpr(srcTok)
	dst := parseExpr(tok[iTo+1]) // 可能是 RefMod
	return ast.StMove{Src: src, Dst: dst}
}

// parseDisplay 支援語法（子集）：
//   DISPLAY <expr ...> [WITH/LINE/COL/COLOR/ERASE ...]（UI 參數全部裁掉）
// 目前回傳單一參數版本：把前段 <expr ...> 合併成一個 token，再 parseExpr。
func parseDisplay(line string) ast.Stmt {
	u := strings.ToUpper(line)
	// 找到 UI 參數關鍵詞，把它們後面全部切掉
	cutKeys := []string{" LINE ", " COL ", " COLOR ", " ERASE", " WITH "}
	cut := len(line)
	for _, k := range cutKeys {
		if i := strings.Index(u, k); i >= 0 && i < cut {
			cut = i
		}
	}
	part := strings.TrimSpace(line[:cut])
	tok := tokenize(part)
	if len(tok) < 2 {
		return nil
	}
	arg := strings.Join(tok[1:], " ")
	ex := parseExpr(arg)
	return ast.StDisplay{Args: []ast.Expr{ex}}
}

// ───────────────────────────────────────────────────────────────────────────────
// 條件解析：比較運算 + AND/OR（AND 優先於 OR，左結合）
//   支援：A (=|<>|NOT =|<|<=|>|>=) B、A [NOT] NUMERIC  [AND/OR ...]
//   括號會被接受（僅用於分組，這裡直接過濾掉不做優先權影響）。
//   回傳：(布林 AST, THEN 後的尾字串 tail)
// ───────────────────────────────────────────────────────────────────────────────

func parseCond(rest string) (ast.Bool, string) {
	// 先切出 THEN 後面的尾巴（單行 IF THEN ... END-IF 用）
	u := strings.ToUpper(rest)
	thenIdx := strings.Index(u, " THEN ")
	var condText, tail string
	if thenIdx >= 0 {
		condText = strings.TrimSpace(rest[:thenIdx])
		tail = strings.TrimSpace(rest[thenIdx+6:])
	} else {
		condText = strings.TrimSpace(rest)
		tail = ""
	}

	// token 化後，過濾括號
	rawTok := tokenize(condText)
	var tok []string
	for _, t := range rawTok {
		if t == "(" || t == ")" {
			continue
		}
		tok = append(tok, t)
	}
	if len(tok) == 0 {
		return nil, tail
	}

	// 解析單一比較或 NUMERIC 測試
	parseCmp := func(i int) (ast.Bool, int) {
		if i >= len(tok) {
			return nil, i
		}
		left := parseExpr(tok[i])
		if i+1 >= len(tok) {
			return nil, i
		}
		opTok := strings.ToUpper(tok[i+1])
		j := i + 2

		// <expr> [NOT] NUMERIC
		if opTok == "NUMERIC" {
			return ast.IsNumeric{Expr: left, Neg: false}, j
		}
		if opTok == "NOT" && j < len(tok) && strings.ToUpper(tok[j]) == "NUMERIC" {
			return ast.IsNumeric{Expr: left, Neg: true}, j + 1
		}

		// 一般比較
		var op ast.CmpOp
		switch opTok {
		case "=":
			op = ast.OpEQ
		case "<>":
			op = ast.OpNE
		case "<", ">", "<=", ">=":
			op = ast.CmpOp(opTok)
		case "NOT":
			if j < len(tok) && tok[j] == "=" {
				op = ast.OpNE
				j++
			} else {
				return nil, i
			}
		default:
			return nil, i
		}
		if j >= len(tok) {
			return nil, i
		}
		right := parseExpr(tok[j])
		return ast.Cmp{Left: left, Op: op, Right: right}, j + 1
	}

	// AND 優先於 OR（左結合）
	type node struct {
		val ast.Bool
		op  string // "AND" or "OR"
	}
	var nodes []node
	for i := 0; i < len(tok); {
		b, nxt := parseCmp(i)
		if b == nil {
			return nil, tail
		}
		op := ""
		if nxt < len(tok) {
			if strings.EqualFold(tok[nxt], "AND") {
				op = "AND"
				nxt++
			} else if strings.EqualFold(tok[nxt], "OR") {
				op = "OR"
				nxt++
			}
		}
		nodes = append(nodes, node{val: b, op: op})
		i = nxt
	}
	// 先把所有 AND 吃掉
	for i := 0; i < len(nodes); {
		if i+1 < len(nodes) && nodes[i].op == "AND" {
			nodes[i] = node{val: ast.BoolAnd{A: nodes[i].val, B: nodes[i+1].val}, op: nodes[i+1].op}
			nodes = append(nodes[:i+1], nodes[i+2:]...)
			continue
		}
		i++
	}
	// 再吃 OR
	for i := 0; i < len(nodes); {
		if i+1 < len(nodes) && nodes[i].op == "OR" {
			nodes[i] = node{val: ast.BoolOr{A: nodes[i].val, B: nodes[i+1].val}, op: nodes[i+1].op}
			nodes = append(nodes[:i+1], nodes[i+2:]...)
			continue
		}
		i++
	}
	if len(nodes) == 0 {
		return nil, tail
	}
	return nodes[0].val, tail
}

// parseInlineStmtList：處理「單行 IF THEN a; b; c」內部的多語句。
// 目前支援：MOVE / DISPLAY / ACCEPT / SUBTRACT（其他忽略）。
func parseInlineStmtList(text string) []ast.Stmt {
	parts := strings.Split(text, ";")
	var out []ast.Stmt
	for _, p := range parts {
		p = strings.TrimSpace(p)
		if p == "" {
			continue
		}
		u := strings.ToUpper(p)
		switch {
		case strings.HasPrefix(u, "MOVE "):
			if st := parseMove(p); st != nil {
				out = append(out, st)
			}
		case strings.HasPrefix(u, "DISPLAY "):
			if st := parseDisplay(p); st != nil {
				out = append(out, st)
			}
		case strings.HasPrefix(u, "ACCEPT "):
			if st := parseAccept(p); st != nil {
				out = append(out, st)
			}
		case strings.HasPrefix(u, "SUBTRACT "):
			if st := parseSubtract(p); st != nil {
				out = append(out, st)
			}
		}
	}
	return out
}

// ───────────────────────────────────────────────────────────────────────────────
// Expr / RefMod（Reference Modification）
// ───────────────────────────────────────────────────────────────────────────────

// reRefMod：支援 "BASE(start:length)" 的參照修飾（1-based、含冒號）。
//   e.g. "APPIN-NAME(1:40)"
var reRefMod = regexp.MustCompile(`^([A-Z0-9\-]+)\s*\(\s*([0-9]+)\s*:\s*([0-9]+)\s*\)$`)

// parseExpr：把單一 token 解析成 AST 表達式（簡化版）。
// 支援：字串（單雙引號）、純數字（整數）、RefMod、識別字（其他）。
func parseExpr(tok string) ast.Expr {
	tok = strings.TrimSpace(tok)
	if tok == "" {
		return nil
	}
	// 字串常值
	if (strings.HasPrefix(tok, `"`) && strings.HasSuffix(tok, `"`)) ||
		(strings.HasPrefix(tok, `'`) && strings.HasSuffix(tok, `'`)) {
		return ast.LitString{Val: trimQuotes(tok)}
	}
	// 純數字（保留 raw，讓 emitter 再處理前導零等問題）
	if isNumber(tok) {
		return ast.LitNumber{Raw: tok}
	}
	// RefMod？
	if m := reRefMod.FindStringSubmatch(strings.ToUpper(tok)); m != nil {
		return ast.RefMod{
			Base:  m[1],
			Start: ast.LitNumber{Raw: m[2]},
			Len:   ast.LitNumber{Raw: m[3]},
		}
	}
	// 其他當作識別字
	return ast.Ident{Name: strings.ToUpper(tok)}
}

// ───────────────────────────────────────────────────────────────────────────────
// Utils（小工具 & 行清理）
// ───────────────────────────────────────────────────────────────────────────────

// guessProgramID 嘗試從 "PROGRAM-ID. <name>." 找出名稱；找不到就回 "MAIN"。
func guessProgramID(lines []string) string {
	for _, ln := range lines {
		u := strings.ToUpper(ln)
		if strings.HasPrefix(strings.TrimSpace(u), "PROGRAM-ID.") {
			rest := strings.TrimSpace(strings.TrimPrefix(strings.TrimSpace(u), "PROGRAM-ID."))
			rest = strings.TrimSuffix(rest, ".")
			rest = strings.TrimSpace(rest)
			if rest != "" {
				fields := strings.Fields(rest)
				if len(fields) > 0 {
					return fields[0]
				}
			}
		}
	}
	return "MAIN"
}

// tokenize：超簡化 tokenizer，僅以空白切詞；但會把引號內的空白保留成同一個 token。
// 例：DISPLAY "HELLO WORLD" 會得到 ["DISPLAY", "\"HELLO WORLD\""]。
// 注意：不處理跳脫字元；遇到不規則引號（單邊/巢狀）會當成原樣輸出。
func tokenize(s string) []string {
	var out []string
	var cur strings.Builder
	inQuote := byte(0)

	appendCur := func() {
		if cur.Len() > 0 {
			out = append(out, cur.String())
			cur.Reset()
		}
	}
	for i := 0; i < len(s); i++ {
		c := s[i]
		if inQuote != 0 {
			cur.WriteByte(c)
			if c == inQuote {
				appendCur()
				inQuote = 0
			}
			continue
		}
		if c == '"' || c == '\'' {
			appendCur()
			inQuote = c
			cur.WriteByte(c)
			continue
		}
		if c == ' ' || c == '\t' {
			appendCur()
			continue
		}
		cur.WriteByte(c)
	}
	appendCur()
	return out
}

// trimQuotes：若字串是被成對引號包起來，去掉最外層引號（不處理跳脫）。
func trimQuotes(s string) string {
	if len(s) >= 2 {
		if (s[0] == '"' && s[len(s)-1] == '"') || (s[0] == '\'' && s[len(s)-1] == '\'') {
			return s[1 : len(s)-1]
		}
	}
	return s
}

// isNumber：判斷是否為「純數字」字串（不含 +/-、不含小數點；前導零允許）。
func isNumber(s string) bool {
	if s == "" {
		return false
	}
	for i := 0; i < len(s); i++ {
		if s[i] < '0' || s[i] > '9' {
			return false
		}
	}
	return true
}

// hasPrefixI：大小寫不敏感的前綴判斷（會先 TrimSpace）。
func hasPrefixI(s, pref string) bool {
	return strings.HasPrefix(strings.ToUpper(strings.TrimSpace(s)), strings.ToUpper(pref))
}

// indexOf：回傳第一個令 pred 成立的元素索引；找不到回 -1。
func indexOf(arr []string, pred func(string) bool) int {
	for i, v := range arr {
		if pred(v) {
			return i
		}
	}
	return -1
}

// 下列 regex 與輔助函式用於保守的行標準化（normalizeLines）

// rePageHdr：比對 "... Page: N"（配合 ACUCOBOL），常見於頁首；我們在 normalizeLines 再做一次防呆。
var rePageHdr = regexp.MustCompile(`\bPage:\s*\d+\b`)

// reSeq：行首 5~6 碼位址/行號 + 空白，作為 .REP 殘留的保險清理。
var reSeq = regexp.MustCompile(`^\s*[0-9A-F]{5,6}\s+`)

// reDatedComment：例如 "941219* ..." 這種「日期+星號」變更註記行，整行當註解丟棄。
var reDatedComment = regexp.MustCompile(`^\s*[0-9A-F]{5,6}\*`)

// normalizeLines：再做一次保守清理（在 loader 清的基礎上）。
// - 去掉空行 / form feed
// - 再次剔除帶 ACUCOBOL 的 Page: 標頭
// - stripSeqNo：行首 5~6 碼位址碼；也移除前導 '|'（某些歷史檔案會用 '|' 當欄位輔助線）
// - 把像 "941219* ..." 這種日期+星號的變更標記行直接捨棄
func normalizeLines(text string) []string {
	raw := strings.Split(text, "\n")
	var out []string
	for _, ln := range raw {
		ln = strings.TrimRight(ln, " \t\r")
		if ln == "" || ln == "\f" {
			continue
		}
		if rePageHdr.MatchString(ln) && strings.Contains(ln, "ACUCOBOL") {
			continue
		}
		ln = stripSeqNo(ln)
		if ln == "" {
			continue
		}
		out = append(out, ln)
	}
	return out
}

// stripSeqNo：移除行首位址欄與前導 '|'；並處理日期星號註記行。
func stripSeqNo(s string) string {
	// 先把像 "941219* ..." 的變更標記整行視為註解
	if reDatedComment.MatchString(s) {
		return ""
	}
	// 去掉 "| " 版面輔助線
	ss := strings.TrimLeft(s, " \t")
	if strings.HasPrefix(ss, "|") {
		ss = strings.TrimSpace(ss[1:])
	}
	return reSeq.ReplaceAllString(ss, "")
}

// cleanStmtLine：清理一個 Procedure 行為「可解析」的語句：
// - 去首尾空白
// - 整行是 "*" 開頭的註解：回空字串
// - 若行內有 "      *"（右側註解），砍掉其右邊
// - 把多重空白壓成單空白（方便 tokenizer）
func cleanStmtLine(s string) string {
	s = strings.TrimSpace(s)
	if s == "" {
		return ""
	}
	if strings.HasPrefix(s, "*") {
		return ""
	}
	if i := strings.Index(s, "      *"); i >= 0 {
		s = strings.TrimSpace(s[:i])
	}
	s = strings.TrimSpace(regexp.MustCompile(`\s+`).ReplaceAllString(s, " "))
	return s
}

// looksLikeLabel：判斷此行是否像是「標籤/段落/SECTION 標頭」，若是就不當語句處理。
// 規則：
//   - 以 " SECTION." 結尾 → 是
//   - 或整行（去掉句點）沒有空白且不是已識別的關鍵字前綴（IF/MOVE/...），視為標籤（像 0000-EXIT.）
func looksLikeLabel(u string) bool {
	// e.g. "MAIN-RTN SECTION."  or "0000-EXIT." / "DCL-TDY-ERROR-EXIT."
	if strings.HasSuffix(u, " SECTION.") {
		return true
	}
	uu := strings.TrimSuffix(u, ".")
	// 標籤通常不含空白；含空白就不當標籤處理
	if strings.IndexFunc(uu, func(r rune) bool {
		return r == ' '
	}) != -1 {
		return false
	}
	keyverbs := []string{"IF ", "MOVE ", "DISPLAY ", "ACCEPT ", "SUBTRACT ", "STOP RUN", "ELSE", "END-IF", "UNSTRING "}
	for _, kv := range keyverbs {
		if strings.HasPrefix(u, kv) {
			return false
		}
	}
	return true
}
