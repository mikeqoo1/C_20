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
// 支援：X(n) / S?9(d)[V9(s)] [COMP-3]
// 回傳 (spec, ok)。不支援的 PIC 會回 false（呼叫端通常略過該欄位）。
func parsePicSpec(picText string) (ast.PicSpec, bool) {
	txt := strings.TrimSpace(strings.ToUpper(picText))

	// 字母型：X(n)
	if m := regexp.MustCompile(`^[X]\((\d+)\)$`).FindStringSubmatch(txt); m != nil {
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
			Digits: d + scale,                  // 總位數（含小數位）
			Scale:  scale,                      // 小數位數
			Signed: strings.HasPrefix(txt, "S9"), // 是否帶號
			Comp3:  reHasComp3.MatchString(txt), // 是否出現 COMP-3 標記
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
// 支援語句：IF/ELSE/END-IF（含單行 THEN ... END-IF）、MOVE、DISPLAY、ACCEPT、SUBTRACT、STOP RUN。
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
		cond    ast.Bool   // IF 條件
		parent  *[]ast.Stmt // 上一層容器（頂層或外層 IF 的 then/else）
		thenB   []ast.Stmt // THEN 區塊
		elseB   []ast.Stmt // ELSE 區塊
		inElse  bool       // 目前是否在 else 區
		srcLine int        // IF 起始行號（給 lineIndex）
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
		if hasPrefixI
