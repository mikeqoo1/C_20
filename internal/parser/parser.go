package parser

import (
	"fmt"
	"regexp"
	"strconv"
	"strings"

	"c20/internal/ast"
	"c20/internal/loader"
)

/*──────────────────────────────────────────────────────────────────────────────*/
/* Top-level                                                                   */
/*──────────────────────────────────────────────────────────────────────────────*/

func ParseUnits(sources []loader.Source) []ast.Program {
	var out []ast.Program
	for _, src := range sources {
		p := parseOne(src)
		out = append(out, p)
	}
	return out
}

func parseOne(src loader.Source) ast.Program {
	rawLines := strings.Split(src.Text, "\n")
	fmt.Printf("[DBG] RAW lines: %d for %s\n", len(rawLines), src.Name)
	for i := 0; i < len(rawLines) && i < 3; i++ {
		fmt.Printf("[DBG] RAW L%d   %s\n", i, previewLine(rawLines[i]))
	}

	lines := normalizeLines(src.Text)
	fmt.Printf("[DBG] normalizeLines -> %d lines for %s\n", len(lines), src.Name)
	for i := 0; i < len(lines) && i < 3; i++ {
		fmt.Printf("[DBG] NORM L%d   %s\n", i, previewLine(lines[i]))
	}

	procIdx := indexOf(lines, func(s string) bool {
		return hasPrefixI(s, "PROCEDURE") && strings.Contains(strings.ToUpper(s), "DIVISION")
	})
	fmt.Printf("[DBG] PROCEDURE DIVISION at index: %d\n", procIdx)

	dataLines := lines
	procLines := []string{}
	if procIdx >= 0 {
		dataLines = lines[:procIdx]
		procLines = lines[procIdx:]
	}

	data := parseDataDivision(dataLines)
	fmt.Printf("[DBG] DataItems collected (before ensure): %d\n", len(data))

	// 特別處理 PF-SYSTEM 缺漏：補 START/PROG/CODE/KEYNO/STUS 與 group PARM-PF
	data = ensurePfSystemDefaults(data)

	fmt.Printf("[DBG] DataItems collected (final): %d\n", len(data))

	stmts, lineIndex := parseProcedureDivision(procLines)
	fmt.Printf("[DBG] Proc stmts collected: %d\n", len(stmts))

	return ast.Program{
		ID:        guessProgramID(lines),
		Source:    src.Name,
		Data:      data,
		Stmts:     stmts,
		LineIndex: lineIndex,
	}
}

/*──────────────────────────────────────────────────────────────────────────────*/
/* DATA DIVISION                                                               */
/*──────────────────────────────────────────────────────────────────────────────*/

var (
	reLevelPic = regexp.MustCompile(
		`^\s*(\d{2})\s+([A-Z0-9\-]+)\s+(?:REDEFINES\s+[A-Z0-9\-]+\s+)?PIC\s+(.+?)\.\s*(?:\*.*)?$`,
	)
	reLevelOnly    = regexp.MustCompile(`^\s*(\d{2})\s+([A-Z0-9\-]+)\s*\.\s*$`)
	rePicNumParts  = regexp.MustCompile(`(?i)S?9(?:\((\d+)\))?(?:V9(?:\((\d+)\))?)?(?:\s+COMP-3)?`)
	reHasComp3     = regexp.MustCompile(`(?i)\bCOMP-3\b`)
	reAlphaPic     = regexp.MustCompile(`^[X]\((\d+)\)(?:\s+COMP-X)?$`)
	reMultiSpace   = regexp.MustCompile(`\s+`)
	reStripPicTail = regexp.MustCompile(`\s+(VALUE|OCCURS|USAGE|SIGN|JUSTIFIED|BLANK|INDEXED|GLOBAL)\b.*$`)
)

func normalizePicTail(picText string) string {
	t := strings.ToUpper(strings.TrimSpace(picText))
	t = strings.TrimSuffix(t, ".")
	t = reMultiSpace.ReplaceAllString(t, " ")
	t = reStripPicTail.ReplaceAllString(t, "")
	return strings.TrimSpace(t)
}

func parsePicSpec(picText string) (ast.PicSpec, bool) {
	txt := normalizePicTail(picText)

	if m := reAlphaPic.FindStringSubmatch(txt); m != nil {
		n, _ := strconv.Atoi(m[1])
		return ast.PicSpec{Kind: ast.PicAlpha, Len: n}, true
	}
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
			Digits: d + scale,
			Scale:  scale,
			Signed: strings.HasPrefix(txt, "S9"),
			Comp3:  reHasComp3.MatchString(txt),
		}, true
	}
	return ast.PicSpec{}, false
}

func parseDataDivision(lines []string) []ast.DataItem {
	var out []ast.DataItem
	for _, ln := range lines {
		if i := strings.Index(ln, "      *"); i >= 0 {
			ln = strings.TrimSpace(ln[:i])
		}
		u := strings.ToUpper(strings.TrimSpace(ln))
		if u == "" || strings.HasPrefix(u, "*") {
			continue
		}
		if strings.Contains(u, "DIVISION.") || strings.HasPrefix(u, "FD ") || strings.HasPrefix(u, "FILE ") || strings.HasPrefix(u, "SELECT ") || strings.HasPrefix(u, "COPY ") {
			continue
		}
		if strings.HasPrefix(u, "88 ") {
			continue
		}

		if m := reLevelPic.FindStringSubmatch(u); m != nil {
			lv, _ := strconv.Atoi(m[1])
			name := m[2]
			rawPic := strings.TrimSpace(m[3])
			picText := rawPic
			var initVal *ast.Literal
			upper := strings.ToUpper(rawPic)
			if idx := strings.Index(upper, " VALUE "); idx >= 0 {
				picText = strings.TrimSpace(rawPic[:idx])
				valPart := strings.TrimSpace(rawPic[idx+len(" VALUE "):])
				if strings.HasPrefix(strings.ToUpper(valPart), "IS ") {
					valPart = strings.TrimSpace(valPart[3:])
				}
				initVal = parseValueLiteral(valPart)
			}
			pic, ok := parsePicSpec(picText)
			if !ok {
				continue
			}
			out = append(out, ast.DataItem{
				Level: lv,
				Name:  name,
				Pic:   pic,
				Value: initVal,
			})
			continue
		}
		// group header（沒有 PIC）先略過；之後用 ensurePfSystemDefaults 針對 PARM-PF 補齊
	}
	return out
}

/*──────────────────────────────────────────────────────────────────────────────*/
/* PF-SYSTEM 專用補齊                                                           */
/*──────────────────────────────────────────────────────────────────────────────*/

func picByteLen(p ast.PicSpec) int {
	switch p.Kind {
	case ast.PicAlpha:
		return p.Len
	case ast.PicNumeric:
		// 這裡我們只處理 DISPLAY/9(n) 的 case；COMP-3 的實際 bytes 另有規則，但 PF 系列不會用到
		return p.Digits
	default:
		return 0
	}
}

func findItem(items []ast.DataItem, name string) (ast.DataItem, bool) {
	up := strings.ToUpper(name)
	for _, it := range items {
		if strings.ToUpper(it.Name) == up {
			return it, true
		}
	}
	return ast.DataItem{}, false
}

func hasItem(items []ast.DataItem, name string) bool {
	_, ok := findItem(items, name)
	return ok
}

func ensurePfSystemDefaults(items []ast.DataItem) []ast.DataItem {
	add := func(name string, pic ast.PicSpec) {
		fmt.Printf("[DBG] ensurePfSystemDefaults: injected %-8s (", name)
		if pic.Kind == ast.PicAlpha {
			fmt.Printf("PIC X(%d)", pic.Len)
		} else {
			fmt.Printf("PIC 9(%d)", pic.Digits)
		}
		fmt.Println(")")
		items = append(items, ast.DataItem{Level: 5, Name: name, Pic: pic})
	}

	// 先補 5 個基礎欄位（避免後面算 PARM-PF 時缺料）
	if !hasItem(items, "START-PF") {
		add("START-PF", ast.PicSpec{Kind: ast.PicAlpha, Len: 30})
	}
	if !hasItem(items, "PROG-PF") {
		add("PROG-PF", ast.PicSpec{Kind: ast.PicAlpha, Len: 20})
	}
	if !hasItem(items, "CODE-PF") {
		add("CODE-PF", ast.PicSpec{Kind: ast.PicAlpha, Len: 2})
	}
	if !hasItem(items, "KEYNO-PF") {
		add("KEYNO-PF", ast.PicSpec{Kind: ast.PicNumeric, Digits: 6})
	}
	if !hasItem(items, "STUS-PF") {
		add("STUS-PF", ast.PicSpec{Kind: ast.PicAlpha, Len: 2})
	}

	// 再補 group：PARM-PF（沒有 PIC，但程式會 MOVE PARM-PF, SPACES）
	if !hasItem(items, "PARM-PF") {
		// 長度 = CODE-PF(2) + FILLER(1) + KEYNO-PF(6) + FILLER(1) + STUS-PF(2) = 12
		code, _ := findItem(items, "CODE-PF")
		keyn, _ := findItem(items, "KEYNO-PF")
		stus, _ := findItem(items, "STUS-PF")
		size := picByteLen(code.Pic) + 1 + picByteLen(keyn.Pic) + 1 + picByteLen(stus.Pic)
		if size > 0 {
			add("PARM-PF", ast.PicSpec{Kind: ast.PicAlpha, Len: size})
		}
	}

	return items
}

/*──────────────────────────────────────────────────────────────────────────────*/
/* PROCEDURE DIVISION（與你先前相同，略有清理）                                 */
/*──────────────────────────────────────────────────────────────────────────────*/

func parseProcedureDivision(lines []string) ([]ast.Stmt, map[int]int) {
	var stmts []ast.Stmt
	lineIndex := make(map[int]int)
	if len(lines) == 0 {
		return stmts, lineIndex
	}

	i := 0
	if hasPrefixI(lines[0], "PROCEDURE") {
		i++
	}

	appendStmt := func(dst *[]ast.Stmt, s ast.Stmt, srcLine int) {
		*dst = append(*dst, s)
		lineIndex[len(lineIndex)] = srcLine
	}

	type ifFrame struct {
		cond         ast.Bool
		parent       *[]ast.Stmt
		thenB, elseB []ast.Stmt
		srcLine      int
	}
	var cur *[]ast.Stmt = &stmts
	var ifstk []ifFrame
	inDecl := false

	for i < len(lines) {
		raw := lines[i]
		srcLine := i
		s := cleanStmtLine(raw)
		if s == "" {
			i++
			continue
		}
		u := strings.ToUpper(s)

		if hasPrefixI(u, "DECLARATIVES") {
			inDecl = true
			i++
			continue
		}
		if inDecl {
			if hasPrefixI(u, "END DECLARATIVES") {
				inDecl = false
			}
			i++
			continue
		}
		if looksLikeLabel(u) {
			i++
			continue
		}

		if strings.HasPrefix(u, "UNSTRING ") {
			block := s
			j := i + 1
			for ; j < len(lines); j++ {
				part := cleanStmtLine(lines[j])
				if part == "" {
					continue
				}
				block += " " + part
				if strings.Contains(strings.ToUpper(part), "END-UNSTRING") {
					j++
					break
				}
			}
			i = j
			if st := parseUnstring(block); st != nil {
				appendStmt(cur, st, srcLine)
			}
			continue
		}

		if strings.HasPrefix(u, "IF ") {
			cond, tail := parseCond(strings.TrimSpace(s[3:]))
			if cond == nil {
				i++
				continue
			}
			utail := strings.ToUpper(tail)
			if strings.Contains(utail, "END-IF") {
				bodyText := strings.TrimSpace(strings.ReplaceAll(strings.TrimSuffix(tail, "."), "END-IF", ""))
				body := parseInlineStmtList(bodyText)
				appendStmt(cur, ast.StIf{Cond: cond, Then: body}, srcLine)
				i++
				continue
			}
			ifstk = append(ifstk, ifFrame{cond: cond, parent: cur, srcLine: srcLine})
			cur = &ifstk[len(ifstk)-1].thenB
			i++
			continue
		}
		if strings.HasPrefix(u, "ELSE") {
			if len(ifstk) > 0 {
				cur = &ifstk[len(ifstk)-1].elseB
			}
			i++
			continue
		}
		if strings.HasPrefix(u, "END-IF") {
			if len(ifstk) > 0 {
				top := ifstk[len(ifstk)-1]
				ifstk = ifstk[:len(ifstk)-1]
				appendStmt(top.parent, ast.StIf{Cond: top.cond, Then: top.thenB, Else: top.elseB}, top.srcLine)
				cur = top.parent
			}
			i++
			continue
		}

		switch {
		case strings.HasPrefix(u, "OPEN "):
			if st := parseOpen(u); st != nil {
				appendStmt(cur, st, srcLine)
			}
			i++
		case strings.HasPrefix(u, "CLOSE "):
			if st := parseClose(u); st != nil {
				appendStmt(cur, st, srcLine)
			}
			i++
		case strings.HasPrefix(u, "READ "):
			if st := parseRead(lines, &i); st != nil {
				appendStmt(cur, st, srcLine)
			}
		case strings.HasPrefix(u, "WRITE "):
			if st := parseWrite(u); st != nil {
				appendStmt(cur, st, srcLine)
			}
			i++
		case strings.HasPrefix(u, "REWRITE "):
			if st := parseRewrite(lines, &i); st != nil {
				appendStmt(cur, st, srcLine)
			}
		case strings.HasPrefix(u, "ACCEPT "):
			if st := parseAccept(s); st != nil {
				appendStmt(cur, st, srcLine)
			}
			i++
		case strings.HasPrefix(u, "SUBTRACT "):
			if st := parseSubtract(s); st != nil {
				appendStmt(cur, st, srcLine)
			}
			i++
		case strings.HasPrefix(u, "ADD "):
			if st := parseAdd(s); st != nil {
				appendStmt(cur, st, srcLine)
			}
			i++
		case strings.HasPrefix(u, "DIVIDE "):
			if st := parseDivide(s); st != nil {
				appendStmt(cur, st, srcLine)
			}
			i++
		case strings.HasPrefix(u, "MULTIPLY "):
			if st := parseMultiply(s); st != nil {
				appendStmt(cur, st, srcLine)
			}
			i++
		case strings.HasPrefix(u, "COMPUTE "):
			if st := parseCompute(s); st != nil {
				appendStmt(cur, st, srcLine)
			}
			i++
		case strings.HasPrefix(u, "INITIALIZE "):
			if st := parseInitialize(s); st != nil {
				appendStmt(cur, st, srcLine)
			}
			i++
		case strings.HasPrefix(u, "MOVE "):
			if st := parseMove(s); st != nil {
				appendStmt(cur, st, srcLine)
			}
			i++
		case strings.HasPrefix(u, "DISPLAY "):
			if st := parseDisplay(s); st != nil {
				appendStmt(cur, st, srcLine)
			}
			i++
		case strings.HasPrefix(u, "STOP RUN"):
			appendStmt(cur, ast.StStopRun{}, srcLine)
			i++
		default:
			i++
		}
	}

	for len(ifstk) > 0 {
		top := ifstk[len(ifstk)-1]
		ifstk = ifstk[:len(ifstk)-1]
		appendStmt(top.parent, ast.StIf{Cond: top.cond, Then: top.thenB, Else: top.elseB}, top.srcLine)
	}
	return stmts, lineIndex
}

/*──────────────────────────────────────────────────────────────────────────────*/
/* UNSTRING / ACCEPT / SUBTRACT / MOVE / DISPLAY / READ…（沿用你原本邏輯）       */
/*──────────────────────────────────────────────────────────────────────────────*/

func parseUnstring(block string) ast.Stmt {
	U := strings.ToUpper(block)
	if !strings.Contains(U, "END-UNSTRING") {
		return nil
	}
	iDelim := strings.Index(U, " DELIMITED BY ALL ")
	iInto := strings.Index(U, " INTO ")
	if iDelim < 0 || iInto < 0 || iInto <= iDelim {
		return nil
	}
	srcText := strings.TrimSpace(block[len("UNSTRING "):iDelim])
	src := parseExpr(srcText)

	rest := block[iInto+len(" INTO "):]
	if j := strings.Index(strings.ToUpper(rest), "END-UNSTRING"); j >= 0 {
		rest = rest[:j]
	}
	tally := ""
	UR := strings.ToUpper(rest)
	if k := strings.Index(UR, " TALLYING "); k >= 0 {
		tallyPart := strings.TrimSpace(rest[k+len(" TALLYING "):])
		rest = strings.TrimSpace(rest[:k])
		tt := tokenize(tallyPart)
		if len(tt) > 0 {
			tally = strings.ToUpper(strings.Trim(tt[0], ",; "))
		}
	}
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

func parseAccept(line string) ast.Stmt {
	u := strings.TrimSuffix(line, ".")
	tok := tokenize(u)
	if len(tok) < 2 {
		return nil
	}
	dstTok := tok[1]
	var from ast.AcceptFrom = nil
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
					if len(rest) >= 2 {
						from = ast.AcceptEnv{Name: trimQuotes(rest[1])}
					} else {
						from = ast.AcceptEnv{Name: ""}
					}
				case "COMMAND-LINE":
					from = ast.AcceptCommandLine{}
				}
			}
			break
		}
	}
	return ast.StAccept{Dst: parseExpr(dstTok), From: from}
}

func parseSubtract(line string) ast.Stmt {
	u := strings.TrimSuffix(line, ".")
	tok := tokenize(u)
	if len(tok) < 4 {
		return nil
	}
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
	iGiving := -1
	for i := iFrom + 1; i < len(tok); i++ {
		if strings.EqualFold(tok[i], "GIVING") {
			iGiving = i
			break
		}
	}
	amount := parseExpr(strings.Join(tok[1:iFrom], " "))
	dst := tok[iFrom+1]
	if iGiving > 0 && iGiving+1 < len(tok) {
		giving := parseExpr(tok[iGiving+1])
		if amount != nil && giving != nil {
			fromExpr := parseExpr(dst)
			return ast.StBinaryMath{Op: "-", Left: fromExpr, Right: amount, Dst: giving}
		}
	}
	return ast.StSubFrom{Amount: amount, Dst: strings.ToUpper(dst)}
}

func parseAdd(line string) ast.Stmt {
	u := strings.TrimSuffix(line, ".")
	tok := tokenize(u)
	if len(tok) < 4 {
		return nil
	}
	iGiving := -1
	for i, t := range tok {
		if strings.EqualFold(t, "GIVING") {
			iGiving = i
			break
		}
	}
	if iGiving < 0 || iGiving+1 >= len(tok) {
		return nil
	}
	iTo := -1
	for i := 1; i < iGiving; i++ {
		if strings.EqualFold(tok[i], "TO") {
			iTo = i
			break
		}
	}
	if iTo < 0 || iTo+1 >= iGiving {
		return nil
	}
	addTok := strings.TrimSpace(strings.Join(tok[1:iTo], " "))
	if addTok == "" {
		return nil
	}
	addExpr := parseExpr(addTok)
	toExpr := parseExpr(tok[iTo+1])
	giving := parseExpr(tok[iGiving+1])
	if addExpr == nil || toExpr == nil || giving == nil {
		return nil
	}
	return ast.StBinaryMath{Op: "+", Left: toExpr, Right: addExpr, Dst: giving}
}

func parseDivide(line string) ast.Stmt {
	u := strings.TrimSuffix(line, ".")
	tok := tokenize(u)
	if len(tok) < 5 {
		return nil
	}
	iBy := -1
	for i := 1; i < len(tok); i++ {
		if strings.EqualFold(tok[i], "BY") {
			iBy = i
			break
		}
	}
	if iBy < 0 || iBy+1 >= len(tok) {
		return nil
	}
	iGiving := -1
	for i := iBy + 1; i < len(tok); i++ {
		if strings.EqualFold(tok[i], "GIVING") {
			iGiving = i
			break
		}
	}
	if iGiving < 0 || iGiving+1 >= len(tok) {
		return nil
	}
	left := parseExpr(strings.TrimSpace(strings.Join(tok[1:iBy], " ")))
	right := parseExpr(tok[iBy+1])
	giving := parseExpr(tok[iGiving+1])
	if left == nil || right == nil || giving == nil {
		return nil
	}
	return ast.StBinaryMath{Op: "/", Left: left, Right: right, Dst: giving}
}

func parseMultiply(line string) ast.Stmt {
	u := strings.TrimSuffix(line, ".")
	tok := tokenize(u)
	if len(tok) < 5 {
		return nil
	}
	iBy := -1
	for i := 1; i < len(tok); i++ {
		if strings.EqualFold(tok[i], "BY") {
			iBy = i
			break
		}
	}
	if iBy < 0 || iBy+1 >= len(tok) {
		return nil
	}
	iGiving := -1
	for i := iBy + 1; i < len(tok); i++ {
		if strings.EqualFold(tok[i], "GIVING") {
			iGiving = i
			break
		}
	}
	if iGiving < 0 || iGiving+1 >= len(tok) {
		return nil
	}
	left := parseExpr(strings.TrimSpace(strings.Join(tok[1:iBy], " ")))
	right := parseExpr(tok[iBy+1])
	giving := parseExpr(tok[iGiving+1])
	if left == nil || right == nil || giving == nil {
		return nil
	}
	return ast.StBinaryMath{Op: "*", Left: left, Right: right, Dst: giving}
}

func parseCompute(line string) ast.Stmt {
	text := strings.TrimSpace(strings.TrimSuffix(line, "."))
	tok := tokenize(text)
	if len(tok) < 4 {
		return nil
	}
	dst := parseExpr(tok[1])
	if dst == nil {
		return nil
	}
	iEq := -1
	for i := 2; i < len(tok); i++ {
		if tok[i] == "=" {
			iEq = i
			break
		}
	}
	if iEq < 0 || iEq+1 >= len(tok) {
		return nil
	}
	exprTok := append([]string{}, tok[iEq+1:]...)
	if len(exprTok) < 3 {
		return nil
	}
	exprTok[0] = strings.TrimLeft(exprTok[0], "(")
	lastIdx := len(exprTok) - 1
	exprTok[lastIdx] = strings.TrimRight(exprTok[lastIdx], ")")
	if len(exprTok) != 3 {
		return nil
	}
	left := parseExpr(exprTok[0])
	if left == nil {
		return nil
	}
	op := exprTok[1]
	if op != "+" && op != "-" && op != "*" && op != "/" {
		return nil
	}
	right := parseExpr(exprTok[2])
	if right == nil {
		return nil
	}
	return ast.StBinaryMath{Op: op, Left: left, Right: right, Dst: dst}
}

func parseInitialize(line string) ast.Stmt {
	u := strings.TrimSuffix(line, ".")
	tok := tokenize(u)
	if len(tok) < 2 {
		return nil
	}
	var targets []ast.Expr
	i := 1
	for ; i < len(tok); i++ {
		if strings.EqualFold(tok[i], "REPLACING") {
			break
		}
		ex := parseExpr(tok[i])
		if ex != nil {
			targets = append(targets, ex)
		}
	}
	if len(targets) == 0 {
		return nil
	}
	var replace ast.Expr
	if i < len(tok) && strings.EqualFold(tok[i], "REPLACING") {
		i++
		if i < len(tok) && strings.EqualFold(tok[i], "NUMERIC") {
			i++
		}
		if i < len(tok) && strings.EqualFold(tok[i], "DATA") {
			i++
		}
		if i < len(tok) && strings.EqualFold(tok[i], "BY") {
			i++
		}
		if i < len(tok) {
			replace = parseExpr(tok[i])
		}
	}
	return ast.StInitialize{Targets: targets, ReplaceNumeric: replace}
}

func parseMove(line string) ast.Stmt {
	u := strings.TrimSuffix(line, ".")
	tok := tokenize(u)
	if len(tok) < 4 {
		return nil
	}
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
	if strings.HasPrefix(strings.ToUpper(srcTok), "ALL ") {
		srcTok = strings.TrimSpace(srcTok[4:])
	}
	src := parseExpr(srcTok)
	dst := parseExpr(tok[iTo+1])
	return ast.StMove{Src: src, Dst: dst}
}

func parseDisplay(line string) ast.Stmt {
	u := strings.ToUpper(line)
	cutKeys := []string{" LINE ", " COL ", " COLOR ", " ERASE", " WITH "}
	cut := len(line)
	for _, k := range cutKeys {
		if i := strings.Index(u, k); i >= 0 && i < cut {
			cut = i
		}
	}
	part := strings.TrimSpace(line[:cut])
	part = strings.TrimSpace(strings.TrimSuffix(part, "."))
	tok := tokenize(part)
	if len(tok) < 2 {
		return nil
	}
	var args []ast.Expr
	for _, t := range tok[1:] {
		t = strings.TrimSpace(t)
		if t == "" {
			continue
		}
		if ex := parseExpr(t); ex != nil {
			args = append(args, ex)
		}
	}
	if len(args) == 0 {
		return nil
	}
	return ast.StDisplay{Args: args}
}

// ───────────────────────────────────────────────────────────────────────────────
// 條件解析：比較運算 + AND/OR（AND 優先於 OR，左結合）
//
//	支援：A (=|<>|NOT =|<|<=|>|>=) B、A [NOT] NUMERIC  [AND/OR ...]
//	回傳：(布林 AST, THEN 後的尾字串 tail)
//
// ───────────────────────────────────────────────────────────────────────────────
func parseCond(rest string) (ast.Bool, string) {
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

	parseCmp := func(i int) (ast.Bool, int) {
		if i >= len(tok) {
			return nil, i
		}
		// 單一旗標
		if strings.EqualFold(tok[i], "RECORD-LOCK") {
			return ast.IsStatus{Flag: ast.FlagRecordLock}, i + 1
		}
		if strings.EqualFold(tok[i], "NOT-FOUND") {
			return ast.IsStatus{Flag: ast.FlagNotFound}, i + 1
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
	// AND 優先
	for i := 0; i < len(nodes); {
		if i+1 < len(nodes) && nodes[i].op == "AND" {
			nodes[i] = node{val: ast.BoolAnd{A: nodes[i].val, B: nodes[i+1].val}, op: nodes[i+1].op}
			nodes = append(nodes[:i+1], nodes[i+2:]...)
			continue
		}
		i++
	}
	// 再 OR
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

// 單行 IF THEN 裡面的語句分解：支援 MOVE / DISPLAY / ACCEPT / SUBTRACT
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

// WRITE <record>.
func parseWrite(line string) ast.Stmt {
	u := strings.TrimSuffix(strings.TrimSpace(line), ".")
	t := tokenize(u)
	if len(t) < 2 || !strings.EqualFold(t[0], "WRITE") {
		return nil
	}
	return ast.StWrite{Record: t[1]}
}

func parseOpen(line string) ast.Stmt {
	u := strings.TrimSuffix(strings.TrimSpace(line), ".")
	t := tokenize(u)
	if len(t) < 3 || !strings.EqualFold(t[0], "OPEN") {
		return nil
	}
	mode := strings.ToUpper(t[1])
	var m ast.OpenMode
	switch mode {
	case "INPUT":
		m = ast.OpenInput
	case "OUTPUT":
		m = ast.OpenOutput
	case "I-O", "I-O,":
		m = ast.OpenIO
	case "EXTEND":
		m = ast.OpenExtend
	default:
		return nil
	}
	return ast.StOpen{File: t[2], Mode: m}
}

func parseClose(line string) ast.Stmt {
	u := strings.TrimSuffix(strings.TrimSpace(line), ".")
	t := tokenize(u)
	if len(t) < 2 || !strings.EqualFold(t[0], "CLOSE") {
		return nil
	}
	return ast.StClose{File: t[1]}
}

func parseRead(lines []string, i *int) ast.Stmt {
	start := *i
	if start >= len(lines) {
		return nil
	}
	line := strings.TrimSpace(lines[start])
	tok := tokenize(line)
	if len(tok) < 2 || !strings.EqualFold(tok[0], "READ") {
		return nil
	}

	file := tok[1]
	lock := ast.ReadDefault
	U := strings.ToUpper(line)
	if strings.Contains(U, "WITH NO LOCK") {
		lock = ast.ReadNoLock
	} else if strings.Contains(U, "WITH LOCK") || strings.Contains(U, " WITH ") {
		lock = ast.ReadWithLock
	}

	if strings.Contains(U, "INVALID KEY") {
		var body []ast.Stmt
		j := start + 1
		for j < len(lines) {
			u2 := strings.ToUpper(strings.TrimSpace(lines[j]))
			if strings.HasPrefix(u2, "END-READ") {
				j++
				break
			}
			if s := parseLineAsStmt(lines[j]); s != nil {
				body = append(body, s)
			}
			j++
		}
		*i = j
		return ast.StRead{File: file, LockMode: lock, OnInvalid: body}
	}
	*i = start + 1
	return ast.StRead{File: file, LockMode: lock}
}

func parseRewrite(lines []string, i *int) ast.Stmt {
	start := *i
	if start >= len(lines) {
		return nil
	}
	line := strings.TrimSpace(lines[start])
	tok := tokenize(line)
	if len(tok) < 2 || !strings.EqualFold(tok[0], "REWRITE") {
		return nil
	}
	rec := tok[1]
	U := strings.ToUpper(line)
	if !strings.Contains(U, "INVALID KEY") {
		*i = start + 1
		return ast.StRewrite{Record: rec}
	}
	after := line[strings.Index(U, "INVALID KEY")+len("INVALID KEY"):]
	var body []ast.Stmt
	if s := parseLineAsStmt(after); s != nil {
		body = append(body, s)
	}
	*i = start + 1
	return ast.StRewrite{Record: rec, OnInvalid: body}
}

func parseLineAsStmt(line string) ast.Stmt {
	u := strings.ToUpper(strings.TrimSpace(line))
	switch {
	case strings.HasPrefix(u, "MOVE "):
		return parseMove(line)
	case strings.HasPrefix(u, "DISPLAY "):
		return parseDisplay(line)
	case strings.HasPrefix(u, "ACCEPT "):
		return parseAccept(line)
	case strings.HasPrefix(u, "SUBTRACT "):
		return parseSubtract(line)
	default:
		return nil
	}
}

/*──────────────────────────────────────────────────────────────────────────────*/
/* Expr / Utils                                                                 */
/*──────────────────────────────────────────────────────────────────────────────*/

var reRefMod = regexp.MustCompile(`^([A-Z0-9\-]+)\s*\(\s*([0-9]+)\s*:\s*([0-9]+)\s*\)$`)

func parseExpr(tok string) ast.Expr {
	tok = strings.TrimSpace(tok)
	if tok == "" {
		return nil
	}
	if (strings.HasPrefix(tok, `"`) && strings.HasSuffix(tok, `"`)) ||
		(strings.HasPrefix(tok, `'`) && strings.HasSuffix(tok, `'`)) {
		return ast.LitString{Val: trimQuotes(tok)}
	}
	if isNumber(tok) {
		return ast.LitNumber{Raw: tok}
	}
	if m := reRefMod.FindStringSubmatch(strings.ToUpper(tok)); m != nil {
		return ast.RefMod{
			Base:  m[1],
			Start: ast.LitNumber{Raw: m[2]},
			Len:   ast.LitNumber{Raw: m[3]},
		}
	}
	return ast.Ident{Name: strings.ToUpper(tok)}
}

func parseValueLiteral(raw string) *ast.Literal {
	r := strings.TrimSpace(raw)
	if r == "" {
		return nil
	}
	// 去掉尾端註解或逗號
	r = strings.TrimSuffix(r, ",")
	if (strings.HasPrefix(r, `"`) && strings.HasSuffix(r, `"`)) ||
		(strings.HasPrefix(r, `'`) && strings.HasSuffix(r, `'`)) {
		return &ast.Literal{Kind: "string", Str: trimQuotes(r)}
	}
	up := strings.ToUpper(r)
	switch up {
	case "SPACE", "SPACES":
		return &ast.Literal{Kind: "string", Str: " "}
	case "ZERO", "ZEROS", "ZEROES":
		return &ast.Literal{Kind: "number", Str: "0"}
	}
	if strings.HasPrefix(up, "ALL ") {
		rest := strings.TrimSpace(r[4:])
		if lit := parseValueLiteral(rest); lit != nil {
			return lit
		}
	}
	// 處理正負號整數
	trimmed := strings.TrimPrefix(strings.TrimPrefix(r, "+"), "-")
	if isNumber(trimmed) {
		return &ast.Literal{Kind: "number", Str: r}
	}
	if _, err := strconv.Atoi(r); err == nil {
		return &ast.Literal{Kind: "number", Str: r}
	}
	return &ast.Literal{Kind: "string", Str: r}
}

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

func trimQuotes(s string) string {
	if len(s) >= 2 {
		if (s[0] == '"' && s[len(s)-1] == '"') || (s[0] == '\'' && s[len(s)-1] == '\'') {
			return s[1 : len(s)-1]
		}
	}
	return s
}

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

func hasPrefixI(s, pref string) bool {
	return strings.HasPrefix(strings.ToUpper(strings.TrimSpace(s)), strings.ToUpper(pref))
}

func indexOf(arr []string, pred func(string) bool) int {
	for i, v := range arr {
		if pred(v) {
			return i
		}
	}
	return -1
}

/*──────────────────────────────────────────────────────────────────────────────*/
/* normalizeLines                                                               */
/*──────────────────────────────────────────────────────────────────────────────*/

var rePageHdr = regexp.MustCompile(`\bPage:\s*\d+\b`)
var reSeq = regexp.MustCompile(`^\s*[0-9A-F]{5,6}\s+`)
var reDatedComment = regexp.MustCompile(`^\s*[0-9A-F]{5,6}\*`)

func normalizeLines(text string) []string {
	raw := strings.Split(text, "\n")
	var out []string
	for _, ln := range raw {
		ln = strings.TrimRight(ln, " \t\r")
		if ln == "" || ln == "\f" {
			fmt.Printf("[DBG] NORM drop L?  : blank\n")
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

func stripSeqNo(s string) string {
	if reDatedComment.MatchString(s) {
		return ""
	}
	ss := strings.TrimLeft(s, " \t")
	if strings.HasPrefix(ss, "|") {
		ss = strings.TrimSpace(ss[1:])
	}
	return reSeq.ReplaceAllString(ss, "")
}

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
	s = strings.TrimSpace(reMultiSpace.ReplaceAllString(s, " "))
	return s
}

func looksLikeLabel(u string) bool {
	if strings.HasSuffix(u, " SECTION.") {
		return true
	}
	uu := strings.TrimSuffix(u, ".")
	if strings.IndexFunc(uu, func(r rune) bool { return r == ' ' }) != -1 {
		return false
	}
	keyverbs := []string{
		"IF ", "MOVE ", "DISPLAY ", "ACCEPT ", "SUBTRACT ", "STOP RUN", "ELSE", "END-IF", "UNSTRING ",
		"OPEN ", "CLOSE ", "READ ", "WRITE ", "REWRITE ",
	}
	for _, kv := range keyverbs {
		if strings.HasPrefix(u, kv) {
			return false
		}
	}
	return true
}

/*──────────────────────────────────────────────────────────────────────────────*/

func previewLine(s string) string {
	s = strings.TrimSpace(s)
	if len(s) > 60 {
		return s[:58] + "…"
	}
	return s
}
