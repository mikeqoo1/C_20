package emit

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strings"

	"c20/internal/ast"
)

// EmitNode：把多個 Program 逐一輸出成 Node.js 檔案（每個 Program 一支 .js）
// 同時在 outDir 寫入 runtime `cobol_compat.js`（由 writeRuntimeNode 負責）。
// - 這裡不負責執行，只負責把 AST 轉成具體 JS 字串並寫檔。
// - 檔名預設用 PROGRAM-ID，若無，改用來源檔名去副檔名。
func EmitNode(progs []ast.Program, outDir string) error {
	// 先確保 runtime 存在（避免 node 執行 require('./cobol_compat') 時找不到）
	if err := writeRuntimeNode(outDir); err != nil {
		return err
	}
	for _, p := range progs {
		name := strings.TrimSpace(p.ID)
		if name == "" {
			// 若無 PROGRAM-ID，用來源檔名（去掉副檔名）
			base := filepath.Base(p.Source)
			if i := strings.LastIndexByte(base, '.'); i > 0 {
				base = base[:i]
			}
			name = base
		}
		// 將 AST 轉成 JS 程式碼字串
		js, err := emitProgramNode(&p)
		if err != nil {
			return err
		}
		// 寫檔
		outPath := filepath.Join(outDir, name+".js")
		if err := os.WriteFile(outPath, []byte(js), 0o644); err != nil {
			return err
		}
	}
	return nil
}

// ───────────────────────── helpers ─────────────────────────

// jsIdent：將 COBOL 名稱正規化為 JS 變數名稱。
// 規則：
// 1) 全轉大寫。
// 2) 非 [A-Z0-9_] 的字元轉為 '_'（例如 '-' '.' 等都轉）
// 3) 壓縮連續 '_' 並去頭尾 '_'。
// 4) 空字串改成 "FIELD"。
// 5) 若首字是數字，補 "V_" 前綴（避免 JS 語法錯）。
func jsIdent(raw string) string {
	s := strings.ToUpper(strings.TrimSpace(raw))
	s = strings.Map(func(r rune) rune {
		switch {
		case r >= 'A' && r <= 'Z':
			return r
		case r >= '0' && r <= '9':
			return r
		case r == '_':
			return r
		default:
			return '_'
		}
	}, s)
	for strings.Contains(s, "__") {
		s = strings.ReplaceAll(s, "__", "_")
	}
	s = strings.Trim(s, "_")
	if s == "" {
		s = "FIELD"
	}
	if s[0] >= '0' && s[0] <= '9' {
		s = "V_" + s
	}
	return s
}

// nameGen：同名欄位（如多個 FILLER）需要生成唯一 JS 名稱：FILLER, FILLER_2, FILLER_3...
type nameGen struct{ used map[string]int }

func newNameGen() *nameGen { return &nameGen{used: map[string]int{}} }

// uniq：回傳針對某原始名稱的唯一 JS 識別字。
// 第一次使用 base -> base，第二次開始 base_2, base_3...
func (g *nameGen) uniq(raw string) string {
	base := jsIdent(raw)
	if n, ok := g.used[base]; ok {
		n++
		g.used[base] = n
		return fmt.Sprintf("%s_%d", base, n)
	}
	g.used[base] = 1
	return base
}

// jsDec：把十進位整數字串轉成「去除前導 0」的型態（避免在 JS 被視為八進位）。
// 例如："00123" -> "123"、"-0005" -> "-5"
func jsDec(raw string) string {
	s := strings.TrimSpace(raw)
	if s == "" {
		return "0"
	}
	sign := ""
	if s[0] == '-' {
		sign = "-"
		s = s[1:]
	}
	s = strings.TrimLeft(s, "0")
	if s == "" {
		s = "0"
	}
	return sign + s
}

// q：產生 JS 字面量字串（由 Go 的 %q 幫忙跳脫）
func q(s string) string {
	var b bytes.Buffer
	fmt.Fprintf(&b, "%q", s)
	return b.String()
}

// isSpacesLit：判斷一個 Expr 是否為字串常值 "SPACES"（大小寫不敏感）。
// COBOL 中 "MOVE SPACES TO X" 表示以空白填滿，對應 runtime.COBOL.spaces()
func isSpacesLit(e ast.Expr) bool {
	ls, ok := e.(ast.LitString)
	return ok && strings.EqualFold(strings.TrimSpace(ls.Val), "SPACES")
}

// ───────────────────────── emit env ─────────────────────────

// env：單支 JS 輸出過程中的狀態（縮排、名稱對應表、唯一命名器）
type env struct {
	buf     bytes.Buffer
	indent  int
	nameMap map[string]string // 「正規化名」→「此 Program 內第一個對應的 JS 名」
	gen     *nameGen
}

// line：輸出一行（自動依 indent 補兩空白縮排）
func (e *env) line(s string, a ...any) {
	for i := 0; i < e.indent; i++ {
		e.buf.WriteString("  ")
	}
	if len(a) == 0 {
		e.buf.WriteString(s)
	} else {
		fmt.Fprintf(&e.buf, s, a...)
	}
	e.buf.WriteByte('\n')
}

// ident：把 COBOL 名稱轉成當前 JS 變數名。
// 若在 nameMap 有紀錄（例如 FILLER 的第一個別名），就用該別名；否則直接用正規化名。
func (e *env) ident(name string) string {
	n := jsIdent(name)
	if v, ok := e.nameMap[n]; ok {
		return v
	}
	return n // 若未記錄，直接用正規化名
}

// ───────────────────────── auto-declare 掃描 ─────────────────────────

// collectUsedIdents：遍歷語句樹，收集「用到的識別字（基底變數名）」
// 目的：解決 COBOL 程式中常見的「未在 DATA DIVISION 宣告，但在 PROCEDURE 使用」的變數，
// 例如：ACCEPT CMD-LINE FROM COMMAND-LINE。
// emitter 可在輸出前補宣告（預設 alpha(80)），避免 Node 執行期 ReferenceError。
func collectUsedIdents(stmts []ast.Stmt, out map[string]bool) {
	var walkExpr func(ast.Expr)
	var walkBool func(ast.Bool)
	var walkStmt func(ast.Stmt)

	walkExpr = func(ex ast.Expr) {
		switch v := ex.(type) {
		case ast.Ident:
			out[jsIdent(v.Name)] = true
		case ast.RefMod:
			// ref-mod "BASE(start:length)"：要標記 base，start/len 也遞迴
			out[jsIdent(v.Base)] = true
			walkExpr(v.Start)
			walkExpr(v.Len)
		case ast.LitString, ast.LitNumber:
			// 字面值不用記
		default:
			// 未知型別：忽略
		}
	}

	walkBool = func(b ast.Bool) {
		switch v := b.(type) {
		case ast.Cmp:
			walkExpr(v.Left)
			walkExpr(v.Right)
		case ast.BoolAnd:
			walkBool(v.A)
			walkBool(v.B)
		case ast.BoolOr:
			walkBool(v.A)
			walkBool(v.B)
		default:
		}
	}

	walkStmt = func(s ast.Stmt) {
		switch v := s.(type) {
		case ast.StDisplay:
			for _, a := range v.Args {
				walkExpr(a)
			}
		case ast.StMove:
			walkExpr(v.Src)
			walkExpr(v.Dst)
		case ast.StIf:
			walkBool(v.Cond)
			for _, t := range v.Then {
				walkStmt(t)
			}
			for _, t := range v.Else {
				walkStmt(t)
			}
		case ast.StPerformUntil:
			walkBool(v.Cond)
			for _, t := range v.Body {
				walkStmt(t)
			}
		case ast.StPerformVarying:
			// 迴圈變數本身也算「使用到的識別字」
			out[jsIdent(v.VarName)] = true
			walkExpr(v.From)
			walkExpr(v.By)
			walkBool(v.Cond)
			for _, t := range v.Body {
				walkStmt(t)
			}
		case ast.StAccept:
			// 常見缺漏是接受到 CMD-LINE（未宣告），因此 Dst 要納入
			walkExpr(v.Dst)
		case ast.StSubFrom:
			// SUBTRACT <amount> FROM <dst>：dst 必定是識別字
			out[jsIdent(v.Dst)] = true
			walkExpr(v.Amount)
		case ast.StStopRun:
			// no-op
		default:
			// 未支援語句：忽略
		}
	}

	for _, s := range stmts {
		walkStmt(s)
	}
}

// ───────────────────────── emit core ─────────────────────────

// emitProgramNode：把單一 AST Program 轉為 JS 原始碼。
// 進行三步：
// 1) 輸出 DATA DIVISION（含 FILLER 唯名化、nameMap 建立）。
// 2) 掃描語句中使用的變數，對「DATA 未宣告」者做 auto-declare（const X = COBOL.alpha(80)）。
// 3) 輸出 main()：按語句序列化各個節點。
func emitProgramNode(p *ast.Program) (string, error) {
	e := &env{
		nameMap: map[string]string{},
		gen:     newNameGen(),
	}

	// header（檔頭資訊與 runtime import）
	e.line("// Auto-generated by C_20 (Go->Node)")
	if p.Source != "" {
		e.line("// Source: %s", p.Source)
	}
	e.line("\"use strict\";")
	e.line("const COBOL = require('./cobol_compat');")
	e.line("")
	e.line("// DATA DIVISION (flattened)")

	// 1) 宣告 DATA DIVISION 欄位
	declared := map[string]bool{} // 用正規化名記錄「已宣告」
	for _, d := range p.Data {
		jsName := e.gen.uniq(d.Name) // 針對 FILLER 等重名做唯一化

		// nameMap 僅記住「第一個」基底名對應的 JS 名稱，便於後續引用
		base := jsIdent(d.Name)
		if _, ok := e.nameMap[base]; !ok {
			e.nameMap[base] = jsName
		}
		declared[base] = true

		// 依 PIC 類型輸出對應 runtime 建構子
		switch d.Pic.Kind {
		case ast.PicAlpha:
			e.line("const %s = COBOL.alpha(%d); // PIC X(%d)", jsName, d.Pic.Len, d.Pic.Len)
		case ast.PicNumeric:
			e.line("const %s = COBOL.num({digits:%d, scale:%d, signed:%v}); // PIC 9(%d)", jsName, d.Pic.Digits, d.Pic.Scale, d.Pic.Signed, d.Pic.Digits)
		default:
			// 安全保守：未知就當成一位元 Alpha
			e.line("const %s = COBOL.alpha(%d); // PIC ?", jsName, 1)
		}
	}
	e.line("")

	// 2) auto-declare：把「有使用但未宣告」的識別字補成 Alpha(80)
	//    範例：ACCEPT CMD-LINE FROM COMMAND-LINE → 會自動輸出：
	//    const CMD_LINE = COBOL.alpha(80); // [auto-declared]
	used := map[string]bool{}
	collectUsedIdents(p.Stmts, used)

	// 找出缺漏（used - declared）
	var missing []string
	for base := range used {
		if !declared[base] {
			missing = append(missing, base)
		}
	}
	sort.Strings(missing) // 固定輸出順序，便於 diff 與除錯

	if len(missing) > 0 {
		for _, base := range missing {
			jsName := e.gen.uniq(base)
			if _, ok := e.nameMap[base]; !ok {
				e.nameMap[base] = jsName
			}
			// 預設 Alpha(80) 足以容納一般 ENV/命令列字串；如需更精確可視情境調整。
			e.line("const %s = COBOL.alpha(80); // [auto-declared]", jsName)
		}
		e.line("")
	}

	// 3) main()：逐條輸出語句
	e.line("function main(){")
	e.indent++

	for _, s := range p.Stmts {
		emitStmt(e, s)
	}

	e.indent--
	e.line("}")
	e.line("")
	e.line("if (require.main === module) { main(); }")
	return e.buf.String(), nil
}

// emitStmt：序列化單一語句節點為一段 JS。
// 這些 JS 依賴 runtime `cobol_compat.js` 的下列 API：
// - COBOL.alpha(len) / COBOL.num({digits,scale,signed})
// - COBOL.str(x) / COBOL.move(dst, src) / COBOL.slice(base, start, len)
// - COBOL.accept(dst, {from: "TIME"|"CENTURY-DATE"|"ENV"| "COMMAND-LINE", name?})
// - COBOL.cmp(a, op, b) / COBOL.spaces(x) / COBOL.sub(dstVar, amount)
func emitStmt(e *env, s ast.Stmt) {
	switch v := s.(type) {

	case ast.StDisplay:
		// DISPLAY 支援多參數：以 COBOL.str() 轉為字串後 console.log
		var parts []string
		for _, a := range v.Args {
			parts = append(parts, "COBOL.str("+emitExprJS(e, a)+")")
		}
		e.line("console.log(%s);", strings.Join(parts, ", "))

	case ast.StMove:
		// MOVE <src> TO <dst>
		// 若 src 是字面值 "SPACES"，改以 COBOL.spaces(dstBase) 填充。
		dstJS, dstBase := emitLHSAndBase(e, v.Dst)
		var src string
		if isSpacesLit(v.Src) {
			src = "COBOL.spaces(" + dstBase + ")"
		} else {
			src = emitExprJS(e, v.Src)
		}
		e.line("COBOL.move(%s, %s);", dstJS, src)

	case ast.StIf:
		// if / else 區塊
		cond := emitBool(e, v.Cond)
		e.line("if (%s) {", cond)
		e.indent++
		for _, t := range v.Then {
			emitStmt(e, t)
		}
		e.indent--
		if len(v.Else) > 0 {
			e.line("} else {")
			e.indent++
			for _, t := range v.Else {
				emitStmt(e, t)
			}
			e.indent--
		}
		e.line("}")

	case ast.StStopRun:
		// STOP RUN → 直接結束行程
		e.line("process.exit(0);")

	case ast.StPerformUntil:
		// while (!(cond)) { body }
		cond := emitBool(e, v.Cond)
		e.line("while (!(%s)) {", cond)
		e.indent++
		for _, t := range v.Body {
			emitStmt(e, t)
		}
		e.indent--
		e.line("}")

	case ast.StPerformVarying:
		// for 變形：VAR = FROM; for(;;){ if(cond) break; body; VAR += BY }
		varName := e.ident(v.VarName)
		from := emitExprJS(e, v.From)
		by := emitExprJS(e, v.By)
		cond := emitBool(e, v.Cond)
		e.line("%s.value = %s|0;", varName, from)
		e.line("for (;;) {")
		e.indent++
		e.line("if (%s) break;", cond)
		for _, t := range v.Body {
			emitStmt(e, t)
		}
		e.line("%s.value = (%s.value|0) + (%s|0);", varName, varName, by)
		e.indent--
		e.line("}")

	case ast.StAccept:
		// ACCEPT <dst> [FROM ...]
		dstJS, _ := emitLHSAndBase(e, v.Dst)
		switch f := v.From.(type) {
		case ast.AcceptTime:
			e.line("COBOL.accept(%s, {from:\"TIME\"});", dstJS)
		case ast.AcceptCenturyDate:
			e.line("COBOL.accept(%s, {from:\"CENTURY-DATE\"});", dstJS)
		case ast.AcceptEnv:
			e.line("COBOL.accept(%s, {from:\"ENV\", name:%s});", dstJS, q(f.Name))
		case ast.AcceptCommandLine:
			// 對應 runtime：會把 process.argv/環境組裝成字串寫入 dst
			e.line("COBOL.accept(%s, {from:\"COMMAND-LINE\"});", dstJS)
		default:
			// 安全預設：UNKNOWN（runtime 可決定忽略或寫空字串）
			e.line("COBOL.accept(%s, {from:\"UNKNOWN\"});", dstJS)
		}

	case ast.StSubFrom:
		// SUBTRACT <amount> FROM <dst>
		// 注意：dst 是變數名（以 ident 解析），amount 是任意 Expr。
		e.line("COBOL.sub(%s, %s);", e.ident(v.Dst), emitExprJS(e, v.Amount))

	default:
		// 未支援語句：留註解便於追蹤
		e.line("// [TODO not-emitted]: %#v", v)
	}
}

// emitExprJS：把 Expr（識別字/常數/RefMod）轉成 JS 表達式字串。
func emitExprJS(e *env, expr ast.Expr) string {
	switch v := expr.(type) {
	case ast.Ident:
		return e.ident(v.Name)

	case ast.LitString:
		return q(v.Val)

	case ast.LitNumber:
		return jsDec(v.Raw)

	case ast.RefMod:
		// BASE(start:length) → COBOL.slice(base, start, len)
		base := e.ident(v.Base)
		start := emitExprJS(e, v.Start)
		length := emitExprJS(e, v.Len)
		return fmt.Sprintf("COBOL.slice(%s, %s, %s)", base, start, length)

	default:
		// 安全保守：未知 expr 以 undefined 表示
		return "undefined"
	}
}

// emitLHSAndBase：針對 MOVE/ACCEPT 等「需要左值」的語句，
// 取得左值對應的 JS 表達式（可為 slice）與它的底層「base 變數名」。
// 用處：MOVE SPACES TO FOO(1:3) 需要知道 base=FOO 以便 COBOL.spaces(FOO) 生成正確長度。
func emitLHSAndBase(e *env, dst ast.Expr) (dstJS string, baseJS string) {
	switch v := dst.(type) {
	case ast.Ident:
		id := e.ident(v.Name)
		return id, id
	case ast.RefMod:
		base := e.ident(v.Base)
		start := emitExprJS(e, v.Start)
		length := emitExprJS(e, v.Len)
		return fmt.Sprintf("COBOL.slice(%s, %s, %s)", base, start, length), base
	default:
		// 例外狀況：當作一般表達式處理（避免中斷）
		return emitExprJS(e, dst), emitExprJS(e, dst)
	}
}

// emitBool：把布林條件（比較/AND/OR）轉成 JS。
// 比較由 runtime.COBOL.cmp() 處理，確保字串/數值/對齊/填充等 COBOL 規則正確。
func emitBool(e *env, b ast.Bool) string {
	switch v := b.(type) {
	case ast.Cmp:
		left := emitExprJS(e, v.Left)
		right := ""
		// IF X = SPACES → 右側以 COBOL.spaces(left) 生成相同長度的 all-spaces
		if isSpacesLit(v.Right) {
			right = "COBOL.spaces(" + left + ")"
		} else {
			right = emitExprJS(e, v.Right)
		}
		return fmt.Sprintf("COBOL.cmp(%s, %q, %s)", left, string(v.Op), right)

	case ast.BoolAnd:
		a := emitBool(e, v.A)
		bb := emitBool(e, v.B)
		return "(" + a + " && " + bb + ")"

	case ast.BoolOr:
		a := emitBool(e, v.A)
		bb := emitBool(e, v.B)
		return "(" + a + " || " + bb + ")"

	default:
		return "false"
	}
}

// sortedKeys：除錯/測試用的小工具（未在主要流程使用）
func sortedKeys(m map[string]string) []string {
	ks := make([]string, 0, len(m))
	for k := range m {
		ks = append(ks, k)
	}
	sort.Strings(ks)
	return ks
}
