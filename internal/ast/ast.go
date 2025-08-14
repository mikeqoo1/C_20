package ast

// 這個套件定義了 C_20 的「中介語法樹(AST)」資料模型。
// 解析器(parser)會產生這些結構；接著各種 emitter(像 Node.js 產生器) 會讀取它們輸出目標語言。
// 設計重點：
//   1) 結構盡量貼近 COBOL 語意（例如 reference modification、ACCEPT 的來源分類）。
//   2) 保留必要的字面量原貌（像數字字面量的前導零），後端再決定如何處理。
//   3) 語句(Stmt)與運算式(Expr)分層，條件(Boolean)也獨立，便於各 emitters 組合與最佳化。

// ─── DATA MODEL ─────────────────────────────────────────────────────────────────

// PicKind 表示 PIC 的大類型（字母/數值）。
//   PicAlpha   → PIC X(n)
//   PicNumeric → PIC 9(n) [可含小數位/符號]
type PicKind int

const (
	PicAlpha PicKind = iota
	PicNumeric
)

// PicSpec 描述一個 PIC 的細節。
//   - Alpha 欄位只會用到 Len。
//   - Numeric 欄位使用 Digits/Scale/Signed；Scale 表示小數位數（虛擬小數點右邊幾位）。
//   - Comp3 保留以後做 decimal/packed 支援（目前 emitter 可能先忽略）。
type PicSpec struct {
	Kind   PicKind
	Len    int   // 只用在 alpha：X(Len)
	Digits int   // 只用在 numeric：9(Digits)
	Scale  int   // numeric 的小數位數（9(Digits) 其中的 Scale 位在小數點右側）
	Signed bool  // numeric 是否允許負號
	Comp3  bool  // 保留：packed/COMP-3；目前尚未使用，之後整合 decimal.js 時可派上用場
}

// Literal 用於部分資料區塊的初值，或日後可擴充作一般字面量。
//   - Kind: "string" 或 "number"
//   - Str : 原字串（未加工，number 也用字串保存以保留原始格式）
type Literal struct {
	Kind string // "string" | "number"
	Str  string
}

// DataItem 代表 DATA DIVISION 中的一個項目（已攤平）。
//   Level：COBOL 定義的層級（目前主要保留資訊用途）。
//   Name ：原字段名（例如 FILLER、APPIN-NAME 等，未正規化）。
//   Pic  ：PIC 規格
//   Value：可選的初值（像 VALUE "ABC" 或 VALUE 0）
type DataItem struct {
	Level int
	Name  string
	Pic   PicSpec
	Value *Literal // 可選初值（若無則為 nil）
}

// Program 是一個完整的 COBOL 程式單位。
//   ID     ：PROGRAM-ID（可能為空，emitter 會 fallback 成檔名）
//   Source ：來源檔完整路徑（便於報告/除錯）
//   Data   ：已「攤平」的資料項目清單（不含群組階層；emitters 直接使用）
//   Stmts  ：PROCEDURE DIVISION 轉出的語句序列
//   LineIndex：將 stmt 索引對應回來源行號（非必要但有助錯誤訊息/追蹤）
type Program struct {
	ID        string
	Source    string
	Data      []DataItem
	Stmts     []Stmt
	LineIndex map[int]int // key: stmt index → value: source line number
}

// ─── EXPR（運算式） ─────────────────────────────────────────────────────────────

// Expr 是所有運算式節點的共同介面。
type Expr interface{ isExpr() }

// Ident 代表一個變數/欄位名稱（未正規化）。
//   例：APPIN-NAME、CMD-LINE、ERR-FLAG 等
type Ident struct{ Name string }
func (Ident) isExpr() {}

// LitString 是字串字面量。
//   例：DISPLAY "HELLO" → "HELLO" 會以 LitString{Val:"HELLO"} 表示
type LitString struct{ Val string }
func (LitString) isExpr() {}

// LitNumber 是數字字面量，使用原始字串保留前導零等格式。
//   例：'001' 會保留 Raw:"001"。emitter 端（像 Node）再把它轉為十進位避免八進位誤判。
type LitNumber struct{ Raw string }
func (LitNumber) isExpr() {}

// RefMod 表示 COBOL 的 reference modification（切片）語法：BASE(start:length)
//   - Base  ：底層欄位名（未正規化）
//   - Start ：起始位置（1-based），通常是 LitNumber
//   - Len   ：長度，通常是 LitNumber
//   例：MOVE "A" TO APPIN-NAME(3:1) → RefMod{Base:"APPIN-NAME", Start:1, Len:1}
type RefMod struct {
	Base       string
	Start, Len Expr // 通常是 LitNumber，但保留為 Expr 以利以後表達式擴充
}
func (RefMod) isExpr() {}

// ─── BOOL / CONDITIONS（條件） ─────────────────────────────────────────────────

// Bool 是所有條件節點的共同介面。
type Bool interface{ isBool() }

// CmpOp 是比較運算子（COBOL 常見的六種）。
type CmpOp string

const (
	OpEQ CmpOp = "="   // =
	OpNE CmpOp = "<>"  // <>
	OpLT CmpOp = "<"   // <
	OpLE CmpOp = "<="  // <=
	OpGT CmpOp = ">"   // >
	OpGE CmpOp = ">="  // >=
)

// Cmp 是二元比較：Left <op> Right。
//   - Left/Right 可為任一 Expr（Ident/Lit/RefMod）
//   - 右側若在原始程式為 SPACES，parser 仍會用 LitString{"SPACES"} 表示，讓 emitter 做特別處理。
type Cmp struct {
	Left  Expr
	Op    CmpOp
	Right Expr
}
func (Cmp) isBool() {}

// BoolAnd / BoolOr：支援複合條件（短路行為交由 emitter 實作）。
type BoolAnd struct{ A, B Bool }
func (BoolAnd) isBool() {}

type BoolOr struct{ A, B Bool }
func (BoolOr) isBool() {}

// IsNumeric 對應條件「<expr> NUMERIC / NOT NUMERIC」的最小型 AST。
//   - Expr：被測試的運算式（通常是欄位或字面值）
//   - Neg ：true 代表 NOT NUMERIC
//   emitter(Node) 端以 runtime 的 isNumericStr() 實作。
type IsNumeric struct {
	Expr Expr
	Neg  bool // true = NOT NUMERIC
}
func (IsNumeric) isBool() {}

// ─── STMTS（語句） ─────────────────────────────────────────────────────────────

// Stmt 是所有語句節點的共同介面。
type Stmt interface{ isStmt() }

// StDisplay 對應 COBOL DISPLAY，可接受多個引數。
//   例：DISPLAY "A" APPIN-NAME
type StDisplay struct{ Args []Expr }
func (StDisplay) isStmt() {}

// StMove 對應 MOVE 語句：MOVE Src TO Dst。
//   - Dst 可是 Ident 或 RefMod
//   - Src 可是任一 Expr（Ident/Lit/RefMod）
//   - SPACES 會在 emitter 端被轉成等長空白（依 Dst 基底欄位長度）
type StMove struct {
	Src Expr
	Dst Expr // Ident or RefMod
}
func (StMove) isStmt() {}

// StIf 對應 IF/ELSE（ELSE 可為空）。
type StIf struct {
	Cond Bool
	Then []Stmt
	Else []Stmt
}
func (StIf) isStmt() {}

// StPerformUntil 對應 PERFORM ... UNTIL <Cond>。
//   在 emitter（Node）以 while(!(Cond)) { body } 近似實現。
type StPerformUntil struct {
	Cond Bool
	Body []Stmt
}
func (StPerformUntil) isStmt() {}

// StPerformVarying 對應 PERFORM VARYING 變數 FROM x BY y UNTIL <Cond>。
//   - VarName：迴圈變數名稱（Ident 名字字串）
//   - From/By：起始與步進（Expr）
//   - Cond   ：直到條件（Bool）
//   - Body   ：迴圈主體語句
type StPerformVarying struct {
	VarName string
	From, By Expr
	Cond     Bool
	Body     []Stmt
}
func (StPerformVarying) isStmt() {}

// StStopRun 對應 STOP RUN。
type StStopRun struct{}
func (StStopRun) isStmt() {}

// StUnstring 對應 UNSTRING 的最小子集：
//   UNSTRING <Src> DELIMITED BY ALL " " INTO a, b, c [, d] [TALLYING ct] END-UNSTRING.
//   - 我們目前只支援「以一個或多個空白切割」並把結果依序放到 Dsts；不足的補空字串，多的截掉。
//   - Tally：若有指定，emitter 會寫入實際「被賦值的欄位數」。
type StUnstring struct {
	Src   Expr   // 通常是 Ident（例如 CMD-LINE）
	Dsts  []Expr // 通常是 Ident；允許 RefMod 但目前 emitter 會當一般目的地處理
	Tally string // 可為空字串（表示未指定）
}
func (StUnstring) isStmt() {}

// StAccept 對應 ACCEPT 語句：ACCEPT <Dst> [FROM <source>]。
//   - Dst  ：目的欄位（Ident 或 RefMod）
//   - From：來源；省略時交由 emitter 採預設行為（通常是環境/時間等）。
type StAccept struct {
	Dst  Expr
	From AcceptFrom
}
func (StAccept) isStmt() {}

// AcceptFrom 是 ACCEPT 的來源分類的共同介面。
//   實作型別對應 COBOL 常見幾種來源。
type AcceptFrom interface{ isAccept() }

// ACCEPT ... FROM TIME
type AcceptTime struct{}
func (AcceptTime) isAccept() {}

// ACCEPT ... FROM CENTURY-DATE
type AcceptCenturyDate struct{}
func (AcceptCenturyDate) isAccept() {}

// ACCEPT ... FROM ENVIRONMENT "NAME"
type AcceptEnv struct{ Name string }
func (AcceptEnv) isAccept() {}

// ACCEPT ... FROM COMMAND-LINE
type AcceptCommandLine struct{}
func (AcceptCommandLine) isAccept() {}

// StSubFrom 對應 SUBTRACT <amount> FROM <dst>。
//   - Amount：可為數字字面量或運算式（目前多為 LitNumber）
//   - Dst   ：目標數值欄位名稱（以字串持有，因為就是一個變數名；在 emitter 端做查表/正規化）
type StSubFrom struct {
	Amount Expr
	Dst    string // numeric var
}
func (StSubFrom) isStmt() {}
