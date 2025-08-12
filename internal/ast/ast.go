// Package ast defines a tiny AST subset to get us moving.
//
// We keep it intentionally small in MVP:
//   - Program { ID string, Stmts []Stmt }
//   - Stmt: DisplayLiteral | StopRun
package ast

// Program is a single COBOL program unit.
type Program struct {
    ID     string
    Stmts  []Stmt
    Source string // optional: original path, for comments in emitted code
}

// Stmt is a union of all statement node types we support.
type Stmt interface {
    isStmt()
}

// DisplayLiteral prints a string literal to the console.
type DisplayLiteral struct {
    Text string
}
func (DisplayLiteral) isStmt() {}

// StopRun marks program termination.
type StopRun struct{}
func (StopRun) isStmt() {}
