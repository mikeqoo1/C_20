package parser

import (
    "testing"

    "c20/internal/ast"
)

func TestParseDisplayLiteralStripsTrailingPeriod(t *testing.T) {
    st := parseDisplay("DISPLAY \"HELLO, NODE.JS!\".")
    disp, ok := st.(ast.StDisplay)
    if !ok {
        t.Fatalf("expected StDisplay, got %T", st)
    }
    if len(disp.Args) != 1 {
        t.Fatalf("expected 1 arg, got %d", len(disp.Args))
    }
    lit, ok := disp.Args[0].(ast.LitString)
    if !ok {
        t.Fatalf("expected literal arg, got %T", disp.Args[0])
    }
    if lit.Val != "HELLO, NODE.JS!" {
        t.Fatalf("unexpected literal value %q", lit.Val)
    }
}
