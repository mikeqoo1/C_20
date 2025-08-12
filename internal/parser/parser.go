// Package parser implements a super-small line-oriented parser for MVP.
//
// Supported (very limited):
//   - PROGRAM-ID. NAME.
//   - PROCEDURE DIVISION.
//   - DISPLAY "literal".
//   - STOP RUN.
//
// Notes:
//   * This is purposely naive to get us shipping fast. We'll replace with a real
//     tokenizer+parser once we lock down the pipeline and tests.
package parser

import (
    "strings"

    "c20/internal/ast"
)

// ParseUnits takes raw sources and returns a list of Programs with statements.
func ParseUnits(sources []struct{ Path, Content string }) []ast.Program {
    var programs []ast.Program
    for _, src := range sources {
        pid := programIDFrom(src.Content)
        stmts := parseProcedureDivision(src.Content)
        programs = append(programs, ast.Program{
            ID:     pid,
            Stmts:  stmts,
            Source: src.Path,
        })
    }
    return programs
}

// programIDFrom finds PROGRAM-ID. NAME.
func programIDFrom(content string) string {
    pid := "UNKNOWN"
    lines := strings.Split(content, "\n")
    for _, raw := range lines {
        line := strings.TrimSpace(raw)
        up := strings.ToUpper(line)
        if strings.HasPrefix(up, "PROGRAM-ID.") {
            // e.g. PROGRAM-ID. HELLO.
            name := strings.TrimSpace(strings.TrimSuffix(line[len("PROGRAM-ID."):], "."))
            if name != "" { pid = name }
            break
        }
    }
    return sanitizeIdent(pid)
}

// parseProcedureDivision scans lines after PROCEDURE DIVISION.
func parseProcedureDivision(content string) []ast.Stmt {
    var (
        inProc bool
        stmts  []ast.Stmt
    )
    for _, raw := range strings.Split(content, "\n") {
        line := strings.TrimSpace(raw)
        if line == "" { continue }
        up := strings.ToUpper(line)

        if up == "PROCEDURE DIVISION." || strings.HasPrefix(up, "PROCEDURE DIVISION.") {
            inProc = true
            continue
        }
        if !inProc { continue }

        // DISPLAY "...".
        if strings.HasPrefix(up, "DISPLAY ") || up == "DISPLAY" {
            rest := strings.TrimSpace(line[len("DISPLAY"):])
            // Very naive: assume next non-space starts with a double-quote and ends at next quote.
            if strings.HasPrefix(rest, "\"") {
                // find closing quote
                rest2 := rest[1:]
                if idx := strings.IndexRune(rest2, '"'); idx >= 0 {
                    lit := rest2[:idx]
                    stmts = append(stmts, ast.DisplayLiteral{Text: lit})
                    continue
                }
            }
            // TODO: identifier / concatenation / commas
            continue
        }

        // STOP RUN.
        if strings.HasPrefix(up, "STOP RUN") {
            stmts = append(stmts, ast.StopRun{})
            continue
        }
    }
    return stmts
}

// sanitizeIdent -> make a JS-friendly file/class name.
func sanitizeIdent(s string) string {
    if s == "" { return "PROG" }
    out := make([]rune, 0, len(s))
    for _, r := range s {
        if (r >= 'a' && r <= 'z') || (r >= 'A' && r <= 'Z') || (r >= '0' && r <= '9') {
            out = append(out, r)
        } else {
            out = append(out, '_')
        }
    }
    if len(out) == 0 { return "PROG" }
    if out[0] >= '0' && out[0] <= '9' {
        return "_" + string(out)
    }
    return string(out)
}
