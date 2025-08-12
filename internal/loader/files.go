// Package loader handles reading source files.
// In the future, this is also where COPY/REPLACING expansion can be added.
package loader

import (
    "fmt"
    "os"
)

// Source is a (path, content) pair.
type Source struct {
    Path    string
    Content string
}

// ReadFiles loads each path into memory.
func ReadFiles(paths []string) ([]Source, error) {
    var out []Source
    for _, p := range paths {
        b, err := os.ReadFile(p)
        if err != nil {
            return nil, fmt.Errorf("read %s: %w", p, err)
        }
        out = append(out, Source{Path: p, Content: string(b)})
    }
    return out, nil
}
