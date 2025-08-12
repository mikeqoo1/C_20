// Package report defines minimal report structures and writers.
package report

// Summary is a tiny status file consumed by a future UI.
type Summary struct {
    Status         string `json:"status"`
    ConvertedFiles int    `json:"converted_files"`
    Target         string `json:"target"`
}
