package loader

import (
	"bufio"
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"strings"
)

// Source 是 parser 的輸入單位（單一原始檔）。
// 這層只負責把檔案讀進來，並盡量把 .REP（列印清單）裡的雜訊清乾淨。
type Source struct {
	Name string // 原始檔路徑（用 / 正規化）
	Text string // 文字內容（已清理 .REP 雜訊）
}

// ReadFiles 讀檔並清理 ACU .REP 列印清單的雜訊（頁首、頁碼、行首位址碼等）。
func ReadFiles(paths []string) ([]Source, error) {
	var out []Source
	for _, p := range paths {
		b, err := os.ReadFile(p)
		if err != nil {
			return nil, fmt.Errorf("read %s: %w", p, err)
		}
		raw := string(b)
		clean := stripListingArtifacts(raw)
		out = append(out, Source{
			Name: filepath.ToSlash(p),
			Text: clean,
		})
	}
	return out, nil
}

// -------------------------------
// .REP 清理（stripListingArtifacts）
// -------------------------------

// 例如： "000442      "、"00004D      " 這種 6 碼十六進/十進位位址＋空白。
var repAddr = regexp.MustCompile(`^[ \t]*[0-9A-F]{6}[ \t]+`)

// 例如：頁首 "APPERROR.CBL  Fri ...  ACUCOBOL-85 ... Page: 0001"
var repHeader1 = regexp.MustCompile(`ACUCOBOL-?[-0-9A-Za-z]*\b.*\bPage:\s*\d+`)

// 版面用的「點點」與長串句點（清單常見的對齊符號）
var repLeaderMidDot = regexp.MustCompile(`[·•∙⋅]+`)   // U+00B7/U+2022/U+2219/U+22C5 等
var repLeaderAsciiDots = regexp.MustCompile(`\.{3,}`) // 三個以上句點視為版面符
var repCollapseWS = regexp.MustCompile(`[ \t]+`)

// normalizeRepGutters 把版面用的符號與 NBSP 等換成一般空白，並收斂空白
func normalizeRepGutters(s string) string {
	// 非斷行空白（NBSP）→ space
	s = strings.ReplaceAll(s, "\u00A0", " ")
	// 中點/項目符號 → space
	s = repLeaderMidDot.ReplaceAllString(s, " ")
	// 很多清單會用 "...." 撐欄，視為空白
	s = repLeaderAsciiDots.ReplaceAllString(s, " ")
	// 收斂空白
	s = repCollapseWS.ReplaceAllString(s, " ")
	return strings.TrimRight(s, " ")
}

// stripListingArtifacts 逐行掃描並移除：
//   - form feed(\f)
//   - 頁首列（見上方規則）
//   - 行首 6 碼位址欄
//   - 版面符號（中點/長串句點）
//
// 其餘內容保持原貌（包含空白行）。
func stripListingArtifacts(text string) string {
	// 先把各頁分隔用的 \f 去掉
	text = strings.ReplaceAll(text, "\f", "")

	var b strings.Builder
	sc := bufio.NewScanner(strings.NewReader(text))

	for sc.Scan() {
		line := sc.Text()
		trim := strings.TrimSpace(line)

		// 空白行保留（維持行號分布）
		if trim == "" {
			b.WriteByte('\n')
			continue
		}

		// 規則 1：同時有 ACUCOBOL 與 Page: N → 頁首
		if repHeader1.MatchString(line) {
			continue
		}
		// 規則 2（寬鬆）：同時包含 " Page: " 且含 ".CBL"/".COB"/".REP" → 頁首
		if strings.Contains(line, " Page: ") &&
			(strings.Contains(line, ".CBL") || strings.Contains(line, ".COB") || strings.Contains(line, ".REP")) {
			continue
		}

		// 移除行首位址欄（6 碼 + 空白）
		line = repAddr.ReplaceAllString(line, "")

		// 去除某些清單左邊畫的輔助豎線（如果有）
		ltrim := strings.TrimLeft(line, " \t")
		if strings.HasPrefix(ltrim, "|") {
			line = strings.TrimLeft(strings.TrimPrefix(ltrim, "|"), " \t")
		}

		// 正規化版面符號（中點、長串句點…）
		line = normalizeRepGutters(line)

		// 寫回輸出
		b.WriteString(line)
		b.WriteByte('\n')
	}
	return b.String()
}
