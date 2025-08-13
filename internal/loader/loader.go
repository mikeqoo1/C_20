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
// 這個層只負責「把檔案讀進來，並盡量把 .REP（列印清單）裡的雜訊清乾淨」。
// - Name：來源檔路徑（用 / 正規化，方便跨平台、也讓 report/emitter 的輸出穩定）
// - Text：經 stripListingArtifacts() 處理後的純淨 COBOL 文字
type Source struct {
	Name string // 原始檔路徑
	Text string // 文字內容（已清理 .REP 雜訊）
}

// ReadFiles 讀檔並且自動清理 ACU .REP 列印清單的雜訊（頁首、頁碼、行首位址碼等）。
// 輸入：一組檔案路徑；輸出：對應的 Source 切片。
// 注意：假設檔案為 UTF-8。若來源是 Big5/GBK 等，應在外部先轉碼再呼叫本函式。
func ReadFiles(paths []string) ([]Source, error) {
	var out []Source
	for _, p := range paths {
		b, err := os.ReadFile(p)
		if err != nil {
			return nil, fmt.Errorf("read %s: %w", p, err)
		}
		raw := string(b)                // 原始內容
		clean := stripListingArtifacts(raw) // 去雜訊
		out = append(out, Source{
			// 以 "/" 正規化路徑，避免 Windows 與 *nix 的分隔字元差異
			Name: filepath.ToSlash(p),
			Text: clean,
		})
	}
	return out, nil
}

//-------------------------------
// .REP 清理（stripListingArtifacts）
// 典型的 ACUCOBOL .REP 「列印清單」會在每頁：
//   1) 頁首列印「檔名 + 日期 + ACUCOBOL-xx + Page: 0001」
//   2) 每行左邊加上 6 碼位址/行號（十六或十進）與一段空白
//   3) 頁與頁之間用 form feed(\f) 分隔
// 我們要把 (1)(2)(3) 全部剝掉，只留下 COBOL 原始碼那幾欄。
//-------------------------------

// 例如： "000442      "、"00004D      " 這種 6 碼十六進/十進位位址＋空白。
// ^[ \t]*     → 行首可有空白或 tab
// [0-9A-F]{6} → 六位數（常見為十六進，但也有純數字樣貌）
// [ \t]+      → 後面至少一段空白分隔到真正的程式碼欄位
// 說明：全部「只剝前置欄位」；若某行本來就以六位數開頭也會被剝，
// 但在 COBOL 原始碼第一欄位通常是識別區/指令，不會是 6 碼數字 + 空白，所以風險極低。
var repAddr = regexp.MustCompile(`^[ \t]*[0-9A-F]{6}[ \t]+`)

// 例如：頁首 "APPERROR.CBL  Fri ...  ACUCOBOL-85 ... Page: 0001"
// 我們用兩個規則：
//   1) repHeader1：只要行內同時包含 "ACUCOBOL..." 與 "Page: <數字>"，就視為頁首
//   2) 次要規則（在程式內部以 Contains 判斷）：" Page: " 且含 ".CBL"
// 這樣可涵蓋多數印表/列表格式差異。
var repHeader1 = regexp.MustCompile(`ACUCOBOL-?[-0-9A-Za-z]*\b.*\bPage:\s*\d+`)

// stripListingArtifacts 逐行掃描並移除：
//   - form feed(\f)
//   - 頁首列（見上方規則）
//   - 行首 6 碼位址欄
// 其餘內容保持原貌（包含空白行），維持相近的行號分布讓除錯容易。
func stripListingArtifacts(text string) string {
	// 先把各頁分隔用的 \f 去掉
	text = strings.ReplaceAll(text, "\f", "")

	var b strings.Builder
	sc := bufio.NewScanner(strings.NewReader(text))

	// 提醒：bufio.Scanner 預設單行長度 ~64KB；.REP 行通常很短，不會觸發限制。
	for sc.Scan() {
		line := sc.Text()
		trim := strings.TrimSpace(line)

		// 空白行照樣保留（保留版面/行號大致對得上）
		if trim == "" {
			b.WriteByte('\n')
			continue
		}

		// 規則 1：若行內同時有 ACUCOBOL 與 Page: N，視為頁首 → 丟掉
		if repHeader1.MatchString(line) {
			continue
		}
		// 規則 2（較寬鬆）：同時包含 " Page: " 與 ".CBL" 也視為頁首 → 丟掉
		//   許多列印樣式會在第一欄放「檔名.cbl ... Page: 0002」。
		if strings.Contains(line, " Page: ") && strings.Contains(line, ".CBL") {
			continue
		}

		// 移除行首位址欄（6 碼 + 空白）
		line = repAddr.ReplaceAllString(line, "")

		// 寫回輸出
		b.WriteString(line)
		b.WriteByte('\n')
	}
	return b.String()
}
