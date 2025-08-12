# 總原則（對應你的 7 點）

1. 語法轉換：建立 COBOL→中介表示（IR）→目標語言產碼器的三段式流程，先支援常見語法，逐步擴充。

2. 變數型態：建立嚴格型別系統（PIC/COMP-3/REDEFINES/OCCURS/88-level），在 IR 階段完成語意對齊，再映射到各語言等價型。

3. 結果等價：以「黃金測試（Golden Master）」比對輸入/輸出與副作用（檔案 I/O、狀態碼），確保行為不變；所有算術走十進位，嚴格落實 ON SIZE ERROR、ROUNDED、截斷規則。

4. 不支援 UI：SCREEN SECTION、REPORT SECTION 直接偵測並標示為 不轉換，在報告中列警示與可能替代方案。

5. 不支援特殊 CALL：對未知或特定擴充（如 CALL "C$RERR"）產生 明確 stub 與 TODO，保留呼叫點及參數簽名到報告。

6. 完整紀錄：全流程產出 JSONL 事件流 + HTML 報告（AST/IR/產碼對照、警告、覆蓋率、來源檔行號映射、可追溯性）。

7. 多語言目標：核心轉換引擎與 IR 一次實作，接多個產碼器（Python → Java → Node.js → Go → Rust），並提供各語言的 compat runtime 以保存 COBOL 語意。

## 系統設計（Go 版）

- cmd/c20/：CLI（cobra 或標準 flag）。
- internal/front/：前處理（COPY/REPLACING 展開、來源行號對應）、Lexer/Parser（先手寫/PEG，之後可切換 ANTLR4 Go target）。
- internal/sema/：符號表、型別（PIC/COMP-3/REDEFINES/OCCURS/88-level）、位移計算。
- internal/ir/：控制流程（基本塊/CFG）、算術（十進位）、I/O 節點。
- internal/codegen/：各目標語言產碼器（先 python）。
- internal/report/：JSONL 事件流 + HTML（之後再接簡單前端）。
- runtimes/：各目標語言的 compat runtime（Python/Java/Node/Go/Rust）。Go 自身目標時可直接用本地包。

## 語法與型別支援節奏

MVP（兩週內可反覆迭代）
    - 語法：MOVE、DISPLAY、IF/ELSE、PERFORM [UNTIL/VARYING]、STOP RUN、基本 EVALUATE。
    - 型別：PIC X/9/V9、S 符號、COMP-3 打包/解包、REDEFINES（位元組視圖）、OCCURS（固定）。
    - I/O：Sequential 檔 OPEN/READ NEXT/WRITE/REWRITE/CLOSE + FILE STATUS。
    - 報告：summary.json、conversion.jsonl（AST→IR→產碼對照）、warnings.json。
    - 測試：Golden 測試框架（給同一輸入，對比 COBOL 與轉譯後 Python 的輸出/副作用）。

M1
    - EVALUATE 完整語義、OCCURS DEPENDING ON、Indexed I/O（先用 SQLite 模擬索引/範圍掃描；再視需要換 RocksDB/Badger）。
    - 產碼器：Java / Node.js。

M2
    -  Go / Rust 產碼器、可選「慣用化」重寫（在行為等價驗證後）。

## 關鍵語義落地

- 十進位算術：Python 端走 decimal.Decimal；Go 端 runtime 用 shopspring/decimal 或自寫十進位（必要時以 C 實作關鍵路徑）。
- COMP-3：在 Go 內實作 pack/unpack + 位數檢查；對應到 Python runtime API。
- MOVE 規則：對齊、填補空白、截斷、ON SIZE ERROR 旗標。
- FILE I/O：抽象 Sequential/Relative/Indexed 介面；MVP 用檔案/SQLite，保持 START… KEY >= 與排序語義。

## 轉換與記錄流程

1. 前處理：展開 COPY/REPLACING（保留 source map）。
2. 解析：AST（保留區 A/B、原字面/註解、行號）。
3. 語意：符號/型別/位移、REDEFINES/OCCURS/88。
4. IR：CFG + 十進位運算 + I/O 節點。
5. 產碼：Python（插入 compat runtime 呼叫）。
6. 報告：JSONL/HTML；未支援語法/外部 CALL → 產 stub 與警示。
7. 測試：Golden 對照與 runlog。
