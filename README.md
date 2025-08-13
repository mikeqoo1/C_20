# C_20 轉換 cobol 原型計畫

1. 支援 COBOL 語法轉換

2. 支援 COBOL 變數型態的轉換

3. 程式邏輯跟執行結果不變

4. 不支援 UI 功能的轉換

5. 不支援特殊的語法 例如: CALL "C$RERR" USING ERR-STUS.

6. 完整紀錄整個轉換流程

7. 可轉換至不同語言 (Java Python Node.js Go Rust)

## 怎麼跑

```bash
go build ./cmd/c20
```

```bash
./c20 --in examples/hello/*.cbl --out out/node --report report
node out/node/HELLO.js

./c20 --in examples/hello/*.cbl --out out/node --report report
```

## 專案結構（重點檔案都有很密的註解）

- cmd/c20/main.go：CLI，展開 glob → 讀檔 → 解析 → 產 JS → 報告
- internal/loader/files.go：讀檔（之後可加 COPY/REPLACING 展開）
- internal/parser/parser.go：超精簡行為解析器（抓 PROGRAM-ID 與 DISPLAY/STOP RUN）
- internal/ast/ast.go：MVP AST（DisplayLiteral、StopRun）
- internal/codegen/nodejs/emit.go：輸出 JS 程式 + cobol_compat.js runtime
- examples/hello/hello.cbl：示範 COBOL
