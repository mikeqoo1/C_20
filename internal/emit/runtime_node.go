package emit

import (
	"os"
	"path/filepath"
)

// writeRuntimeNode：把 Node 端 runtime（cobol_compat.js）寫進 outDir。
// 這個 runtime 是轉出來的 .js 程式在 Node.js 環境執行所需的最小支援層。
// 注意：這裡僅負責寫檔，不做內容變更；內容定義在 nodeRuntime 常數字串。
func writeRuntimeNode(outDir string) error {
	if err := os.MkdirAll(outDir, 0o755); err != nil {
		return err
	}
	path := filepath.Join(outDir, "cobol_compat.js")
	return os.WriteFile(path, []byte(nodeRuntime), 0o644)
}

// nodeRuntime：真正寫入的 JS 內容。
// 設計理念：
// 1) 用簡單 JS 物件來表示 COBOL 的欄位型別（alpha / numeric）。
// 2) 提供 MOVE / ACCEPT / SUBTRACT / REFMOD(s) / 比較 等最低限度行為。
// 3) 多處加入「防呆」：就算輸入異常也盡量不中斷，讓轉譯結果可運行與除錯。
// 4) 規則採「可用為先」的簡化版（例如 numeric 以整數處理，不做 packed decimal）。
const nodeRuntime = `
// cobol_compat.js
// 這是轉譯後 JS 程式在 Node.js 執行所需的最低限度 runtime。
// 目標：提供 COBOL 的資料欄位、MOVE、比較、REFMOD（slice）、ACCEPT、SUBTRACT …等功能。

"use strict";

// ───────────────────── 型別定義（JS 物件） ─────────────────────
//
// AlphaField：字串欄位（固定長度）
//   { kind:"alpha", size:N, buf:string }
//   - size 表示固定長度
//   - buf 為目前內容（左靠、右側補空白）
//
// AlphaSlice：針對 AlphaField 的視窗切片（1-based start）
//   { kind:"alpha-slice", field:AlphaField, start:number, len:number }
//   - 對應 COBOL 的 reference modification：BASE(start:length)
//   - start 為 1-based（COBOL 習慣）；內部轉換時會改 0-based
//
// NumericField：數值欄位（簡化為整數，scale 表示小數位數，用字串輸出時才呈現）
//   { kind:"num", digits:number, scale:number, signed:boolean, value:number }
//   - value 只存「整數」，例如 PIC 9(6)V9(2) 會以 value=整數 表示（印字串時才斷點）

function alpha(n) {
  return { kind: "alpha", size: n|0, buf: " ".repeat(n|0) };
}

function num(spec) {
  return {
    kind: "num",
    digits: spec.digits|0,
    scale:  spec.scale|0,
    signed: !!spec.signed,
    value:  0
  };
}

// ───────────────────── 小工具與型別守衛 ─────────────────────
//
// 這些 helper 用來判斷型別與做共用處理；
// 目標是將 COBOL 動作（MOVE/IF/...）映射到 JS 時能保持語意一致。

function isAlpha(x)       { return x && x.kind === "alpha"; }
function isAlphaSlice(x)  { return x && x.kind === "alpha-slice"; }
function isNum(x)         { return x && x.kind === "num"; }

// 取出 alpha/slice 的長度（slice 用 len、alpha 用 size）
function alphaLen(x) {
  if (isAlphaSlice(x)) return x.len|0;
  if (isAlpha(x))      return x.size|0;
  // 若傳進來不是 alpha 類型，就當 0
  return 0;
}

// 取出 alpha/slice 的目前字串值
function alphaGet(x) {
  if (isAlpha(x)) {
    return x.buf;
  } else if (isAlphaSlice(x)) {
    // COBOL ref-mod：start 是 1-based；JS substr 用 0-based
    const i = (x.start|0) - 1;
    const j = i + (x.len|0);
    // 注意：這裡假設 x.field 為合法 AlphaField；若外部傳錯型別，
    // 我們仍會依據初始化時的防呆（slice() 中）讓 field 至少是 alpha(0)。
    return x.field.buf.slice(i, j);
  }
  // 非 alpha 類型，回空字串避免 throw
  return "";
}

// 寫入 alpha/slice：左靠、超出截斷、長度不足補空白
// 注意：這符合 COBOL 對定長字串欄位 MOVE 的常見行為（對齊/截斷/補空白）
function alphaSet(dst, srcStr) {
  const s = (srcStr == null) ? "" : String(srcStr);
  const L = alphaLen(dst);
  // 把 s 左靠到 L 長度
  let out = s;
  if (out.length > L) out = out.slice(0, L);
  if (out.length < L) out = out + " ".repeat(L - out.length);

  if (isAlpha(dst)) {
    dst.buf = out;
  } else if (isAlphaSlice(dst)) {
    const i = (dst.start|0) - 1;
    const j = i + (dst.len|0);
    // 將切片區間以 out 覆蓋回原本的 field.buf
    dst.field.buf = dst.field.buf.slice(0, i) + out + dst.field.buf.slice(j);
  }
}

// JS 顯示用：把欄位或常值轉成字串（DISPLAY/console.log 用）
// - alpha/slice：回傳目前 buf/切片文字
// - num：依 scale 輸出帶小數點的字串
// - 其他型別：String() 一般化
function str(x) {
  if (isAlpha(x) || isAlphaSlice(x)) {
    return alphaGet(x);
  }
  if (isNum(x)) {
    // 只做最簡單的 scale 處理：小數位數若 > 0 就補零
    const s = x.value;
    if ((x.scale|0) <= 0) return String(s|0);
    // e.g. 12345 with scale 2 → "12345" → "123.45"
    const neg = s < 0 ? "-" : "";
    let v = Math.abs(s)|0;
    let raw = String(v);
    while (raw.length <= x.scale) raw = "0" + raw;
    const p = raw.length - (x.scale|0);
    return neg + raw.slice(0, p) + "." + raw.slice(p);
  }
  // 常值/一般 JS 值
  return String(x);
}

// 產生與目標欄位等長的 spaces 字串（MOVE/IF 常會用到）
// 例如 IF X = SPACES、MOVE SPACES TO X 等。
function spaces(dstLike) {
  return " ".repeat(alphaLen(dstLike));
}

function numVal(x) {
  if (typeof x === "number") {
    return x|0;
  }
  if (isNum(x)) {
    return x.value|0;
  }
  if (isAlpha(x) || isAlphaSlice(x)) {
    const t = alphaGet(x).trim();
    return /^\s*[+-]?\d+\s*$/.test(t) ? (parseInt(t, 10)|0) : 0;
  }
  if (typeof x === "string") {
    const t = x.trim();
    return /^\s*[+-]?\d+\s*$/.test(t) ? (parseInt(t, 10)|0) : 0;
  }
  return 0;
}

// ───────────────────── REFMOD（slice） ─────────────────────
//
// COBOL.slice(base, start, len)
// - 回傳一個「切片視圖」，後續 MOVE/str/cmp 都可以直接將此視圖當成目標/來源。
// - start/len 以 COBOL 習慣的 1-based 計算。
// 防呆策略：若 base 不是 alpha，回傳長度 0 的 slice，避免 throw。
//
function slice(base, start, len) {
  if (!isAlpha(base)) {
    // 若不是 alpha 區，回傳一個 0 長度的 slice，避免 throw 讓程式能繼續跑
    return { kind: "alpha-slice", field: alpha(0), start: 1, len: 0 };
  }
  const s = (start|0), L = (len|0);
  if (s <= 0 || L <= 0) return { kind: "alpha-slice", field: base, start: 1, len: 0 };
  return { kind: "alpha-slice", field: base, start: s, len: L };
}

// ───────────────────── MOVE ─────────────────────
//
// MOVE 規則（簡化版）：
// - 目標是 alpha/slice：把來源轉字串後，左靠、截斷/補空白。
// - 目標是 numeric：把來源轉成整數（不處理編碼/符號欄位），寫入 .value。
//   * 來源若是 alpha，會以 parseInt(trimmed) 嘗試轉換（失敗當 0）。
//
function move(dst, src) {
  // 目標是 alpha/slice
  if (isAlpha(dst) || isAlphaSlice(dst)) {
    // src 可能是 alpha/slice、num 或一般常值
    if (isAlpha(src) || isAlphaSlice(src)) {
      alphaSet(dst, alphaGet(src));
      return;
    }
    if (isNum(src)) {
      alphaSet(dst, str(src)); // 將數值轉字串寫入
      return;
    }
    alphaSet(dst, String(src));
    return;
  }

  // 目標是 numeric
  if (isNum(dst)) {
    let v = 0;
    if (isNum(src)) {
      v = src.value|0;
    } else if (isAlpha(src) || isAlphaSlice(src)) {
      // 取出字串後盡量 parse 成整數（非數字就當 0）
      const t = alphaGet(src).trim();
      v = /^\s*-?\d+\s*$/.test(t) ? (parseInt(t, 10)|0) : 0;
    } else if (typeof src === "number") {
      v = src|0;
    } else if (typeof src === "string") {
      const t = src.trim();
      v = /^\s*-?\d+\s*$/.test(t) ? (parseInt(t, 10)|0) : 0;
    }
    dst.value = v|0;
    return;
  }

  // 其他類型直接忽略（安全起見）
}

// ───────────────────── 比較（=, <>, <, <=, >, >=） ─────────────────────
//
// cmp(a, op, b)：回傳布林值。
// 規則：
// - 若 a/b 其中一個是 numeric → 以整數比較（兩邊都嘗試轉整數）；
// - 否則以字串比較（alpha/slice 用 alphaGet；其餘 toString）。
// 注意：這裡是「實用簡化版」：無視 COLLATING SEQUENCE、刪除尾空白等更細規則。
// 若有需要，可日後提升行為嚴謹度。
//
function cmp(a, op, b) {
  // 先把 a, b 正規成值與比較模式
  let mode = "str";
  let A, B;

  if (isNum(a) || isNum(b)) {
    mode = "num";
    A = isNum(a) ? (a.value|0) : (
      (isAlpha(a)||isAlphaSlice(a)) ? (parseInt(alphaGet(a).trim(), 10)|0) :
      (typeof a === "number" ? (a|0) : parseInt(String(a).trim(), 10)|0)
    );
    B = isNum(b) ? (b.value|0) : (
      (isAlpha(b)||isAlphaSlice(b)) ? (parseInt(alphaGet(b).trim(), 10)|0) :
      (typeof b === "number" ? (b|0) : parseInt(String(b).trim(), 10)|0)
    );
  } else {
    A = (isAlpha(a)||isAlphaSlice(a)) ? alphaGet(a) : String(a);
    B = (isAlpha(b)||isAlphaSlice(b)) ? alphaGet(b) : String(b);
  }

  switch (op) {
    case "=":  return (A === B);
    case "<>": return (A !== B);
    case "<":  return (A <  B);
    case "<=": return (A <= B);
    case ">":  return (A >  B);
    case ">=": return (A >= B);
  }
  return false;
}

// ───────────────────── ACCEPT ─────────────────────
//
// accept(dst, {from: ...})
// 支援：
// - TIME           → HHMMSS
// - CENTURY-DATE   → YYYYMMDD
// - ENV + name     → process.env[name] 或 空白（未定義）
// - COMMAND-LINE   → 以空白串起 argv（去掉 node 與 script）
// 其他值一律給空白（保持安全）。
// 回寫：alpha/slice 以字串寫入；numeric 嘗試 parseInt（失敗為 0）。
//
function accept(dst, opt) {
  const from = opt && opt.from ? String(opt.from).toUpperCase() : "UNKNOWN";

  let out = "";
  const now = new Date();

  if (from === "TIME") {
    const hh = String(now.getHours()).padStart(2, "0");
    const mm = String(now.getMinutes()).padStart(2, "0");
    const ss = String(now.getSeconds()).padStart(2, "0");
    out = hh + mm + ss; // HHMMSS
  } else if (from === "CENTURY-DATE") {
    const y = String(now.getFullYear()).padStart(4, "0");
    const m = String(now.getMonth()+1).padStart(2, "0");
    const d = String(now.getDate()).padStart(2, "0");
    out = y + m + d; // YYYYMMDD
  } else if (from === "ENV") {
    const name = (opt && opt.name) ? String(opt.name) : "";
    out = (process.env[name] || "");
  } else if (from === "COMMAND-LINE") {
    const argv = process.argv.slice(2); // 去掉 node 與 script
    out = argv.join(" ");
  } else {
    out = ""; // UNKNOWN
  }

  // 寫回 dst（alpha 或 numeric 都支援）
  if (isAlpha(dst) || isAlphaSlice(dst)) {
    alphaSet(dst, out);
  } else if (isNum(dst)) {
    // 接受時間/日期到 numeric 時，盡量轉成整數（失敗就 0）
    const t = out.trim();
    dst.value = /^\s*-?\d+\s*$/.test(t) ? (parseInt(t, 10)|0) : 0;
  }
}

// ───────────────────── SUBTRACT ─────────────────────
//
// sub(dstNumeric, amount)：對應 COBOL 的「SUBTRACT amount FROM dst」
// amount 可為 number / NumericField / AlphaField（可 parse 時）。
// 注意：這裡把 numeric 視為整數；若未來要支援 packed decimal 或 BCD，需在這裡擴充。
function sub(dst, amount) {
  if (!isNum(dst)) return;

  let v = 0;
  if (typeof amount === "number") {
    v = amount|0;
  } else if (isNum(amount)) {
    v = amount.value|0;
  } else if (isAlpha(amount) || isAlphaSlice(amount)) {
    const t = alphaGet(amount).trim();
    v = /^\s*-?\d+\s*$/.test(t) ? (parseInt(t, 10)|0) : 0;
  } else {
    const t = String(amount).trim();
    v = /^\s*-?\d+\s*$/.test(t) ? (parseInt(t, 10)|0) : 0;
  }
  dst.value = (dst.value|0) - (v|0);
}

// ───────────────────── 檔案 I/O（極簡版） ─────────────────────
const fs = require("fs");

function _mkFH(path, mode){
  return { path:String(path||""), mode:String(mode||"INPUT"), pos:0, buf:null, open:true };
}

function fileOpen(path, mode /* "INPUT"|"OUTPUT"|"I-O"|"EXTEND" */){
  const fh = _mkFH(path, mode);
  try {
    if (mode === "OUTPUT") {
      fs.writeFileSync(fh.path, "");
      fh.buf = [];
    } else if (mode === "EXTEND") {
      if (!fs.existsSync(fh.path)) fs.writeFileSync(fh.path, "");
      const t = fs.readFileSync(fh.path, "utf8");
      fh.buf = t.length ? t.split(/\r?\n/) : [];
      // EXTEND 的寫入會附加在尾端
      fh.pos = fh.buf.length;
    } else { // INPUT / I-O：先當成輸入
      const t = fs.readFileSync(fh.path, "utf8");
      fh.buf = t.length ? t.split(/\r?\n/) : [];
      fh.pos = 0;
    }
  } catch (e) {
    // 檔案不存在或讀取失敗：仍回傳 handle，但當成空集合
    fh.buf = [];
    fh.pos = 0;
  }
  return fh;
}

function fileClose(fh){
  if (!fh) return;
  fh.open = false;
}

let _last = { ok:true, status:"00", recordLock:false, notFound:false };
function setLastStatus(st){
  // st 可以是 {ok, status, recordLock, notFound}
  _last = Object.assign({ ok:true, status:"00", recordLock:false, notFound:false }, st||{});
}
function lastRecordLock(){ return !!_last.recordLock; }
function lastNotFound(){ return !!_last.notFound; }

// READ：順序讀一行（沒有索引鍵與鎖，先給最小可用）
function fileRead(fh, opt){
  if (!fh || !fh.open) return { ok:false, status:"98", recordLock:false, notFound:true };
  const noMore = !fh.buf || fh.pos >= fh.buf.length;
  if (noMore) return { ok:false, status:"10", recordLock:false, notFound:true };
  const line = fh.buf[fh.pos++];
  // 這裡不自動寫回任何 record 變數；由上層在 READ 前後自行 MOVE 欄位/KEY（之後可擴充）
  return { ok:true, status:"00", recordLock:false, notFound:false, line };
}

function fileWrite(fh, recordStr){
  try{
    if (!fh || !fh.path) return { ok:false, status:"98", recordLock:false, notFound:false };
    fs.appendFileSync(fh.path, String(recordStr||"") + "\n");
    if (fh.buf) { fh.buf.push(String(recordStr||"")); fh.pos = fh.buf.length; }
    return { ok:true, status:"00", recordLock:false, notFound:false };
  }catch(e){
    return { ok:false, status:"99", recordLock:false, notFound:false };
  }
}

// REWRITE：最小 stub（先視為成功；之後可做游標覆寫）
function fileRewrite(fh, recordStr){
  // TODO: 可依需求把 fh.pos-1 的那一筆覆蓋；暫時視為成功避免流程中斷
  return { ok:true, status:"00", recordLock:false, notFound:false };
}


// 匯出給轉譯後的程式使用
module.exports = {
  alpha, num,
  move, slice, str, cmp, accept, sub,
  spaces, numVal,
  // 新增：
  fileOpen, fileClose, fileRead, fileWrite, fileRewrite,
  setLastStatus, lastRecordLock, lastNotFound
};
`
