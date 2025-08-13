package emit

import (
	"os"
	"path/filepath"
)

// writeRuntimeNode：把 Node 端 runtime（cobol_compat.js）寫進 outDir
func writeRuntimeNode(outDir string) error {
	path := filepath.Join(outDir, "cobol_compat.js")
	return os.WriteFile(path, []byte(nodeRuntime), 0o644)
}

// 真正寫入的 JS 內容（盡量自註解，方便日後維護）
const nodeRuntime = `
// cobol_compat.js
// 這是轉譯後 JS 程式在 Node.js 執行所需的最低限度 runtime。
// 目標：提供 COBOL 的資料欄位、MOVE、比較、REFMOD（slice）、ACCEPT、SUBTRACT …等功能。

"use strict";

// ───────────────────── 型別定義（JS 物件） ─────────────────────
//
// AlphaField：字串欄位（固定長度）
//   { kind:"alpha", size:N, buf:string }
//
// AlphaSlice：針對 AlphaField 的視窗切片（1-based start）
//   { kind:"alpha-slice", field:AlphaField, start:number, len:number }
//
// NumericField：數值欄位
//   { kind:"num", digits:number, scale:number, signed:boolean, value:number }

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
    return x.field.buf.slice(i, j);
  }
  return "";
}

// 寫入 alpha/slice：左靠、超出截斷、長度不足補空白
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
    dst.field.buf = dst.field.buf.slice(0, i) + out + dst.field.buf.slice(j);
  }
}

// JS 顯示用：把欄位或常值轉成字串（DISPLAY/console.log 用）
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
function spaces(dstLike) {
  return " ".repeat(alphaLen(dstLike));
}

// ───────────────────── REFMOD（slice） ─────────────────────
//
// COBOL.slice(base, start, len)
// 傳回一個視窗，之後 MOVE/STR/CMP 都可以直接拿這個切片使用。
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
// - 若 a/b 其中一個是 numeric → 以整數比較。
// - 否則以字串比較（alpha/slice 用 alphaGet；其餘 toString）。
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
// - TIME           → HHMMSS
// - CENTURY-DATE   → YYYYMMDD
// - ENV + name     → process.env[name] 或 空白
// - COMMAND-LINE   → 以空白串起 argv（去掉 node 與 script）
// 其他值一律給空白（保持安全）
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
// amount 可以是 number / NumericField / AlphaField（可 parse 時）
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

// 匯出給轉譯後的程式使用
module.exports = {
  alpha, num,
  move, slice, str, cmp, accept, sub,
  spaces
};
`
