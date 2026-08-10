"use strict";

const fs = require("fs");

class ParserV7 {
  constructor({ grammarPath, lexiconPath, beam = 8 } = {}) {
    this.beam = beam;

    // symbol table
    this.sym2id = new Map(); // string -> int
    this.id2sym = [null];    // 1-based

    // start symbol id
    this.startSym = 0;

    // lexicon indexed by word
    this.lexByWord = new Map(); // word -> [entry]

    // rules
    this.rules = [];

    // indices
    this.unaryIndex = new Map();  // rhs1 -> [ruleIdx]
    this.binaryIndex = new Map(); // "rhs1,rhs2" -> [ruleIdx]

    // node arena (1-based)
    this.arena = [null];

    if (grammarPath && lexiconPath) {
      this.loadResources(grammarPath, lexiconPath);
    }
  }

  // ---------- Public ----------
  loadResources(grammarPath, lexiconPath) {
    const g = JSON.parse(fs.readFileSync(grammarPath, "utf8"));
    const l = JSON.parse(fs.readFileSync(lexiconPath, "utf8"));

    const start = g.start ?? g.root ?? g.start_symbol ?? "S";
    this.startSym = this.intern(start);

    this.loadLexicon(l);
    this.loadGrammar(g);
    this.buildRuleIndex();
  }

  parseSentence(sentence) {
    const toks0 = this.tokenize(sentence);
    const toks = this.splitMorphology(toks0);
    const n = toks.length;
    if (n === 0) return "";

    this.arena = [null];

    // chart[i][j] with i in [0..n-1], j in [1..n]
    // we'll store as chart[i][j] where spans are [i, j) (half-open)
    const chart = Array.from({ length: n }, () => Array(n + 1).fill(null));

    // seed
    for (let i = 0; i < n; i++) {
      chart[i][i + 1] = this.initCell();
      this.seedLexical(chart[i][i + 1], toks[i]);
      this.unaryClosure(chart[i][i + 1]);
    }

    // CKY
    for (let span = 2; span <= n; span++) {
      for (let i = 0; i <= n - span; i++) {
        const j = i + span;
        const cell = this.initCell();

        for (let k = i + 1; k <= j - 1; k++) {
          const left = chart[i][k];
          const right = chart[k][j];
          if (!left || !right) continue;
          this.combineCells(cell, left, right);
        }

        this.unaryClosure(cell);
        chart[i][j] = cell;
      }
    }

    const rootCell = chart[0][n];
    const best = this.pickBestRoot(rootCell);
    if (!best) return "";
    return this.renderTree(best.node);
  }

  // ---------- Tokenization ----------
  tokenize(sentence) {
    // Unicode letters/numbers/underscore
    const s = String(sentence ?? "");
    const matches = s.match(/[\p{L}\p{N}_]+/gu) || [];
    return matches.map(t => t.toLowerCase());
  }

  splitMorphology(tokens) {
    // al -> a el ; del -> de el ; enclíticos heurísticos
    const encl = new Set(["me","te","se","lo","la","los","las","le","les","nos","os"]);
    const out = [];

    for (const t0 of tokens) {
      const t = String(t0).toLowerCase();
      if (t === "al") {
        out.push("a", "el");
        continue;
      }
      if (t === "del") {
        out.push("de", "el");
        continue;
      }

      const { base, suf } = this.maybeSplitEnclitic(t, encl);
      if (suf) out.push(base, suf);
      else out.push(t);
    }
    return out;
  }

  maybeSplitEnclitic(tok, enclSet) {
    // heurística: termina en pronombre y antes termina en r/d/n
    for (const suf of enclSet) {
      if (tok.length <= suf.length + 2) continue;
      if (!tok.endsWith(suf)) continue;
      const base = tok.slice(0, tok.length - suf.length);
      const last = base[base.length - 1];
      if (last === "r" || last === "d" || last === "n") {
        return { base, suf };
      }
    }
    return { base: tok, suf: "" };
  }

  // ---------- Symbol table ----------
  intern(s) {
    const key = String(s ?? "").trim().toUpperCase();
    if (!key) return 0;
    const hit = this.sym2id.get(key);
    if (hit) return hit;
    const id = this.id2sym.length;
    this.sym2id.set(key, id);
    this.id2sym.push(key);
    return id;
  }

  symStr(id) {
    if (!id || id <= 0 || id >= this.id2sym.length) return "<?>"; 
    return this.id2sym[id];
  }

  // ---------- Load lexicon ----------
  loadLexicon(l) {
    this.lexByWord = new Map();
    const entriesRaw = l.entries ?? l.lexicon ?? [];
    const entries = Array.isArray(entriesRaw)
      ? entriesRaw
      : Object.entries(entriesRaw).flatMap(([word, items]) =>
          (Array.isArray(items) ? items : []).map(item => ({ word, ...item }))
        );
    for (const e of entries) {
      const word = String(e.word ?? e.form ?? "").toLowerCase();
      if (!word) continue;

      const pos = this.intern(e.pos ?? e.tag ?? "X");
      const w = Number(e.weight ?? e.score ?? e.prob ?? 0);

      const { feats, sig } = this.readFeats(e);

      const entry = {
        word,
        pos,
        logw: Number.isFinite(w) ? w : 0,
        feats, // Map keyId->valId
        sig
      };

      if (!this.lexByWord.has(word)) this.lexByWord.set(word, []);
      this.lexByWord.get(word).push(entry);
    }
  }

  readFeats(e) {
    const featsRaw = e.feats ?? e.features;
    const map = new Map();

    if (featsRaw && typeof featsRaw === "object" && !Array.isArray(featsRaw)) {
      // object mapping
      for (const [k, v] of Object.entries(featsRaw)) {
        const kid = this.intern(k);
        const vid = this.intern(String(v));
        if (kid && vid) map.set(kid, vid);
      }
    } else if (Array.isArray(featsRaw)) {
      // array of {k,v}
      for (const p of featsRaw) {
        if (!p || typeof p !== "object") continue;
        if (!("k" in p) || !("v" in p)) continue;
        const kid = this.intern(p.k);
        const vid = this.intern(p.v);
        if (kid && vid) map.set(kid, vid);
      }
    }

    const sig = this.featSig(map);
    return { feats: map, sig };
  }

  featSig(featMap) {
    if (!featMap || featMap.size === 0) return "";
    const keys = Array.from(featMap.keys()).sort((a,b)=>a-b);
    return keys.map(k => `${k}:${featMap.get(k)}`).join(",");
    }

  // ---------- Load grammar ----------
  loadGrammar(g) {
    this.rules = [];
    const rules = g.rules ?? g.productions ?? [];
    for (const r of rules) {
      const lhs = this.intern(r.lhs ?? "<?>");

      let rhs = r.rhs ?? r.rhs_symbols;
      let rhsArr = [];
      if (Array.isArray(rhs)) rhsArr = rhs.map(String);
      else {
        const r1 = r.rhs1, r2 = r.rhs2;
        rhsArr = [r1, r2].filter(x => x !== undefined && x !== null && String(x).trim() !== "").map(String);
      }
      if (rhsArr.length < 1 || rhsArr.length > 2) continue;
      const rhsIds = rhsArr.map(s => this.intern(s));

      const w = Number(r.weight ?? r.score ?? r.prob ?? 0);

      const op = String(r.op ?? r.propagate ?? "MERGE").trim().toUpperCase() || "MERGE";
      const prop = op;
      const args = r.args && typeof r.args === "object" ? r.args : {};

      const csRaw = r.constraints ?? r.conds ?? [];
      const cs = [];
      if (Array.isArray(csRaw)) {
        for (const c of csRaw) {
          if (!c || typeof c !== "object") continue;
          cs.push({
            type: String(c.type ?? "REQUIRE").toUpperCase(),
            target: String(c.target ?? "LEFT").toUpperCase(),
            target2: String(c.target2 ?? c.other ?? "RIGHT").toUpperCase(),
            key: this.intern(c.key ?? ""),
            val: this.intern(c.value ?? ""),
            key2: this.intern(c.key2 ?? "")
          });
        }
      }

      this.rules.push({
        lhs,
        rhsLen: rhsIds.length,
        rhs1: rhsIds[0],
        rhs2: rhsIds[1] ?? 0,
        logw: Number.isFinite(w) ? w : 0,
        op,
        prop,
        args,
        cs
      });
    }
  }

  buildRuleIndex() {
    this.unaryIndex = new Map();
    this.binaryIndex = new Map();

    for (let i = 0; i < this.rules.length; i++) {
      const r = this.rules[i];
      if (r.rhsLen === 1) {
        const k = String(r.rhs1);
        const arr = this.unaryIndex.get(k) ?? [];
        arr.push(i);
        this.unaryIndex.set(k, arr);
      } else {
        const k = `${r.rhs1},${r.rhs2}`;
        const arr = this.binaryIndex.get(k) ?? [];
        arr.push(i);
        this.binaryIndex.set(k, arr);
      }
    }
  }

  // ---------- Arena ----------
  addLeafNode(label, leaf) {
    const node = { label, left: 0, right: 0, isLeaf: true, leaf: String(leaf) };
    this.arena.push(node);
    return this.arena.length - 1;
  }

  addUnaryNode(label, child) {
    const node = { label, left: child, right: 0, isLeaf: false, leaf: "" };
    this.arena.push(node);
    return this.arena.length - 1;
  }

  addBinaryNode(label, left, right) {
    const node = { label, left, right, isLeaf: false, leaf: "" };
    this.arena.push(node);
    return this.arena.length - 1;
  }

  // ---------- Chart cell ----------
  initCell() {
    return { items: [], byKey: new Map() }; // key = cat|sig
  }

  makeItem(cat, score, node, featsMap) {
    const sig = this.featSig(featsMap);
    return { cat, score, node, feats: featsMap, sig };
  }

  cellInsert(cell, it) {
    const k = `${it.cat}|${it.sig}`;
    if (cell.byKey.has(k)) {
      const idx = cell.byKey.get(k);
      if (it.score > cell.items[idx].score) cell.items[idx] = it;
      this.sortTrim(cell);
      return;
    }

    cell.items.push(it);
    this.sortTrim(cell);
  }

  sortTrim(cell) {
    cell.items.sort((a,b)=>b.score-a.score);
    if (cell.items.length > this.beam) cell.items.length = this.beam;

    cell.byKey.clear();
    for (let i=0; i<cell.items.length; i++) {
      const it = cell.items[i];
      cell.byKey.set(`${it.cat}|${it.sig}`, i);
    }
  }

  // ---------- Lexical seeding ----------
  seedLexical(cell, word) {
    const w = String(word).toLowerCase();
    const entries = this.lexByWord.get(w);

    if (entries && entries.length) {
      for (const e of entries) {
        const nodeId = this.addLeafNode(e.pos, word);
        const it = this.makeItem(e.pos, e.logw, nodeId, new Map(e.feats));
        this.cellInsert(cell, it);
      }
      return;
    }

    // OOV fallback
    const cat = /^[\p{Lu}]/u.test(word) ? this.intern("PROPN") : this.intern("NOUN");
    const nodeId = this.addLeafNode(cat, word);
    const it = this.makeItem(cat, Math.log(1e-6), nodeId, new Map());
    this.cellInsert(cell, it);
  }

  // ---------- Unary closure ----------
  unaryClosure(cell) {
    let changed = true;
    let iter = 0;

    while (changed && iter < 64) {
      iter++;
      changed = false;

      const snapshot = cell.items.slice();
      for (const src of snapshot) {
        const key = String(src.cat);
        const ruleIdx = this.unaryIndex.get(key);
        if (!ruleIdx) continue;

        for (const ri of ruleIdx) {
          const rule = this.rules[ri];

          const { ok, outFeats } = this.applyUnaryConstraints(rule, src.feats);
          if (!ok) continue;

          const nodeId = this.addUnaryNode(rule.lhs, src.node);
          const it = this.makeItem(rule.lhs, src.score + rule.logw, nodeId, outFeats);

          const before = cell.items.length;
          this.cellInsert(cell, it);
          if (cell.items.length > before) changed = true;
        }
      }
    }
  }

  applyUnaryConstraints(rule, childFeats) {
    return this.applyRuleOp(rule, childFeats, new Map());
  }

  // ---------- Combine (binary) ----------
  combineCells(outCell, leftCell, rightCell) {
    for (const L of leftCell.items) {
      for (const R of rightCell.items) {
        const key = `${L.cat},${R.cat}`;
        const idxs = this.binaryIndex.get(key);
        if (!idxs) continue;

        for (const ri of idxs) {
          const rule = this.rules[ri];
          const { ok, outFeats } = this.applyBinaryConstraints(rule, L.feats, R.feats);
          if (!ok) continue;

          const nodeId = this.addBinaryNode(rule.lhs, L.node, R.node);
          const it = this.makeItem(rule.lhs, L.score + R.score + rule.logw, nodeId, outFeats);
          this.cellInsert(outCell, it);
        }
      }
    }
  }

  applyBinaryConstraints(rule, lf, rf) {
    return this.applyRuleOp(rule, lf, rf);
  }

  applyRuleOp(rule, lf, rf) {
    let out;
    const op = rule.op ?? rule.prop ?? "MERGE";

    if (op === "EMPTY") {
      out = new Map();
    } else if (op === "LEFT") {
      out = new Map(lf);
    } else if (op === "RIGHT") {
      out = new Map(rf);
    } else if (op === "UNIFY" || op === "MERGE") {
      const unified = this.unifyFeats(lf, rf);
      if (!unified.ok) return { ok: false, outFeats: new Map() };
      out = unified.out;
    } else if (op === "REQUIRE_LEFT" || op === "REQUIRE_RIGHT") {
      const src = op === "REQUIRE_RIGHT" ? rf : lf;
      const key = this.intern(rule.args.key ?? "");
      const val = this.intern(rule.args.value ?? "");
      if (!key || !val || (src.get(key) ?? 0) !== val) {
        return { ok: false, outFeats: new Map() };
      }
      out = new Map(src);
    } else if (op === "MAKE_GAP") {
      out = new Map();
      out.set(this.intern("idx"), this.intern("?i"));
      out.set(this.intern("gap"), this.intern(rule.args.type ?? "gap"));
    } else if (op === "RELCLAUSE_OBL") {
      const keyObl = this.intern("obl");
      const valYes = this.intern("yes");
      if ((rf.get(keyObl) ?? 0) !== valYes) return { ok: false, outFeats: new Map() };
      out = new Map(lf);
      out.set(this.intern("gap"), this.intern("obl"));
    } else {
      out = this.mergeFeats(lf, rf);
    }

    for (const c of rule.cs) {
      const typ = c.type;

      if (typ === "REQUIRE") {
        const src = (c.target === "RIGHT") ? rf : lf;
        const v = src.get(c.key) ?? 0;
        if (v !== c.val) return { ok: false, outFeats: out };

      } else if (typ === "UNIFY") {
        const k = c.key;
        const v1 = lf.get(k) ?? 0;
        const v2 = rf.get(k) ?? 0;
        if (v1 && v2 && v1 !== v2) return { ok: false, outFeats: out };
        if (v1) out.set(k, v1);
        else if (v2) out.set(k, v2);

      } else if (typ === "AGREE") {
        const k = c.key;
        const v1 = lf.get(k) ?? 0;
        const v2 = rf.get(k) ?? 0;
        if (!v1 || !v2 || v1 !== v2) return { ok: false, outFeats: out };
        out.set(k, v1);

      } else if (typ === "ASSIGN") {
        if (c.key && c.val) out.set(c.key, c.val);
      }
    }

    return { ok: true, outFeats: this.sortFeatMap(out) };
  }

  unifyFeats(lf, rf) {
    const out = new Map(lf);
    for (const [k, v] of rf.entries()) {
      if (out.has(k) && out.get(k) !== v) return { ok: false, out: new Map() };
      out.set(k, v);
    }
    return { ok: true, out: this.sortFeatMap(out) };
  }

  mergeFeats(lf, rf) {
    const out = new Map(lf);
    for (const [k, v] of rf.entries()) {
      if (!out.has(k)) out.set(k, v);
    }
    return this.sortFeatMap(out);
  }

  sortFeatMap(map) {
    if (!map || map.size === 0) return new Map();
    const keys = Array.from(map.keys()).sort((a,b)=>a-b);
    const out = new Map();
    for (const k of keys) out.set(k, map.get(k));
    return out;
  }

  // ---------- Root + render ----------
  pickBestRoot(cell) {
    if (!cell) return null;
    let best = null;
    for (const it of cell.items) {
      if (it.cat === this.startSym) {
        if (!best || it.score > best.score) best = it;
      }
    }
    return best;
  }

  renderTree(nodeId) {
    const n = this.arena[nodeId];
    const lab = this.symStr(n.label);

    if (n.isLeaf) {
      return `(${lab} ${n.leaf})`;
    }
    if (!n.right) {
      const a = this.renderTree(n.left);
      return `(${lab} ${a})`;
    }
    const a = this.renderTree(n.left);
    const b = this.renderTree(n.right);
    return `(${lab} ${a} ${b})`;
  }
}

module.exports = { ParserV7 };
