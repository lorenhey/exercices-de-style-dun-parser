// Parser V7 - Rust (edition 2021)
// CKY + beam + rasgos mínimos + carga grammar.json/lexicon.json + tokenización (al/del + enclíticos)
// Export JSON + pretty tree. Dependencias: serde + serde_json.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::env;
use std::fs;

fn die(msg: &str) -> ! {
    eprintln!("{msg}");
    std::process::exit(1);
}

/* ============================================================
 * Symtab: string <-> int
 * ============================================================ */

#[derive(Default)]
struct Symtab {
    m: HashMap<String, usize>,
    id2s: Vec<String>,
}
impl Symtab {
    fn intern(&mut self, s: &str) -> usize {
        if let Some(&id) = self.m.get(s) {
            return id;
        }
        let id = self.id2s.len();
        self.id2s.push(s.to_string());
        self.m.insert(self.id2s[id].clone(), id);
        id
    }
    fn str_(&self, id: usize) -> &str {
        self.id2s.get(id).map(|s| s.as_str()).unwrap_or("?")
    }
    fn is_var(sym: &str) -> bool {
        sym.starts_with('?')
    }
}

/* ============================================================
 * Feats + unificación
 * ============================================================ */

#[derive(Clone, Copy, Debug)]
struct Feat {
    key: usize,
    val: usize,
}

#[derive(Clone, Debug, Default)]
struct Feats {
    a: Vec<Feat>,
}
impl Feats {
    fn new(mut a: Vec<Feat>) -> Self {
        a.sort_by(|x, y| x.key.cmp(&y.key).then(x.val.cmp(&y.val)));
        Self { a }
    }
    fn find_key(&self, key: usize) -> Option<usize> {
        self.a.iter().position(|f| f.key == key)
    }
    fn hash64(&self) -> u64 {
        // FNV-1a 64
        let mut h: u64 = 1469598103934665603;
        for f in &self.a {
            h ^= f.key as u64;
            h = h.wrapping_mul(1099511628211);
            h ^= f.val as u64;
            h = h.wrapping_mul(1099511628211);
        }
        h
    }
    fn unify(st: &Symtab, a: &Feats, b: &Feats) -> Option<Feats> {
        let mut tmp = a.a.clone();

        for fb in &b.a {
            let k = fb.key;
            let vb = fb.val;

            let mut idx: Option<usize> = None;
            for (i, f) in tmp.iter().enumerate() {
                if f.key == k {
                    idx = Some(i);
                    break;
                }
            }

            if idx.is_none() {
                tmp.push(*fb);
                continue;
            }

            let i = idx.unwrap();
            let va = tmp[i].val;
            if va == vb {
                continue;
            }

            let sa = st.str_(va);
            let sb = st.str_(vb);
            let va_var = Symtab::is_var(sa);
            let vb_var = Symtab::is_var(sb);

            if va_var && !vb_var {
                tmp[i] = Feat { key: k, val: vb };
                continue;
            }
            if !va_var && vb_var {
                continue;
            }
            if va_var && vb_var {
                continue;
            }
            return None; // conflicto
        }

        Some(Feats::new(tmp))
    }

    fn require(st: &Symtab, f: &Feats, key: usize, val: usize) -> Option<Feats> {
        let req = Feats::new(vec![Feat { key, val }]);
        Feats::unify(st, f, &req)
    }
}

/* ============================================================
 * Lexicon / Grammar JSON (typed)
 * ============================================================ */

#[derive(Deserialize)]
struct LexiconFile {
    entries: HashMap<String, Vec<LexEntryFile>>,
}
#[derive(Deserialize)]
struct LexEntryFile {
    pos: String,
    weight: f64,
    feats: HashMap<String, String>,
}

#[derive(Deserialize)]
struct GrammarFile {
    rules: Vec<RuleFile>,
}
#[derive(Deserialize)]
struct RuleFile {
    lhs: String,
    rhs: Vec<String>,
    weight: f64,
    op: String,
    #[serde(default)]
    args: Option<ArgsFile>,
    #[serde(default)]
    post: Option<Vec<String>>,
}
#[derive(Deserialize, Default)]
struct ArgsFile {
    #[serde(default)]
    key: Option<String>,
    #[serde(default)]
    value: Option<String>,
    #[serde(default)]
    r#type: Option<String>,
}

#[derive(Clone)]
struct LexEntry {
    pos: usize,
    weight: f64,
    feats: Feats,
}
#[derive(Default)]
struct Lexicon {
    entries: HashMap<String, Vec<LexEntry>>,
}

#[derive(Clone, Copy)]
enum Op {
    Empty,
    Left,
    Right,
    Unify,
    RequireLeft,
    RequireRight,
    MakeGap,
    RelClauseObl,
}

bitflags::bitflags! {
    struct PostFlags: u32 {
        const NONE = 0;
        const PROPAGATE_IDX_TO_RIGHT = 1;
    }
}

#[derive(Clone)]
struct Rule {
    lhs: usize,
    rhs_len: usize,
    rhs1: usize,
    rhs2: usize,
    weight: f64,
    op: Op,
    arg_key: Option<usize>,
    arg_val: Option<usize>,
    arg_type: Option<usize>,
    post: PostFlags,
}

#[derive(Default)]
struct Grammar {
    rules: Vec<Rule>,
}

/* ============================================================
 * bitflags sin crate extra? -> usamos bitflags crate… pero no está en Cargo.toml.
 * Para mantener deps mínimas, implemento flags a mano:
 * ============================================================ */

#[derive(Clone, Copy)]
struct PostFlagsLite(u32);
impl PostFlagsLite {
    const NONE: PostFlagsLite = PostFlagsLite(0);
    const PROPAGATE_IDX_TO_RIGHT: PostFlagsLite = PostFlagsLite(1);

    fn contains(self, f: PostFlagsLite) -> bool {
        (self.0 & f.0) != 0
    }
    fn insert(&mut self, f: PostFlagsLite) {
        self.0 |= f.0;
    }
}

#[derive(Clone)]
struct RuleLite {
    lhs: usize,
    rhs_len: usize,
    rhs1: usize,
    rhs2: usize,
    weight: f64,
    op: Op,
    arg_key: Option<usize>,
    arg_val: Option<usize>,
    arg_type: Option<usize>,
    post: PostFlagsLite,
}
#[derive(Default)]
struct GrammarLite {
    rules: Vec<RuleLite>,
}

/* ============================================================
 * Load model
 * ============================================================ */

fn op_from(s: &str) -> Op {
    match s {
        "EMPTY" => Op::Empty,
        "LEFT" => Op::Left,
        "RIGHT" => Op::Right,
        "UNIFY" => Op::Unify,
        "REQUIRE_LEFT" => Op::RequireLeft,
        "REQUIRE_RIGHT" => Op::RequireRight,
        "MAKE_GAP" => Op::MakeGap,
        "RELCLAUSE_OBL" => Op::RelClauseObl,
        _ => die(&format!("op desconocido: {s}")),
    }
}

fn load_lexicon(st: &mut Symtab, path: &str) -> Lexicon {
    let s = fs::read_to_string(path).unwrap_or_else(|_| die(&format!("No puedo abrir: {path}")));
    let lf: LexiconFile = serde_json::from_str(&s).unwrap_or_else(|e| die(&format!("JSON lexicon falló: {e}")));

    let mut lx = Lexicon::default();
    for (word, arr) in lf.entries {
        let mut list: Vec<LexEntry> = Vec::with_capacity(arr.len());
        for e in arr {
            let pos = st.intern(&e.pos);
            let weight = e.weight;
            let mut feats: Vec<Feat> = Vec::with_capacity(e.feats.len());
            for (k, v) in e.feats {
                feats.push(Feat { key: st.intern(&k), val: st.intern(&v) });
            }
            list.push(LexEntry { pos, weight, feats: Feats::new(feats) });
        }
        lx.entries.insert(word, list);
    }
    lx
}

fn load_grammar(st: &mut Symtab, path: &str) -> GrammarLite {
    let s = fs::read_to_string(path).unwrap_or_else(|_| die(&format!("No puedo abrir: {path}")));
    let gf: GrammarFile = serde_json::from_str(&s).unwrap_or_else(|e| die(&format!("JSON grammar falló: {e}")));

    let mut gr = GrammarLite::default();
    for rf in gf.rules {
        if rf.rhs.is_empty() || rf.rhs.len() > 2 {
            die("grammar.json: rhs len debe ser 1 o 2");
        }
        let mut post = PostFlagsLite::NONE;
        if let Some(p) = rf.post {
            for s in p {
                if s == "PROPAGATE_IDX_TO_RIGHT" {
                    post.insert(PostFlagsLite::PROPAGATE_IDX_TO_RIGHT);
                }
            }
        }

        let arg_key = rf.args.as_ref().and_then(|a| a.key.as_ref()).map(|x| st.intern(x));
        let arg_val = rf.args.as_ref().and_then(|a| a.value.as_ref()).map(|x| st.intern(x));
        let arg_type = rf.args.as_ref().and_then(|a| a.r#type.as_ref()).map(|x| st.intern(x));

        let lhs = st.intern(&rf.lhs);
        let rhs1 = st.intern(&rf.rhs[0]);
        let rhs2 = if rf.rhs.len() == 2 { st.intern(&rf.rhs[1]) } else { usize::MAX };

        gr.rules.push(RuleLite {
            lhs,
            rhs_len: rf.rhs.len(),
            rhs1,
            rhs2,
            weight: rf.weight,
            op: op_from(&rf.op),
            arg_key,
            arg_val,
            arg_type,
            post,
        });
    }
    gr
}

/* ============================================================
 * Tokenización + split oraciones
 * ============================================================ */

#[derive(Clone)]
struct Token {
    raw: String,
    text: String,
    index: usize,
}

fn split_sentences(corpus: &str) -> Vec<String> {
    let mut out = Vec::new();
    let mut start = 0usize;
    let bytes = corpus.as_bytes();
    for i in 0..=bytes.len() {
        let c = if i == bytes.len() { b'\0' } else { bytes[i] };
        if c == b'.' || c == b'\n' || c == b'\0' {
            let mut s = start;
            let mut e = i;
            while s < e && corpus[s..].chars().next().map(|ch| ch.is_whitespace()).unwrap_or(false) {
                s += corpus[s..].chars().next().unwrap().len_utf8();
            }
            while e > s {
                let prev = corpus[..e].chars().rev().next().unwrap();
                if prev.is_whitespace() {
                    e -= prev.len_utf8();
                } else {
                    break;
                }
            }
            if e > s {
                out.push(corpus[s..e].to_string());
            }
            start = i + 1;
        }
    }
    out
}

fn tokenize(s: &str) -> Vec<Token> {
    const CLITICS: [&str; 11] = ["me","te","se","lo","la","los","las","le","les","nos","os"];

    let mut out: Vec<Token> = Vec::new();
    let mut idx = 0usize;

    let mut i = 0usize;
    while i < s.len() {
        let ch = s[i..].chars().next().unwrap();
        if !ch.is_alphabetic() {
            i += ch.len_utf8();
            continue;
        }
        let start = i;
        i += ch.len_utf8();

        while i < s.len() {
            let c2 = s[i..].chars().next().unwrap();
            if c2.is_alphabetic() || c2 == '-' || c2 == '\'' {
                i += c2.len_utf8();
            } else {
                break;
            }
        }

        let raw = s[start..i].to_string();
        let low = raw.to_lowercase();

        if low == "al" {
            out.push(Token { raw: "a".into(), text: "a".into(), index: idx }); idx += 1;
            out.push(Token { raw: "el".into(), text: "el".into(), index: idx }); idx += 1;
            continue;
        }
        if low == "del" {
            out.push(Token { raw: "de".into(), text: "de".into(), index: idx }); idx += 1;
            out.push(Token { raw: "el".into(), text: "el".into(), index: idx }); idx += 1;
            continue;
        }

        // enclítico (1): elegir el más largo
        let mut best: Option<&str> = None;
        for c in CLITICS {
            if low.ends_with(c) {
                if best.map(|b| c.len() > b.len()).unwrap_or(true) {
                    best = Some(c);
                }
            }
        }

        if let Some(cl) = best {
            let cllen = cl.len();
            if low.len() > cllen + 2 {
                let base = &low[..low.len() - cllen];
                let looks_verb =
                    base.ends_with("ar") || base.ends_with("er") || base.ends_with("ir") ||
                    base.ends_with("ando") || base.ends_with("iendo");
                if looks_verb {
                    let raw_base = raw[..raw.len() - cllen].to_string();
                    let raw_cl = raw[raw.len() - cllen..].to_string();
                    out.push(Token { raw: raw_base.clone(), text: raw_base.to_lowercase(), index: idx }); idx += 1;
                    out.push(Token { raw: raw_cl.clone(), text: raw_cl.to_lowercase(), index: idx }); idx += 1;
                    continue;
                }
            }
        }

        out.push(Token { raw: raw.clone(), text: low, index: idx });
        idx += 1;
    }

    out
}

/* ============================================================
 * Árbol + arena (índices en Vec)
 * ============================================================ */

#[derive(Clone)]
struct Node {
    label: usize,
    is_leaf_token: bool,
    leaf_raw: Option<String>,
    feats: Feats,
    score: f64,
    left: Option<usize>,
    right: Option<usize>,
    child: Option<usize>,
}

#[derive(Default)]
struct Arena {
    nodes: Vec<Node>,
}
impl Arena {
    fn push(&mut self, n: Node) -> usize {
        let id = self.nodes.len();
        self.nodes.push(n);
        id
    }

    fn replace_feat_value(&mut self, node_id: usize, from_val: usize, to_val: usize) {
        fn rec(ar: &mut Arena, id: usize, from_val: usize, to_val: usize) {
            let (child, left, right);
            {
                let n = &mut ar.nodes[id];
                for f in &mut n.feats.a {
                    if f.val == from_val {
                        f.val = to_val;
                    }
                }
                child = n.child;
                left = n.left;
                right = n.right;
            }
            if let Some(c) = child { rec(ar, c, from_val, to_val); }
            if let Some(l) = left { rec(ar, l, from_val, to_val); }
            if let Some(r) = right { rec(ar, r, from_val, to_val); }
        }
        rec(self, node_id, from_val, to_val);
    }

    fn pretty(&self, st: &Symtab, node_id: usize) -> String {
        fn rec(ar: &Arena, st: &Symtab, id: usize, indent: usize, out: &mut String) {
            for _ in 0..indent { out.push_str("  "); }
            let n = &ar.nodes[id];

            if n.is_leaf_token {
                out.push_str(n.leaf_raw.as_deref().unwrap_or(""));
                out.push('\n');
                return;
            }

            out.push_str(st.str_(n.label));
            if !n.feats.a.is_empty() {
                out.push_str(" [");
                for (i, f) in n.feats.a.iter().enumerate() {
                    if i > 0 { out.push_str(", "); }
                    out.push_str(st.str_(f.key));
                    out.push('=');
                    out.push_str(st.str_(f.val));
                }
                out.push(']');
            }
            out.push_str(&format!("  (score={:.3})\n", n.score));

            if let Some(c) = n.child {
                rec(ar, st, c, indent + 1, out);
            } else {
                if let Some(l) = n.left { rec(ar, st, l, indent + 1, out); }
                if let Some(r) = n.right { rec(ar, st, r, indent + 1, out); }
            }
        }

        let mut s = String::new();
        rec(self, st, node_id, 0, &mut s);
        s
    }
}

/* ============================================================
 * Chart + beam
 * ============================================================ */

#[derive(Clone)]
struct Item {
    cat: usize,
    feats: Feats,
    feats_h: u64,
    score: f64,
    node: usize,
}

#[derive(Default)]
struct Bucket {
    cat: usize,
    items: Vec<Item>,  // desc score
    hashes: Vec<u64>,  // dedupe por hash
}

#[derive(Default)]
struct Cell {
    b: HashMap<usize, Bucket>,
}

fn bucket_has_hash(bk: &Bucket, h: u64) -> bool {
    bk.hashes.iter().any(|&x| x == h)
}

fn bucket_insert_sorted(bk: &mut Bucket, it: Item, beam: usize, pruned: &mut i32) {
    let mut pos = 0usize;
    while pos < bk.items.len() && bk.items[pos].score > it.score {
        pos += 1;
    }
    bk.items.insert(pos, it.clone());
    bk.hashes.insert(pos, it.feats_h);

    if bk.items.len() > beam {
        let removed = (bk.items.len() - beam) as i32;
        bk.items.truncate(beam);
        bk.hashes.truncate(beam);
        *pruned += removed;
    }
}

fn cell_add_item(cell: &mut Cell, it: Item, beam: usize, pruned: &mut i32) {
    let bk = cell.b.entry(it.cat).or_insert_with(|| Bucket { cat: it.cat, ..Default::default() });
    if bucket_has_hash(bk, it.feats_h) { return; }
    bucket_insert_sorted(bk, it, beam, pruned);
}

/* ============================================================
 * Operaciones (op DSL)
 * ============================================================ */

fn apply_op(st: &mut Symtab, r: &RuleLite, left: &Feats, right: &Feats) -> Option<Feats> {
    match r.op {
        Op::Empty => Some(Feats::default()),
        Op::Left => Some(left.clone()),
        Op::Right => Some(right.clone()),
        Op::Unify => Feats::unify(st, left, right),

        Op::RequireLeft => {
            let k = r.arg_key?;
            let v = r.arg_val?;
            Feats::require(st, left, k, v)
        }
        Op::RequireRight => {
            let k = r.arg_key?;
            let v = r.arg_val?;
            Feats::require(st, right, k, v)
        }

        Op::MakeGap => {
            let k_idx = st.intern("idx");
            let v_qi = st.intern("?i");
            let k_gap = st.intern("gap");
            let v_type = r.arg_type?;
            Some(Feats::new(vec![
                Feat { key: k_idx, val: v_qi },
                Feat { key: k_gap, val: v_type },
            ]))
        }

        Op::RelClauseObl => {
            let k_obl = st.intern("obl");
            let v_yes = st.intern("yes");
            let _tmp = Feats::require(st, right, k_obl, v_yes)?;

            let base = left.clone();
            let k_gap = st.intern("gap");
            let v_obl = st.intern("obl");
            let req = Feats::new(vec![Feat { key: k_gap, val: v_obl }]);
            Feats::unify(st, &base, &req)
        }
    }
}

/* ============================================================
 * Guess OOV
 * ============================================================ */

fn is_det_word(w: &str) -> bool {
    matches!(w, "el" | "la" | "los" | "las")
}

fn guess_lex(st: &mut Symtab, tk: &Token) -> Vec<LexEntry> {
    let mut out = Vec::new();

    // PropN: mayúscula ASCII inicial
    if let Some(c) = tk.raw.chars().next() {
        if c.is_ascii_uppercase() && !is_det_word(&tk.text) {
            out.push(LexEntry {
                pos: st.intern("PropN"),
                weight: 0.03,
                feats: Feats::new(vec![Feat { key: st.intern("num"), val: st.intern("sg") }]),
            });
        }
    }

    if tk.text.ends_with("mente") {
        out.push(LexEntry { pos: st.intern("Adv"), weight: -0.03, feats: Feats::default() });
    }

    let mut add_nf = |w_vi: f64, w_vt: f64| {
        let base = Feats::new(vec![
            Feat { key: st.intern("fin"), val: st.intern("no") },
            Feat { key: st.intern("obl"), val: st.intern("no") },
        ]);
        out.push(LexEntry { pos: st.intern("Vi"), weight: w_vi, feats: base.clone() });
        out.push(LexEntry { pos: st.intern("Vt"), weight: w_vt, feats: base.clone() });
    };

    if tk.text.ends_with("ar") || tk.text.ends_with("er") || tk.text.ends_with("ir") {
        add_nf(-0.12, -0.14);
    }
    if tk.text.ends_with("ando") || tk.text.ends_with("iendo") {
        add_nf(-0.14, -0.16);
    }

    if out.is_empty() {
        out.push(LexEntry {
            pos: st.intern("N"),
            weight: -0.35,
            feats: Feats::new(vec![
                Feat { key: st.intern("gen"), val: st.intern("?g") },
                Feat { key: st.intern("num"), val: st.intern("?n") },
            ]),
        });
    }

    out
}

/* ============================================================
 * Unary closure
 * ============================================================ */

fn unary_closure(
    st: &mut Symtab,
    gr: &GrammarLite,
    arena: &mut Arena,
    cell: &mut Cell,
    beam: usize,
    pruned: &mut i32,
    unary_apps: &mut i32,
) {
    let mut changed = true;
    while changed {
        changed = false;
        for r in &gr.rules {
            if r.rhs_len != 1 { continue; }
            let rhs_bucket = match cell.b.get(&r.rhs1) {
                Some(bk) => bk,
                None => continue,
            };

            let before = cell.b.get(&r.lhs).map(|b| b.items.len()).unwrap_or(0);
            let snapshot = rhs_bucket.items.clone(); // snapshot para no pelear con borrows

            for ch in snapshot {
                let pf = match apply_op(st, r, &ch.feats, &Feats::default()) {
                    Some(x) => x,
                    None => continue,
                };
                *unary_apps += 1;

                let score = ch.score + r.weight;

                let node_id = arena.push(Node {
                    label: r.lhs,
                    is_leaf_token: false,
                    leaf_raw: None,
                    feats: pf.clone(),
                    score,
                    left: None,
                    right: None,
                    child: Some(ch.node),
                });

                let it = Item {
                    cat: r.lhs,
                    feats: pf.clone(),
                    feats_h: pf.hash64(),
                    score,
                    node: node_id,
                };

                cell_add_item(cell, it, beam, pruned);
            }

            let after = cell.b.get(&r.lhs).map(|b| b.items.len()).unwrap_or(0);
            if after != before { changed = true; }
        }
    }
}

/* ============================================================
 * Sanity checks
 * ============================================================ */

fn has_desc_label(arena: &Arena, node_id: usize, label: usize) -> bool {
    let n = &arena.nodes[node_id];
    if !n.is_leaf_token && n.label == label { return true; }
    if let Some(c) = n.child { if has_desc_label(arena, c, label) { return true; } }
    if let Some(l) = n.left { if has_desc_label(arena, l, label) { return true; } }
    if let Some(r) = n.right { if has_desc_label(arena, r, label) { return true; } }
    false
}

fn sanity_s_has_vpfin(st: &mut Symtab, arena: &Arena, tree: usize) -> bool {
    let sym_s = st.intern("S");
    let sym_vpfin = st.intern("VP_FIN");
    fn walk(ar: &Arena, id: usize, sym_s: usize, sym_vpfin: usize, found: &mut bool) {
        let n = &ar.nodes[id];
        if !n.is_leaf_token && n.label == sym_s {
            if let Some(c) = n.child { if ar.nodes[c].label == sym_vpfin { *found = true; } }
            if let Some(l) = n.left  { if ar.nodes[l].label == sym_vpfin { *found = true; } }
            if let Some(r) = n.right { if ar.nodes[r].label == sym_vpfin { *found = true; } }
        }
        if let Some(c) = n.child { walk(ar, c, sym_s, sym_vpfin, found); }
        if let Some(l) = n.left  { walk(ar, l, sym_s, sym_vpfin, found); }
        if let Some(r) = n.right { walk(ar, r, sym_s, sym_vpfin, found); }
    }
    let mut found = false;
    walk(arena, tree, sym_s, sym_vpfin, &mut found);
    found
}

fn sanity_sin_takes_vpnf(st: &mut Symtab, arena: &Arena, tree: usize) -> bool {
    let sym_pinf = st.intern("Pinf");
    let sym_vpnf = st.intern("VP_NF");

    fn rec(st: &mut Symtab, ar: &Arena, id: usize, sym_pinf: usize, sym_vpnf: usize) -> bool {
        let n = &ar.nodes[id];
        if !n.is_leaf_token && n.label == sym_pinf {
            if !has_desc_label(ar, id, sym_vpnf) { return false; }
        }
        if let Some(c) = n.child { if !rec(st, ar, c, sym_pinf, sym_vpnf) { return false; } }
        if let Some(l) = n.left  { if !rec(st, ar, l, sym_pinf, sym_vpnf) { return false; } }
        if let Some(r) = n.right { if !rec(st, ar, r, sym_pinf, sym_vpnf) { return false; } }
        true
    }
    rec(st, arena, tree, sym_pinf, sym_vpnf)
}

fn sanity_enclitic_only_nf(st: &mut Symtab, arena: &Arena, tree: usize) -> bool {
    let sym_vp = st.intern("VP");
    let sym_cl = st.intern("Cl");
    let sym_vt = st.intern("Vt");
    let sym_vi = st.intern("Vi");

    fn rec(ar: &Arena, id: usize, sym_vp: usize, sym_cl: usize, sym_vt: usize, sym_vi: usize) -> bool {
        let n = &ar.nodes[id];
        if !n.is_leaf_token && n.label == sym_vp {
            if let (Some(l), Some(r)) = (n.left, n.right) {
                let rn = &ar.nodes[r];
                let ln = &ar.nodes[l];
                if !rn.is_leaf_token && rn.label == sym_cl {
                    if !ln.is_leaf_token && (ln.label == sym_vt || ln.label == sym_vi) {
                        return false;
                    }
                }
            }
        }
        if let Some(c) = n.child { if !rec(ar, c, sym_vp, sym_cl, sym_vt, sym_vi) { return false; } }
        if let Some(l) = n.left  { if !rec(ar, l, sym_vp, sym_cl, sym_vt, sym_vi) { return false; } }
        if let Some(r) = n.right { if !rec(ar, r, sym_vp, sym_cl, sym_vt, sym_vi) { return false; } }
        true
    }
    rec(arena, tree, sym_vp, sym_cl, sym_vt, sym_vi)
}

/* ============================================================
 * Parse sentence
 * ============================================================ */

#[derive(Serialize)]
struct RowOut {
    sentence: String,
    tokens: usize,
    oov_tokens: usize,
    parsed: bool,
    n_parses_returned: usize,
    best_score: Option<f64>,
    time_ms: f64,
    chart_items_total: i32,
    chart_items_max_cell: i32,
    pruned_by_beam: i32,
    unary_applications: i32,
    ambiguous_cells: i32,
    sanity_s_has_vpfin: bool,
    sanity_sin_takes_vpnf: bool,
    sanity_enclitic_only_nf: bool,
    notes: Vec<String>,
    best_tree: Option<String>,
}

#[derive(Serialize)]
struct SummaryOut {
    sentences: usize,
    coverage: f64,
    avg_tokens: f64,
    avg_oov: f64,
    total_time_ms: f64,
    avg_time_ms: f64,
    beam: usize,
    topk: usize,
    rows: Vec<RowOut>,
}

fn parse_sentence(
    st: &mut Symtab,
    lx: &Lexicon,
    gr: &GrammarLite,
    sentence: &str,
    topk: usize,
    beam: usize,
    include_tree: bool,
) -> RowOut {
    let t0 = std::time::Instant::now();

    let mut arena = Arena::default();
    let toks = tokenize(sentence);
    let n = toks.len();

    let mut row = RowOut {
        sentence: sentence.to_string(),
        tokens: n,
        oov_tokens: 0,
        parsed: false,
        n_parses_returned: 0,
        best_score: None,
        time_ms: 0.0,
        chart_items_total: 0,
        chart_items_max_cell: 0,
        pruned_by_beam: 0,
        unary_applications: 0,
        ambiguous_cells: 0,
        sanity_s_has_vpfin: false,
        sanity_sin_takes_vpnf: false,
        sanity_enclitic_only_nf: false,
        notes: Vec::new(),
        best_tree: None,
    };

    if n == 0 {
        row.notes.push("empty".into());
        row.time_ms = t0.elapsed().as_secs_f64() * 1000.0;
        return row;
    }

    let mut chart: Vec<Vec<Cell>> = (0..n).map(|_| (0..=n).map(|_| Cell::default()).collect()).collect();

    let mut pruned: i32 = 0;
    let mut unary_apps: i32 = 0;

    let sym_tok = st.intern("TOK");
    let sym_n = st.intern("N");
    let sym_propn = st.intern("PropN");
    let sym_pron = st.intern("Pron");
    let k_idx = st.intern("idx");

    // lexical init
    for i in 0..n {
        let cell = &mut chart[i][i + 1];
        let mut in_lex = false;

        if let Some(entries) = lx.entries.get(&toks[i].text) {
            in_lex = true;
            for le in entries {
                emit_lex(st, &mut arena, cell, le.clone(), &toks[i], sym_tok, sym_n, sym_propn, sym_pron, k_idx, beam, &mut pruned);
            }
        }
        if !in_lex { row.oov_tokens += 1; }

        for le in guess_lex(st, &toks[i]) {
            emit_lex(st, &mut arena, cell, le, &toks[i], sym_tok, sym_n, sym_propn, sym_pron, k_idx, beam, &mut pruned);
        }

        unary_closure(st, gr, &mut arena, cell, beam, &mut pruned, &mut unary_apps);
    }

    // CKY spans
    for span in 2..=n {
        for i in 0..=n - span {
            let j = i + span;
            let cell = &mut chart[i][j];

            for k in i + 1..j {
                let lcell = &chart[i][k];
                let rcell = &chart[k][j];
                if lcell.b.is_empty() || rcell.b.is_empty() { continue; }

                for rule in &gr.rules {
                    if rule.rhs_len != 2 { continue; }

                    let lb = match lcell.b.get(&rule.rhs1) { Some(b) => b, None => continue };
                    let rb = match rcell.b.get(&rule.rhs2) { Some(b) => b, None => continue };

                    let litems = lb.items.clone();
                    let ritems = rb.items.clone();

                    for ib in &litems {
                        for ic in &ritems {
                            let pf = match apply_op(st, rule, &ib.feats, &ic.feats) {
                                Some(x) => x,
                                None => continue,
                            };

                            let mut right_node = ic.node;
                            if rule.post.contains(PostFlagsLite::PROPAGATE_IDX_TO_RIGHT) {
                                if let Some(pos) = ib.feats.find_key(k_idx) {
                                    let idx_val = ib.feats.a[pos].val;
                                    let from = st.intern("?i");
                                    arena.replace_feat_value(right_node, from, idx_val);
                                }
                            }

                            let score = ib.score + ic.score + rule.weight;

                            let node_id = arena.push(Node {
                                label: rule.lhs,
                                is_leaf_token: false,
                                leaf_raw: None,
                                feats: pf.clone(),
                                score,
                                left: Some(ib.node),
                                right: Some(right_node),
                                child: None,
                            });

                            let it = Item {
                                cat: rule.lhs,
                                feats: pf.clone(),
                                feats_h: pf.hash64(),
                                score,
                                node: node_id,
                            };
                            cell_add_item(cell, it, beam, &mut pruned);
                        }
                    }
                }
            }

            unary_closure(st, gr, &mut arena, cell, beam, &mut pruned, &mut unary_apps);
        }
    }

    // chart metrics
    let mut total_items: i32 = 0;
    let mut max_cell: i32 = 0;
    let mut amb_cells: i32 = 0;
    for i in 0..n {
        for j in i + 1..=n {
            let mut cell_items = 0i32;
            for (_cat, bk) in &chart[i][j].b {
                cell_items += bk.items.len() as i32;
            }
            total_items += cell_items;
            if cell_items > max_cell { max_cell = cell_items; }
            if chart[i][j].b.len() >= 2 { amb_cells += 1; }
        }
    }
    row.chart_items_total = total_items;
    row.chart_items_max_cell = max_cell;
    row.ambiguous_cells = amb_cells;
    row.pruned_by_beam = pruned;
    row.unary_applications = unary_apps;

    // best S
    let sym_s = st.intern("S");
    let bucket_s = chart[0][n].b.get(&sym_s);

    if bucket_s.is_none() || bucket_s.unwrap().items.is_empty() {
        row.parsed = false;
        row.notes.push("NO_PARSE".into());
    } else {
        let bk = bucket_s.unwrap();
        row.parsed = true;
        row.n_parses_returned = topk.min(bk.items.len());
        row.best_score = Some(bk.items[0].score);

        let best_tree_id = bk.items[0].node;
        row.sanity_s_has_vpfin = sanity_s_has_vpfin(st, &arena, best_tree_id);
        row.sanity_sin_takes_vpnf = sanity_sin_takes_vpnf(st, &arena, best_tree_id);
        row.sanity_enclitic_only_nf = sanity_enclitic_only_nf(st, &arena, best_tree_id);

        if !row.sanity_s_has_vpfin { row.notes.push("WARN: S sin VP_FIN visible".into()); }
        if !row.sanity_sin_takes_vpnf { row.notes.push("WARN: 'sin' sin VP_NF bajo Pinf".into()); }
        if !row.sanity_enclitic_only_nf { row.notes.push("WARN: enclítico con verbo finito".into()); }

        if include_tree {
            row.best_tree = Some(arena.pretty(st, best_tree_id));
        }
    }

    row.time_ms = t0.elapsed().as_secs_f64() * 1000.0;
    row
}

fn emit_lex(
    st: &mut Symtab,
    arena: &mut Arena,
    cell: &mut Cell,
    mut le: LexEntry,
    tk: &Token,
    sym_tok: usize,
    sym_n: usize,
    sym_propn: usize,
    sym_pron: usize,
    k_idx: usize,
    beam: usize,
    pruned: &mut i32,
) {
    // default idx para N/PropN/Pron
    if le.pos == sym_n || le.pos == sym_propn || le.pos == sym_pron {
        if le.feats.find_key(k_idx).is_none() {
            let v = st.intern(&format!("t{}", tk.index));
            let req = Feats::new(vec![Feat { key: k_idx, val: v }]);
            if let Some(m) = Feats::unify(st, &le.feats, &req) {
                le.feats = m;
            }
        }
    }

    let leaf = arena.push(Node {
        label: sym_tok,
        is_leaf_token: true,
        leaf_raw: Some(tk.raw.clone()),
        feats: Feats::default(),
        score: le.weight,
        left: None,
        right: None,
        child: None,
    });

    let pre = arena.push(Node {
        label: le.pos,
        is_leaf_token: false,
        leaf_raw: None,
        feats: le.feats.clone(),
        score: le.weight,
        left: None,
        right: None,
        child: Some(leaf),
    });

    let it = Item {
        cat: le.pos,
        feats: le.feats.clone(),
        feats_h: le.feats.hash64(),
        score: le.weight,
        node: pre,
    };
    cell_add_item(cell, it, beam, pruned);
}

/* ============================================================
 * CLI + main
 * ============================================================ */

fn usage() {
    println!(
"Uso:
  parser_v7 [--lex lexicon.json] [--grammar grammar.json]
           [--file corpus.txt | --text \"...\"] 
           [--beam 16] [--topk 1] [--trees] [--print] [--json out.json]

Ejemplos:
  parser_v7 --file corpus.txt --print --json out.json
  parser_v7 --text \"Los científicos lo estudiaron durante décadas sin comprenderlo.\" --trees --print
"
    );
}

fn main() {
    let mut lex_path = "lexicon.json".to_string();
    let mut grammar_path = "grammar.json".to_string();
    let mut file_path = "corpus.txt".to_string();
    let mut text: Option<String> = None;
    let mut json_out: Option<String> = None;
    let mut beam: usize = 16;
    let mut topk: usize = 1;
    let mut trees = false;
    let mut print = false;

    let args: Vec<String> = env::args().collect();
    let mut i = 1usize;
    while i < args.len() {
        let a = &args[i];
        let mut need = || -> String {
            if i + 1 >= args.len() { die(&format!("Falta valor para {a}")); }
            i += 1;
            args[i].clone()
        };

        match a.as_str() {
            "--help" | "-h" => { usage(); return; }
            "--lex" => lex_path = need(),
            "--grammar" => grammar_path = need(),
            "--file" => file_path = need(),
            "--text" => text = Some(need()),
            "--json" => json_out = Some(need()),
            "--beam" => beam = need().parse().unwrap_or_else(|_| die("beam inválido")),
            "--topk" => topk = need().parse().unwrap_or_else(|_| die("topk inválido")),
            "--trees" => trees = true,
            "--print" => print = true,
            _ => die(&format!("Arg desconocido: {a}")),
        }
        i += 1;
    }

    // Load model
    let mut st = Symtab::default();
    let lx = load_lexicon(&mut st, &lex_path);
    let gr = load_grammar(&mut st, &grammar_path);

    // Input
    let corpus = match text {
        Some(t) => t,
        None => fs::read_to_string(&file_path).unwrap_or_else(|_| die(&format!("No puedo abrir: {}", file_path))),
    };
    let sents = split_sentences(&corpus);

    let mut rows: Vec<RowOut> = Vec::with_capacity(sents.len());
    let mut parsed = 0usize;
    let mut total_tokens = 0usize;
    let mut total_oov = 0usize;
    let mut total_time = 0.0f64;

    for s in &sents {
        let row = parse_sentence(&mut st, &lx, &gr, s, topk, beam, trees);
        if row.parsed { parsed += 1; }
        total_tokens += row.tokens;
        total_oov += row.oov_tokens;
        total_time += row.time_ms;

        if print {
            println!("==============================================================================");
            println!("{}", row.sentence);
            println!(
                "tokens={}  oov={}  parsed={}  parses={}  bestScore={}  time_ms={:.1}",
                row.tokens,
                row.oov_tokens,
                if row.parsed { 1 } else { 0 },
                row.n_parses_returned,
                row.best_score.map(|x| format!("{x:.6}")).unwrap_or_else(|| "null".into()),
                row.time_ms
            );
            println!(
                "chart_items={}  max_cell={}  pruned={}  unary_apps={}  amb_cells={}",
                row.chart_items_total,
                row.chart_items_max_cell,
                row.pruned_by_beam,
                row.unary_applications,
                row.ambiguous_cells
            );
            if !row.notes.is_empty() {
                println!("notes: {}", row.notes.join("; "));
            }
            if trees {
                if let Some(t) = &row.best_tree {
                    print!("{t}");
                }
            }
        }

        rows.push(row);
    }

    let sentences = sents.len();
    let coverage = if sentences == 0 { 0.0 } else { parsed as f64 / sentences as f64 };
    let avg_tokens = if sentences == 0 { 0.0 } else { total_tokens as f64 / sentences as f64 };
    let avg_oov = if sentences == 0 { 0.0 } else { total_oov as f64 / sentences as f64 };
    let avg_time = if sentences == 0 { 0.0 } else { total_time / sentences as f64 };

    if !print {
        println!(
            "SUMMARY: sentences={} coverage={:.3} avg_time_ms={:.1} beam={} top_k={}",
            sentences, coverage, avg_time, beam, topk
        );
    } else {
        println!("==============================================================================");
        println!(
            "SUMMARY\nsentences={}  coverage={:.3}  avg_tokens={:.2}  avg_oov={:.2}  avg_time_ms={:.1}  beam={}  top_k={}",
            sentences, coverage, avg_tokens, avg_oov, avg_time, beam, topk
        );
    }

    if let Some(path) = json_out {
        let out = SummaryOut {
            sentences,
            coverage,
            avg_tokens,
            avg_oov,
            total_time_ms: total_time,
            avg_time_ms: avg_time,
            beam,
            topk,
            rows,
        };
        let s = serde_json::to_string_pretty(&out).unwrap_or_else(|e| die(&format!("JSON output falló: {e}")));
        fs::write(&path, s).unwrap_or_else(|_| die(&format!("No puedo escribir: {path}")));
        println!("Wrote JSON: {path}");
    }
}
