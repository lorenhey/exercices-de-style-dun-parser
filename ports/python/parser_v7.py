from __future__ import annotations
from dataclasses import dataclass
from typing import Dict, List, Tuple, Optional, Callable, Set, Any
import re
import time
import json

# ============================================================
# Tipos base
# ============================================================

Feat = Dict[str, str]

@dataclass(frozen=True)
class Token:
    raw: str
    text: str
    i: int
    start: int
    end: int

@dataclass(frozen=True)
class Node:
    label: str
    children: Tuple["Node", ...] = ()
    feats: Tuple[Tuple[str, str], ...] = ()
    score: float = 0.0

    def pretty(self, indent: int = 0) -> str:
        pad = "  " * indent
        feats_str = ""
        if self.feats:
            feats_str = " [" + ", ".join(f"{k}={v}" for k, v in self.feats) + "]"
        if not self.children:
            return f"{pad}{self.label}{feats_str}"
        lines = [f"{pad}{self.label}{feats_str}  (score={self.score:.3f})"]
        for ch in self.children:
            lines.append(ch.pretty(indent + 1))
        return "\n".join(lines)

def feats_to_tuple(f: Feat) -> Tuple[Tuple[str, str], ...]:
    return tuple(sorted(f.items()))

def tuple_to_feats(t: Tuple[Tuple[str, str], ...]) -> Feat:
    return dict(t)

def unify(a: Feat, b: Feat) -> Optional[Feat]:
    """Unificación simple con variables '?x'."""
    out = dict(a)
    for k, vb in b.items():
        if k not in out:
            out[k] = vb
            continue
        va = out[k]
        if va == vb:
            continue
        if va.startswith("?") and not vb.startswith("?"):
            out[k] = vb
            continue
        if vb.startswith("?") and not va.startswith("?"):
            continue
        if va.startswith("?") and vb.startswith("?"):
            continue
        return None
    return out

def replace_feat_values(node: Node, mapping: Dict[str, str]) -> Node:
    feats = dict(node.feats)
    changed = False
    for k, v in list(feats.items()):
        if v in mapping:
            feats[k] = mapping[v]
            changed = True
    new_children = []
    for ch in node.children:
        new_children.append(replace_feat_values(ch, mapping))
    if changed or any(ch is not old for ch, old in zip(new_children, node.children)):
        return Node(node.label, tuple(new_children), feats_to_tuple(feats), node.score)
    return node

# ============================================================
# Gramática CKY + composición
# ============================================================

@dataclass(frozen=True)
class Rule:
    lhs: str
    rhs: Tuple[str, ...]  # len 1 o 2
    compose: Callable[[Feat, Feat], Optional[Feat]]
    weight: float = 0.0

def comp_unary(child: Feat, _unused: Feat) -> Optional[Feat]:
    return dict(child)

def comp_passthrough(a: Feat, _b: Feat) -> Optional[Feat]:
    return dict(a)

def comp_empty(_a: Feat, _b: Feat) -> Optional[Feat]:
    return {}

def require_feat(f: Feat, key: str, val: str) -> Optional[Feat]:
    return unify(f, {key: val})

def comp_det_nbar(det: Feat, nbar: Feat) -> Optional[Feat]:
    return unify(det, nbar)

def comp_nbar_n(n: Feat, _u: Feat) -> Optional[Feat]:
    return dict(n)

def comp_adj_nbar(adj: Feat, nbar: Feat) -> Optional[Feat]:
    return unify(adj, nbar)

def comp_nbar_adj(nbar: Feat, adj: Feat) -> Optional[Feat]:
    return unify(nbar, adj)

def comp_pp(_p: Feat, _np: Feat) -> Optional[Feat]:
    return {}

def comp_np_nbar_pp(nbar: Feat, _pp: Feat) -> Optional[Feat]:
    return dict(nbar)

def comp_vp_from_v(v: Feat, _u: Feat) -> Optional[Feat]:
    return dict(v)

def comp_vp_vt_np(vt: Feat, _np: Feat) -> Optional[Feat]:
    return dict(vt)

def comp_vp_vdt_pp(vdt: Feat, _pp: Feat) -> Optional[Feat]:
    return dict(vdt)

def comp_vp_vp_pp(vp: Feat, _pp: Feat) -> Optional[Feat]:
    return dict(vp)

def comp_vp_vp_adv(vp: Feat, _adv: Feat) -> Optional[Feat]:
    return dict(vp)

def comp_vp_cl(_cl: Feat, vp: Feat) -> Optional[Feat]:
    return dict(vp)

def comp_v_encl(v: Feat, _cl: Feat) -> Optional[Feat]:
    return dict(v)

def make_gap(gap_type: str) -> Feat:
    return {"idx": "?i", "gap": gap_type}

def comp_gap_subj(_vp_fin: Feat, _u: Feat) -> Optional[Feat]:
    return make_gap("subj")

def comp_relclause_gap(comp: Feat, gap_feats: Feat) -> Optional[Feat]:
    return unify(comp, gap_feats)

def comp_nbar_rel(nbar: Feat, relc: Feat) -> Optional[Feat]:
    return unify(nbar, relc)

def comp_pp_rel(p: Feat, np_rel: Feat) -> Optional[Feat]:
    return unify(p, np_rel)

def comp_relclause_obl(pp_rel: Feat, s_obl: Feat) -> Optional[Feat]:
    if unify(s_obl, {"obl": "yes"}) is None:
        return None
    out = dict(pp_rel)
    out["gap"] = "obl"
    return out

RULES: List[Rule] = [
    # -------- S solo acepta VP_FIN
    Rule("S", ("NP","VP_FIN"), compose=comp_empty, weight=0.28),
    Rule("S", ("VP_FIN",),     compose=comp_unary, weight=-0.08),

    Rule("VP_FIN", ("VP",), compose=lambda v,_: require_feat(v, "fin", "yes"), weight=0.04),
    Rule("VP_NF",  ("VP",), compose=lambda v,_: require_feat(v, "fin", "no"),  weight=0.04),

    # -------- VP base
    Rule("VP", ("Vi",),      compose=comp_vp_from_v, weight=0.10),
    Rule("VP", ("Vt","NP"),  compose=comp_vp_vt_np,  weight=0.22),

    Rule("VP", ("Vdt","PP"), compose=comp_vp_vdt_pp, weight=0.06),
    Rule("VP", ("VP","NP"),  compose=comp_passthrough, weight=0.08),

    Rule("VP", ("VP","PP"),  compose=comp_vp_vp_pp,  weight=0.06),
    Rule("VP", ("VP","Adv"), compose=comp_vp_vp_adv, weight=0.06),
    Rule("VP", ("Adv","VP"), compose=lambda a,b: dict(b), weight=0.02),

    Rule("VP", ("Cl","VP"),  compose=comp_vp_cl, weight=0.06),
    Rule("VP", ("Cl","Vt"),  compose=comp_vp_cl, weight=0.18),
    Rule("VP", ("Cl","Vi"),  compose=comp_vp_cl, weight=0.10),

    # Enclíticos SOLO con no finitos
    Rule("Vt_NF", ("Vt",),   compose=lambda v,_: require_feat(v, "fin", "no"), weight=0.02),
    Rule("Vi_NF", ("Vi",),   compose=lambda v,_: require_feat(v, "fin", "no"), weight=0.02),
    Rule("VP", ("Vt_NF","Cl"), compose=comp_v_encl, weight=0.10),
    Rule("VP", ("Vi_NF","Cl"), compose=comp_v_encl, weight=0.06),

    # -------- PP
    Rule("PP", ("P","NP"),       compose=comp_pp, weight=0.16),
    Rule("PP", ("Pinf","VP_NF"), compose=comp_pp, weight=0.30),

    # -------- NP / NBar
    Rule("NP",   ("Det","NBar"), compose=comp_det_nbar, weight=0.38),
    Rule("NP",   ("NBar",),      compose=comp_unary,    weight=0.08),
    Rule("NP",   ("Pron",),      compose=comp_unary,    weight=0.10),
    Rule("NP",   ("PropN",),     compose=comp_unary,    weight=0.10),

    Rule("NBar", ("N",),         compose=comp_nbar_n,   weight=0.14),
    Rule("NBar", ("Adj","NBar"), compose=comp_adj_nbar, weight=0.06),
    Rule("NBar", ("NBar","Adj"), compose=comp_nbar_adj, weight=0.08),
    Rule("NP",   ("NBar","PP"),  compose=comp_np_nbar_pp, weight=0.04),

    # -------- Relativas desnudas
    Rule("RelClause", ("Comp","S_SUBJ_GAP"), compose=comp_relclause_gap, weight=0.30),
    Rule("RelClause", ("Comp","S_OBJ_GAP"),  compose=comp_relclause_gap, weight=0.30),

    # -------- Relativas oblicuas (pied-piping)
    Rule("S_OBL", ("S",),      compose=lambda s,_: require_feat(s, "obl", "yes"), weight=0.02),
    Rule("S_OBL", ("VP_FIN",), compose=lambda v,_: require_feat(v, "obl", "yes"), weight=0.02),
    Rule("RelClause", ("PP_REL","S_OBL"), compose=comp_relclause_obl, weight=0.18),

    Rule("NBar", ("NBar","RelClause"), compose=comp_nbar_rel, weight=0.14),

    # -------- Huecos
    Rule("S_SUBJ_GAP", ("VP_FIN",), compose=comp_gap_subj, weight=0.22),

    Rule("S_OBJ_GAP", ("NP","VP_OBJ_GAP"), compose=lambda a,b: make_gap("obj"), weight=0.22),
    Rule("S_OBJ_GAP", ("VP_OBJ_GAP",),     compose=lambda a,_: make_gap("obj"), weight=0.12),

    Rule("VP_OBJ_GAP", ("Vt",),      compose=lambda v,_: make_gap("obj"), weight=0.18),
    Rule("VP_OBJ_GAP", ("Vdt","PP"), compose=lambda a,b: make_gap("obj"), weight=0.10),
    Rule("VP_OBJ_GAP", ("VP_OBJ_GAP","PP"),  compose=comp_passthrough, weight=0.04),
    Rule("VP_OBJ_GAP", ("VP_OBJ_GAP","Adv"), compose=comp_passthrough, weight=0.04),

    # -------- PP_REL: P + (Det) + que (pronombre relativo)
    Rule("NP_REL", ("RelPro",),      compose=comp_unary, weight=0.15),
    Rule("NP_REL", ("Det","RelPro"), compose=lambda d,r: unify(d,r), weight=0.20),
    Rule("PP_REL", ("P","NP_REL"),   compose=comp_pp_rel, weight=0.22),
]

BINARY = [r for r in RULES if len(r.rhs) == 2]
UNARY  = [r for r in RULES if len(r.rhs) == 1]

# ============================================================
# Léxico + morfología (contracciones / enclíticos)
# ============================================================

@dataclass(frozen=True)
class LexEntry:
    pos: str
    feats: Feat
    weight: float = 0.0

def Vfeats(fin: str, obl: str) -> Feat:
    return {"fin": fin, "obl": obl}

LEXICON: Dict[str, List[LexEntry]] = {
    # determinantes
    "el":  [LexEntry("Det", {"gen":"m","num":"sg"}, 0.25)],
    "la":  [LexEntry("Det", {"gen":"f","num":"sg"}, 0.25), LexEntry("Cl", {}, 0.18)],
    "los": [LexEntry("Det", {"gen":"m","num":"pl"}, 0.25), LexEntry("Cl", {}, 0.12)],
    "las": [LexEntry("Det", {"gen":"f","num":"pl"}, 0.25), LexEntry("Cl", {}, 0.12)],
    "un":  [LexEntry("Det", {"gen":"m","num":"sg"}, 0.18)],
    "una": [LexEntry("Det", {"gen":"f","num":"sg"}, 0.18)],

    # que bifurcado
    "que": [
        LexEntry("Comp",   {"idx":"?i"}, 0.30),
        LexEntry("RelPro", {"idx":"?i"}, 0.18),
    ],

    # preposiciones / Pinf
    "a":  [LexEntry("P", {}, 0.18)],
    "de": [LexEntry("P", {}, 0.20)],
    "en": [LexEntry("P", {}, 0.20)],
    "durante": [LexEntry("P", {}, 0.16)],
    "sin": [LexEntry("Pinf", {}, 0.14), LexEntry("P", {}, -0.10)],

    # clíticos
    "lo": [LexEntry("Cl", {}, 0.22)],
    "le": [LexEntry("Cl", {}, 0.20)],
    "se": [LexEntry("Cl", {}, 0.20)],

    # adv
    "nunca": [LexEntry("Adv", {}, 0.12)],
    "repentinamente": [LexEntry("Adv", {}, 0.06)],
    "espontáneamente": [LexEntry("Adv", {}, 0.06)],
}

# Corpus-oriented
LEXICON.update({
    # N
    "filósofo": [LexEntry("N", {"gen":"m","num":"sg"}, 0.12)],
    "tratado": [LexEntry("N", {"gen":"m","num":"sg"}, 0.10)],
    "exilio": [LexEntry("N", {"gen":"m","num":"sg"}, 0.10)],
    "teoría": [LexEntry("N", {"gen":"f","num":"sg"}, 0.12)],
    "epistemología": [LexEntry("N", {"gen":"f","num":"sg"}, 0.10)],
    "científicos": [LexEntry("N", {"gen":"m","num":"pl"}, 0.10)],
    "décadas": [LexEntry("N", {"gen":"f","num":"pl"}, 0.10)],
    "paradigma": [LexEntry("N", {"gen":"m","num":"sg"}, 0.10)],
    "física": [LexEntry("N", {"gen":"f","num":"sg"}, 0.10)],
    "evidencia": [LexEntry("N", {"gen":"f","num":"sg"}, 0.10)],
    "investigador": [LexEntry("N", {"gen":"m","num":"sg"}, 0.10)],
    "hipótesis": [LexEntry("N", {"gen":"f","num":"sg"}, 0.10)],
    "manuscrito": [LexEntry("N", {"gen":"m","num":"sg"}, 0.10)],
    "anotaciones": [LexEntry("N", {"gen":"f","num":"pl"}, 0.10)],
    "revolución": [LexEntry("N", {"gen":"f","num":"sg"}, 0.10)],
    "estructuras": [LexEntry("N", {"gen":"f","num":"pl"}, 0.10)],
    "argumento": [LexEntry("N", {"gen":"m","num":"sg"}, 0.10)],
    "autor": [LexEntry("N", {"gen":"m","num":"sg"}, 0.10)],
    "capítulos": [LexEntry("N", {"gen":"m","num":"pl"}, 0.10)],
    "consecuencias": [LexEntry("N", {"gen":"f","num":"pl"}, 0.10)],
    "economistas": [LexEntry("N", {"gen":"m","num":"pl"}, 0.10)],
    "fenómeno": [LexEntry("N", {"gen":"m","num":"sg"}, 0.10)],
    "sistemas": [LexEntry("N", {"gen":"m","num":"pl"}, 0.10)],
    "crítica": [LexEntry("N", {"gen":"f","num":"sg"}, 0.10)],
    "empiristas": [LexEntry("N", {"gen":"m","num":"pl"}, 0.10)],
    "concepto": [LexEntry("N", {"gen":"m","num":"sg"}, 0.10)],
    "categorías": [LexEntry("N", {"gen":"f","num":"pl"}, 0.10)],

    # Adj
    "industrial": [LexEntry("Adj", {"gen":"?g","num":"sg"}, 0.05)],
    "contemporánea": [LexEntry("Adj", {"gen":"f","num":"sg"}, 0.08)],
    "alternativa": [LexEntry("Adj", {"gen":"f","num":"sg"}, 0.08)],
    "marginales": [LexEntry("Adj", {"gen":"?g","num":"pl"}, 0.05)],
    "extensas": [LexEntry("Adj", {"gen":"f","num":"pl"}, 0.06)],
    "sociales": [LexEntry("Adj", {"gen":"?g","num":"pl"}, 0.05)],
    "posteriores": [LexEntry("Adj", {"gen":"?g","num":"pl"}, 0.05)],
    "complejos": [LexEntry("Adj", {"gen":"m","num":"pl"}, 0.05)],
    "insuficiente": [LexEntry("Adj", {"gen":"?g","num":"sg"}, 0.05)],
    "tradicionales": [LexEntry("Adj", {"gen":"?g","num":"pl"}, 0.05)],

    # Vi finitos
    "murió": [LexEntry("Vi", Vfeats("yes","yes"), 0.12)],
    "colapsó": [LexEntry("Vi", Vfeats("yes","yes"), 0.12)],
    "prevalecían": [LexEntry("Vi", Vfeats("yes","no"), 0.12)],
    "emerge": [LexEntry("Vi", Vfeats("yes","yes"), 0.12)],
    "materializaron": [LexEntry("Vi", Vfeats("yes","no"), 0.08)],

    # Vt finitos
    "escribió": [LexEntry("Vt", Vfeats("yes","no"), 0.12)],
    "propuso": [LexEntry("Vt", Vfeats("yes","no"), 0.12)],
    "transformó": [LexEntry("Vt", Vfeats("yes","no"), 0.12)],
    "estudiaron": [LexEntry("Vt", Vfeats("yes","yes"), 0.12)],
    "dominaba": [LexEntry("Vt", Vfeats("yes","no"), 0.12)],
    "descubrieron": [LexEntry("Vt", Vfeats("yes","no"), 0.12)],
    "contiene": [LexEntry("Vt", Vfeats("yes","no"), 0.12)],
    "desarrolla": [LexEntry("Vt", Vfeats("yes","no"), 0.12)],
    "refuta": [LexEntry("Vt", Vfeats("yes","yes"), 0.12)],
    "previeron": [LexEntry("Vt", Vfeats("yes","no"), 0.12)],
    "formularon": [LexEntry("Vt", Vfeats("yes","no"), 0.12)],
    "introduce": [LexEntry("Vt", Vfeats("yes","no"), 0.12)],
    "desestabiliza": [LexEntry("Vt", Vfeats("yes","no"), 0.12)],

    # Vdt
    "sugiere": [LexEntry("Vdt", Vfeats("yes","yes"), 0.12)],
})

CLITICS = ("me","te","se","lo","la","los","las","le","les","nos","os")
WORD_RE = re.compile(r"[A-Za-zÁÉÍÓÚÜÑáéíóúüñ]+(?:[-'][A-Za-zÁÉÍÓÚÜÑáéíóúüñ]+)*")

def split_contraction(raw: str) -> Optional[List[str]]:
    w = raw.lower()
    if w == "al":
        return ["a", "el"]
    if w == "del":
        return ["de", "el"]
    return None

def split_enclitic(raw: str) -> Optional[List[str]]:
    w = raw.lower()
    for cl in sorted(CLITICS, key=len, reverse=True):
        if w.endswith(cl) and len(w) > len(cl) + 2:
            base = w[:-len(cl)]
            if base.endswith(("ar","er","ir","ando","iendo")):
                return [raw[:-len(cl)], raw[-len(cl):]]
    return None

def tokenize(text: str) -> List[Token]:
    out: List[Token] = []
    idx = 0
    for m in WORD_RE.finditer(text):
        raw = m.group(0)
        parts = split_contraction(raw) or split_enclitic(raw) or [raw]

        if len(parts) == 1:
            out.append(Token(raw=raw, text=raw.lower(), i=idx, start=m.start(), end=m.end()))
            idx += 1
        else:
            start = m.start()
            end = m.end()
            if parts[0].lower() in ("a","de") and parts[1].lower() == "el":
                mid = start + 1
                out.append(Token(raw=parts[0], text=parts[0].lower(), i=idx, start=start, end=mid)); idx += 1
                out.append(Token(raw=parts[1], text=parts[1].lower(), i=idx, start=mid, end=end)); idx += 1
            else:
                base, cl = parts[0], parts[1]
                mid = end - len(cl)
                out.append(Token(raw=base, text=base.lower(), i=idx, start=start, end=mid)); idx += 1
                out.append(Token(raw=cl,   text=cl.lower(),   i=idx, start=mid,   end=end)); idx += 1
    return out

def split_sentences(text: str) -> List[str]:
    parts = re.split(r"[.\n]+", text)
    return [p.strip() for p in parts if p.strip()]

def guess_lex(tk: Token) -> List[LexEntry]:
    w = tk.text
    out: List[LexEntry] = []

    # N propio por capitalización (Kuhn/Kant/Foucault)
    if tk.raw[:1].isupper() and w not in ("el","la","los","las"):
        out.append(LexEntry("PropN", {"num":"sg"}, 0.03))

    if w.endswith("mente"):
        out.append(LexEntry("Adv", {}, -0.03))

    # infinitivo/gerundio => no finito
    if w.endswith(("ar","er","ir")):
        out.append(LexEntry("Vi", Vfeats("no","no"), -0.12))
        out.append(LexEntry("Vt", Vfeats("no","no"), -0.14))
    if w.endswith(("ando","iendo")):
        out.append(LexEntry("Vi", Vfeats("no","no"), -0.14))
        out.append(LexEntry("Vt", Vfeats("no","no"), -0.16))

    if not out:
        out.append(LexEntry("N", {"gen":"?g","num":"?n"}, -0.35))
    return out

# ============================================================
# Etapa 7: instrumentación
# ============================================================

@dataclass
class ParseStats:
    sentence: str
    tokens: int
    oov_tokens: int
    parsed: bool
    n_parses_returned: int
    best_score: Optional[float]
    time_ms: float

    # complejidad práctica / chart
    chart_items_total: int
    chart_items_max_cell: int
    pruned_by_beam: int
    unary_applications: int

    # ambigüedad “práctica”
    ambiguous_cells: int  # cuántas celdas terminaron con >=2 categorías
    final_cell_categories: List[str]

    # sanity checks (True = OK)
    sanity_S_has_VP_FIN: bool
    sanity_sin_takes_VP_NF: bool
    sanity_enclitic_only_nf: bool

    notes: List[str]

# ============================================================
# CKY con beam/top-k + stats
# ============================================================

@dataclass(frozen=True)
class Item:
    cat: str
    feats: Tuple[Tuple[str, str], ...]
    node: Node
    score: float

def keep_topk(items: List[Item], k: int) -> List[Item]:
    items.sort(key=lambda it: it.score, reverse=True)
    return items[:k]

def walk(node: Node):
    yield node
    for ch in node.children:
        yield from walk(ch)

def sanity_checks(tree: Node) -> Tuple[bool, bool, bool]:
    """
    Chequeos estructurales (no perfectos, pero útiles):
    1) Si hay S, debería tener VP_FIN en alguna rama (por nuestra gramática).
    2) Si aparece "sin" (Pinf), debe dominar un VP_NF.
    3) Enclítico “Vt_NF + Cl” o “Vi_NF + Cl” (aprox): si hay VP cuya hoja sea clítico,
       debería haber evidencia de fin=no en el verbo.
    """
    has_vpfin_under_s = False
    sin_ok = True
    encl_ok = True

    for n in walk(tree):
        if n.label == "S":
            if any(ch.label == "VP_FIN" for ch in n.children):
                has_vpfin_under_s = True

        if n.label == "Pinf":
            # buscamos un VP_NF en descendencia
            found = any(d.label == "VP_NF" for d in walk(n))
            if not found:
                sin_ok = False

        # chequeo enclítico rudimentario: si vemos patrón VP -> Vt_NF + Cl
        if n.label == "VP" and len(n.children) == 2:
            a, b = n.children
            if b.label == "Cl" and a.label in ("Vt_NF","Vi_NF"):
                # OK
                pass
            elif b.label == "Cl" and a.label in ("Vt","Vi"):
                encl_ok = False

    return has_vpfin_under_s, sin_ok, encl_ok

def parse_sentence(text: str, start_symbol: str = "S", top_k: int = 1, beam: int = 16, return_stats: bool = False):
    t0 = time.perf_counter()

    toks = tokenize(text)
    n = len(toks)
    if n == 0:
        if return_stats:
            return [], ParseStats(
                sentence=text, tokens=0, oov_tokens=0, parsed=False,
                n_parses_returned=0, best_score=None, time_ms=0.0,
                chart_items_total=0, chart_items_max_cell=0, pruned_by_beam=0,
                unary_applications=0, ambiguous_cells=0, final_cell_categories=[],
                sanity_S_has_VP_FIN=True, sanity_sin_takes_VP_NF=True, sanity_enclitic_only_nf=True,
                notes=["empty"]
            )
        return []

    chart: List[List[Dict[str, List[Item]]]] = [[dict() for _ in range(n+1)] for _ in range(n)]
    seen:  List[List[Dict[str, Set[Tuple[str, Tuple[Tuple[str,str],...], Node]]]]] = [[dict() for _ in range(n+1)] for _ in range(n)]

    pruned = 0
    unary_apps = 0

    def add(i: int, j: int, item: Item):
        nonlocal pruned
        key = (item.cat, item.feats, item.node)
        cell_seen = seen[i][j].setdefault(item.cat, set())
        if key in cell_seen:
            return
        cell_seen.add(key)

        cell = chart[i][j]
        cell.setdefault(item.cat, []).append(item)
        before = len(cell[item.cat])
        cell[item.cat] = keep_topk(cell[item.cat], beam)
        after = len(cell[item.cat])
        if after < before:
            pruned += (before - after)

    def unary_closure(i: int, j: int):
        nonlocal unary_apps
        changed = True
        while changed:
            changed = False
            cell = chart[i][j]
            for r in UNARY:
                (B,) = r.rhs
                if B not in cell:
                    continue
                for child in list(cell[B]):
                    fc = tuple_to_feats(child.feats)
                    pf = r.compose(fc, {})
                    if pf is None:
                        continue
                    unary_apps += 1
                    score = child.score + r.weight
                    node = Node(r.lhs, (child.node,), feats_to_tuple(pf), score)
                    before = len(cell.get(r.lhs, []))
                    add(i, j, Item(r.lhs, feats_to_tuple(pf), node, score))
                    after = len(cell.get(r.lhs, []))
                    if after != before:
                        changed = True

    # Lex init + OOV count
    oov_count = 0
    for i, tk in enumerate(toks):
        lex = LEXICON.get(tk.text)
        if not lex:
            oov_count += 1
            lex = []
        lex = lex + guess_lex(tk)

        for le in lex:
            feats = dict(le.feats)
            if le.pos in ("N","PropN","Pron"):
                feats.setdefault("idx", f"t{tk.i}")
            leaf = Node(tk.raw, (), (), le.weight)
            pre  = Node(le.pos, (leaf,), feats_to_tuple(feats), le.weight)
            add(i, i+1, Item(le.pos, feats_to_tuple(feats), pre, le.weight))

        unary_closure(i, i+1)

    # CKY
    for span in range(2, n+1):
        for i in range(0, n - span + 1):
            j = i + span
            for k in range(i+1, j):
                left  = chart[i][k]
                right = chart[k][j]
                if not left or not right:
                    continue

                for r in BINARY:
                    B, C = r.rhs
                    if B not in left or C not in right:
                        continue

                    for ib in left[B]:
                        fb = tuple_to_feats(ib.feats)
                        for ic in right[C]:
                            fc = tuple_to_feats(ic.feats)
                            pf = r.compose(fb, fc)
                            if pf is None:
                                continue

                            score = ib.score + ic.score + r.weight
                            L = ib.node
                            R = ic.node

                            # Propagación idx al adjuntar relativa
                            if r.lhs == "NBar" and r.rhs == ("NBar","RelClause"):
                                idx = tuple_to_feats(ib.feats).get("idx")
                                if idx:
                                    R = replace_feat_values(R, {"?i": idx})

                            node = Node(r.lhs, (L, R), feats_to_tuple(pf), score)
                            add(i, j, Item(r.lhs, feats_to_tuple(pf), node, score))

            unary_closure(i, j)

    results = keep_topk(chart[0][n].get(start_symbol, []), top_k)
    trees = [it.node for it in results]

    t1 = time.perf_counter()

    if not return_stats:
        return trees

    # métricas de chart
    total_items = 0
    max_cell = 0
    ambiguous_cells = 0
    for i in range(n):
        for j in range(i+1, n+1):
            cell = chart[i][j]
            cats = list(cell.keys())
            if len(cats) >= 2:
                ambiguous_cells += 1
            cell_count = sum(len(v) for v in cell.values())
            total_items += cell_count
            if cell_count > max_cell:
                max_cell = cell_count

    final_cats = sorted(chart[0][n].keys())
    parsed = len(trees) > 0
    best_score = trees[0].score if parsed else None

    # sanity checks (si no parseó, los damos como True para no contaminar)
    s_ok, sin_ok, encl_ok = (True, True, True)
    notes: List[str] = []
    if parsed:
        s_ok, sin_ok, encl_ok = sanity_checks(trees[0])
        if not s_ok:
            notes.append("WARN: S sin VP_FIN visible")
        if not sin_ok:
            notes.append("WARN: 'sin' sin VP_NF bajo Pinf")
        if not encl_ok:
            notes.append("WARN: enclítico con verbo finito")
    else:
        notes.append("NO_PARSE")
        if final_cats:
            notes.append("final_cell=" + ",".join(final_cats))

    stats = ParseStats(
        sentence=text,
        tokens=n,
        oov_tokens=oov_count,
        parsed=parsed,
        n_parses_returned=len(trees),
        best_score=best_score,
        time_ms=(t1 - t0) * 1000.0,
        chart_items_total=total_items,
        chart_items_max_cell=max_cell,
        pruned_by_beam=pruned,
        unary_applications=unary_apps,
        ambiguous_cells=ambiguous_cells,
        final_cell_categories=final_cats,
        sanity_S_has_VP_FIN=s_ok,
        sanity_sin_takes_VP_NF=sin_ok,
        sanity_enclitic_only_nf=encl_ok,
        notes=notes
    )

    return trees, stats

# ============================================================
# Etapa 7: evaluación de corpus + salida
# ============================================================

def evaluate_corpus(text: str, top_k: int = 1, beam: int = 16, show_trees: bool = False) -> Dict[str, Any]:
    sents = split_sentences(text)
    rows: List[ParseStats] = []
    parsed_count = 0
    total_time = 0.0
    total_tokens = 0
    total_oov = 0

    for s in sents:
        trees, st = parse_sentence(s, top_k=top_k, beam=beam, return_stats=True)
        rows.append(st)
        parsed_count += 1 if st.parsed else 0
        total_time += st.time_ms
        total_tokens += st.tokens
        total_oov += st.oov_tokens

        print("="*78)
        print(s)
        print(f"tokens={st.tokens}  oov={st.oov_tokens}  parsed={st.parsed}  "
              f"parses={st.n_parses_returned}  score={st.best_score}  time_ms={st.time_ms:.1f}")
        print(f"chart_items={st.chart_items_total}  max_cell={st.chart_items_max_cell}  "
              f"pruned={st.pruned_by_beam}  unary_apps={st.unary_applications}  "
              f"amb_cells={st.ambiguous_cells}")
        if st.notes:
            print("notes:", "; ".join(st.notes))
        if show_trees and trees:
            print(trees[0].pretty())

    coverage = parsed_count / max(1, len(sents))
    summary = {
        "sentences": len(sents),
        "coverage": coverage,
        "avg_tokens": total_tokens / max(1, len(sents)),
        "avg_oov": total_oov / max(1, len(sents)),
        "total_time_ms": total_time,
        "avg_time_ms": total_time / max(1, len(sents)),
        "beam": beam,
        "top_k": top_k,
        "rows": [st.__dict__ for st in rows],
    }

    print("\n" + "#"*78)
    print("SUMMARY")
    print(f"sentences={summary['sentences']}  coverage={summary['coverage']:.3f}  "
          f"avg_tokens={summary['avg_tokens']:.2f}  avg_oov={summary['avg_oov']:.2f}  "
          f"avg_time_ms={summary['avg_time_ms']:.1f}")
    return summary

# ============================================================
# Demo
# ============================================================

if __name__ == "__main__":
    corpus = """El filósofo que escribió el tratado murió en el exilio.
La teoría que Kuhn propuso transformó la epistemología contemporánea.
Los científicos lo estudiaron durante décadas sin comprenderlo.
El paradigma que dominaba la física colapsó repentinamente.
La evidencia le sugiere al investigador una hipótesis alternativa.
El manuscrito que descubrieron contiene anotaciones marginales extensas.
La revolución industrial transformó las estructuras sociales que prevalecían.
El argumento que desarrolla el autor lo refuta en capítulos posteriores.
Las consecuencias que previeron los economistas nunca se materializaron.
El fenómeno emerge espontáneamente en sistemas complejos.
La crítica que formularon los empiristas le pareció insuficiente a Kant.
El concepto que introduce Foucault desestabiliza las categorías tradicionales."""
    summary = evaluate_corpus(corpus, top_k=1, beam=16, show_trees=False)

    # si querés exportar JSON:
    # with open("eval_summary.json", "w", encoding="utf-8") as f:
    #     json.dump(summary, f, ensure_ascii=False, indent=2)
