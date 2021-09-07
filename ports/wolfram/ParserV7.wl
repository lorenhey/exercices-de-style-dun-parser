(* ParserV7.wl
   CKY + beam + unary closure + features + constraints
   + incremental real: ResetIncremental[], Step[token], BestTree[]
*)

BeginPackage["ParserV7`"];

CreateParser::usage = "CreateParser[grammarPath, lexiconPath, beam] crea un parser.";
ParseSentence::usage = "ParseSentence[p, sentence] parsea batch y devuelve un árbol bracketed o \"\".";
ResetIncremental::usage = "ResetIncremental[p] resetea el estado incremental.";
Step::usage = "Step[p, token] agrega token(s) y devuelve el mejor árbol del prefijo (o \"\").";
BestTree::usage = "BestTree[p] devuelve el mejor árbol del prefijo actual (o \"\").";
ParseSentenceIncremental::usage = "ParseSentenceIncremental[p, sentence] usa Step internamente y devuelve el árbol final.";

Begin["`Private`"];

(* ---------------------------------------
   Helpers: JSON / log
---------------------------------------- *)
ReadJSON[path_] := Import[path, "RawJSON"];

SafeLog[w_] := Log[Max[10^-12, N[w]]];

(* ---------------------------------------
   Symbol table
---------------------------------------- *)
Intern[p_, s_] := Module[{key},
  key = ToUpperCase@StringTrim@ToString@s;
  If[key === "", 0,
    If[KeyExistsQ[p["sym2id"], key],
      p["sym2id"][key],
      p["nextSymId"]++;
      p["sym2id"][key] = p["nextSymId"];
      p["id2sym"][p["nextSymId"]] = key;
      p["nextSymId"]
    ]
  ]
];

SymStr[p_, id_] := Lookup[p["id2sym"], id, "<?>"];

(* ---------------------------------------
   Feature utilities
---------------------------------------- *)
FeatsFromEntry[p_, e_] := Module[{raw, feats = <||>, k, v},
  raw = Lookup[e, "feats", Lookup[e, "features", Missing[]]];
  If[raw === Missing[], Return[{feats, ""}]];

  Which[
    AssociationQ[raw],
      Do[
        k = Intern[p, kk];
        v = Intern[p, ToString[raw[kk]]];
        If[k != 0 && v != 0, feats[k] = v],
      {kk, Keys[raw]}],

    ListQ[raw],
      Do[
        If[AssociationQ[item] && KeyExistsQ[item, "k"] && KeyExistsQ[item, "v"],
          k = Intern[p, item["k"]];
          v = Intern[p, item["v"]];
          If[k != 0 && v != 0, feats[k] = v]
        ],
      {item, raw}],

    True, Null
  ];

  {KeySort@feats, FeatSig[feats]}
];

FeatSig[feats_Association] := Module[{keys},
  keys = Sort[Keys[feats]];
  StringRiffle[(ToString[#] <> ":" <> ToString[feats[#]]) & /@ keys, ","]
];

(* ---------------------------------------
   Chart cell / beam
---------------------------------------- *)
InitCell[] := <|"items" -> {}, "byKey" -> <||>|>;

SortTrim[p_, cell_] := Module[{items = cell["items"], beam = p["beam"], byKey = <||>, i, it, key},
  items = Reverse@SortBy[items, #["score"] &];
  If[Length[items] > beam, items = Take[items, beam]];
  Do[
    it = items[[i]];
    key = ToString[it["cat"]] <> "|" <> it["sig"];
    byKey[key] = i,
  {i, Length[items]}];
  cell["items"] = items;
  cell["byKey"] = byKey;
  cell
];

CellInsert[p_, cell_, it_] := Module[{key, idx, items},
  key = ToString[it["cat"]] <> "|" <> it["sig"];
  If[KeyExistsQ[cell["byKey"], key],
    idx = cell["byKey"][key];
    If[it["score"] > cell["items"][[idx]]["score"], cell["items"][[idx]] = it];
    Return[SortTrim[p, cell]];
  ];
  cell["items"] = Append[cell["items"], it];
  SortTrim[p, cell]
];

MakeItem[p_, cat_, score_, node_, feats_Association] := <|
  "cat" -> cat,
  "score" -> N[score],
  "node" -> node,
  "feats" -> KeySort@feats,
  "sig" -> FeatSig[feats]
|>;

(* ---------------------------------------
   Arena nodes
---------------------------------------- *)
AddLeafNode[p_, label_, leaf_] := Module[{id},
  id = Length[p["arena"]] + 1;
  p["arena"][id] = <|"label" -> label, "left" -> 0, "right" -> 0, "isLeaf" -> True, "leaf" -> leaf|>;
  id
];

AddUnaryNode[p_, label_, child_] := Module[{id},
  id = Length[p["arena"]] + 1;
  p["arena"][id] = <|"label" -> label, "left" -> child, "right" -> 0, "isLeaf" -> False, "leaf" -> ""|>;
  id
];

AddBinaryNode[p_, label_, l_, r_] := Module[{id},
  id = Length[p["arena"]] + 1;
  p["arena"][id] = <|"label" -> label, "left" -> l, "right" -> r, "isLeaf" -> False, "leaf" -> ""|>;
  id
];

RenderTree[p_, node_] := Module[{n, lab, a, b},
  n = p["arena"][node];
  lab = SymStr[p, n["label"]];
  If[TrueQ[n["isLeaf"]],
    Return["(" <> lab <> " " <> n["leaf"] <> ")"]
  ];
  If[n["right"] == 0,
    a = RenderTree[p, n["left"]];
    Return["(" <> lab <> " " <> a <> ")"]
  ];
  a = RenderTree[p, n["left"]];
  b = RenderTree[p, n["right"]];
  "(" <> lab <> " " <> a <> " " <> b <> ")"
];

(* ---------------------------------------
   Tokenization + morphology
---------------------------------------- *)
Tokenize[s_] := Module[{m},
  m = StringCases[ToString@s, RegularExpression["[\\p{L}\\p{N}_]+"]];
  m
];

MaybeSplitEnclitic[tok_] := Module[{encl, suf, base, last},
  encl = {"me","te","se","lo","la","los","las","le","les","nos","os"};
  Do[
    suf = encl[[i]];
    If[StringLength[tok] > StringLength[suf] + 2 && StringEndsQ[tok, suf],
      base = StringDrop[tok, -StringLength[suf]];
      last = StringTake[base, -1];
      If[MemberQ[{"r","d","n"}, last], Return[{base, suf}]]
    ],
  {i, Length[encl]}];
  {tok, ""}
];

SplitMorphology[toks_List] := Module[{out = {}, t, ms},
  Do[
    t = ToLowerCase@toks[[i]];
    If[t === "al", out = Join[out, {"a","el"}]; Continue[]];
    If[t === "del", out = Join[out, {"de","el"}]; Continue[]];
    ms = MaybeSplitEnclitic[t];
    If[ms[[2]] =!= "", out = Join[out, ms], out = Append[out, t]],
  {i, Length[toks]}];
  out
];

(* ---------------------------------------
   Load lexicon / grammar
---------------------------------------- *)
LoadLexicon[p_, l_] := Module[{entries, e, word, pos, w, feats, sig, entry, bucket},
  p["lexByWord"] = <||>;
  entries = Lookup[l, "entries", Lookup[l, "lexicon", {}]];
  If[!ListQ[entries], entries = {}];

  Do[
    e = entries[[i]];
    If[!AssociationQ[e], Continue[]];
    word = ToLowerCase@ToString[Lookup[e, "word", Lookup[e, "form", ""]]];
    If[word === "", Continue[]];
    pos  = Intern[p, Lookup[e, "pos", Lookup[e, "tag", "X"]]];
    w    = Lookup[e, "weight", Lookup[e, "prob", 1.0]];
    {feats, sig} = FeatsFromEntry[p, e];
    entry = <|"word" -> word, "pos" -> pos, "logw" -> SafeLog[w], "feats" -> feats, "sig" -> sig|>;

    bucket = Lookup[p["lexByWord"], word, {}];
    p["lexByWord"][word] = Append[bucket, entry],
  {i, Length[entries]}];
];

LoadGrammar[p_, g_] := Module[{rules, r, lhs, rhs, rhsArr, rhsIds, w, prop, csRaw, cs, c},
  p["rules"] = {};
  rules = Lookup[g, "rules", Lookup[g, "productions", {}]];
  If[!ListQ[rules], rules = {}];

  Do[
    r = rules[[i]];
    If[!AssociationQ[r], Continue[]];
    lhs = Intern[p, Lookup[r, "lhs", "<?>"]];

    rhs = Lookup[r, "rhs", Lookup[r, "rhs_symbols", Missing[]]];
    rhsArr =
      If[ListQ[rhs], ToString /@ rhs,
        DeleteCases[
          {Lookup[r, "rhs1", ""], Lookup[r, "rhs2", ""]} /. Missing[] -> "",
          "" | " "
        ] /. x_ :> ToString[x]
      ];

    If[Length[rhsArr] < 1 || Length[rhsArr] > 2, Continue[]];
    rhsIds = Intern[p, #] & /@ rhsArr;

    w = Lookup[r, "weight", Lookup[r, "prob", 1.0]];
    prop = ToUpperCase@ToString[Lookup[r, "propagate", "MERGE"]];
    If[prop === "", prop = "MERGE"];

    csRaw = Lookup[r, "constraints", Lookup[r, "conds", {}]];
    cs = {};
    If[ListQ[csRaw],
      Do[
        c = csRaw[[j]];
        If[AssociationQ[c],
          cs = Append[cs, <|
            "type" -> ToUpperCase@ToString[Lookup[c, "type", "REQUIRE"]],
            "target" -> ToUpperCase@ToString[Lookup[c, "target", "LEFT"]],
            "target2" -> ToUpperCase@ToString[Lookup[c, "target2", Lookup[c, "other", "RIGHT"]]],
            "key" -> Intern[p, Lookup[c, "key", ""]],
            "val" -> Intern[p, Lookup[c, "value", ""]],
            "key2" -> Intern[p, Lookup[c, "key2", ""]]
          |>]
        ],
      {j, Length[csRaw]}]
    ];

    p["rules"] = Append[p["rules"], <|
      "lhs" -> lhs,
      "rhsLen" -> Length[rhsIds],
      "rhs1" -> rhsIds[[1]],
      "rhs2" -> If[Length[rhsIds] == 2, rhsIds[[2]], 0],
      "logw" -> SafeLog[w],
      "prop" -> prop,
      "cs" -> cs
    |>],
  {i, Length[rules]}];
];

BuildRuleIndex[p_] := Module[{i, r, k},
  p["unaryIndex"] = <||>;
  p["binaryIndex"] = <||>;
  Do[
    r = p["rules"][[i]];
    If[r["rhsLen"] == 1,
      k = ToString[r["rhs1"]];
      p["unaryIndex"][k] = Append[Lookup[p["unaryIndex"], k, {}], i],
      k = ToString[r["rhs1"]] <> "," <> ToString[r["rhs2"]];
      p["binaryIndex"][k] = Append[Lookup[p["binaryIndex"], k, {}], i]
    ],
  {i, Length[p["rules"]]}];
];

(* ---------------------------------------
   Constraints
---------------------------------------- *)
ApplyUnaryConstraints[p_, rule_, childFeats_Association] := Module[{out = childFeats, c, typ, key, val, v},
  Do[
    c = rule["cs"][[i]];
    typ = c["type"];
    If[typ === "REQUIRE",
      key = c["key"]; val = c["val"];
      v = Lookup[out, key, 0];
      If[v =!= val, Return[{False, out}]],
      If[typ === "ASSIGN",
        If[c["key"] != 0 && c["val"] != 0, out[c["key"]] = c["val"]]
      ]
    ],
  {i, Length[rule["cs"]]}];
  {True, KeySort@out}
];

MergeFeats[lf_Association, rf_Association] := Module[{out = lf, k},
  Do[
    k = Keys[rf][[i]];
    If[!KeyExistsQ[out, k], out[k] = rf[k]],
  {i, Length[Keys[rf]]}];
  KeySort@out
];

ApplyBinaryConstraints[p_, rule_, lf_Association, rf_Association] := Module[
  {out, prop = rule["prop"], c, typ, k, v1, v2, src, v},
  out =
    Which[
      prop === "LEFT", lf,
      prop === "RIGHT", rf,
      True, MergeFeats[lf, rf]
    ];

  Do[
    c = rule["cs"][[i]];
    typ = c["type"];

    Which[
      typ === "REQUIRE",
        src = If[c["target"] === "RIGHT", rf, lf];
        v = Lookup[src, c["key"], 0];
        If[v =!= c["val"], Return[{False, out}]],

      typ === "UNIFY",
        k = c["key"];
        v1 = Lookup[lf, k, 0]; v2 = Lookup[rf, k, 0];
        If[v1 != 0 && v2 != 0 && v1 =!= v2, Return[{False, out}]];
        If[v1 != 0, out[k] = v1, If[v2 != 0, out[k] = v2]],

      typ === "AGREE",
        k = c["key"];
        v1 = Lookup[lf, k, 0]; v2 = Lookup[rf, k, 0];
        If[v1 == 0 || v2 == 0 || v1 =!= v2, Return[{False, out}]];
        out[k] = v1,

      typ === "ASSIGN",
        If[c["key"] != 0 && c["val"] != 0, out[c["key"]] = c["val"]],

      True, Null
    ],
  {i, Length[rule["cs"]]}];

  {True, KeySort@out}
];

(* ---------------------------------------
   Lexical seeding
---------------------------------------- *)
SeedLexical[p_, cell_, word_] := Module[{wSurface, w, entries, cat, node, it},
  wSurface = ToString[word];
  w = ToLowerCase@wSurface;
  entries = Lookup[p["lexByWord"], w, {}];

  If[Length[entries] > 0,
    Do[
      node = AddLeafNode[p, entries[[i]]["pos"], wSurface];
      it = MakeItem[p, entries[[i]]["pos"], entries[[i]]["logw"], node, entries[[i]]["feats"]];
      cell = CellInsert[p, cell, it],
    {i, Length[entries]}];
    Return[cell];
  ];

  (* OOV fallback: mayúscula inicial => PROPN *)
  cat = Intern[p, If[StringMatchQ[StringTake[wSurface, 1], RegularExpression["\\p{Lu}"]], "PROPN", "NOUN"]];
  node = AddLeafNode[p, cat, wSurface];
  it = MakeItem[p, cat, Log[10^-6], node, <||>];
  CellInsert[p, cell, it]
];

(* ---------------------------------------
   Unary closure
---------------------------------------- *)
UnaryClosure[p_, cell_] := Module[{changed = True, iter = 0, snapshot, src, idxs, rule, res, node, it, before},
  While[changed && iter < 64,
    iter++;
    changed = False;
    snapshot = cell["items"];
    Do[
      src = snapshot[[i]];
      If[!KeyExistsQ[p["unaryIndex"], ToString[src["cat"]]], Continue[]];
      idxs = p["unaryIndex"][ToString[src["cat"]]];
      Do[
        rule = p["rules"][[ri]];
        res = ApplyUnaryConstraints[p, rule, src["feats"]];
        If[!TrueQ[res[[1]]], Continue[]];
        node = AddUnaryNode[p, rule["lhs"], src["node"]];
        it = MakeItem[p, rule["lhs"], src["score"] + rule["logw"], node, res[[2]]];
        before = Length[cell["items"]];
        cell = CellInsert[p, cell, it];
        If[Length[cell["items"]] > before, changed = True],
      {ri, idxs}],
    {i, Length[snapshot]}];
  ];
  cell
];

(* ---------------------------------------
   Combine cells
---------------------------------------- *)
CombineCells[p_, outCell_, leftCell_, rightCell_] := Module[{L, R, key, idxs, rule, res, node, it},
  Do[
    L = leftCell["items"][[i]];
    Do[
      R = rightCell["items"][[j]];
      key = ToString[L["cat"]] <> "," <> ToString[R["cat"]];
      If[!KeyExistsQ[p["binaryIndex"], key], Continue[]];
      idxs = p["binaryIndex"][key];
      Do[
        rule = p["rules"][[ri]];
        res = ApplyBinaryConstraints[p, rule, L["feats"], R["feats"]];
        If[!TrueQ[res[[1]]], Continue[]];
        node = AddBinaryNode[p, rule["lhs"], L["node"], R["node"]];
        it = MakeItem[p, rule["lhs"], L["score"] + R["score"] + rule["logw"], node, res[[2]]];
        outCell = CellInsert[p, outCell, it],
      {ri, idxs}],
    {j, Length[rightCell["items"]]}],
  {i, Length[leftCell["items"]]}];
  outCell
];

(* ---------------------------------------
   Root pick
---------------------------------------- *)
PickBestRoot[p_, cell_] := Module[{best = None, it},
  If[cell === Null || Length[cell["items"]] == 0, Return[None]];
  Do[
    it = cell["items"][[i]];
    If[it["cat"] === p["startSym"],
      If[best === None || it["score"] > best["score"], best = it]
    ],
  {i, Length[cell["items"]]}];
  best
];

(* ---------------------------------------
   Parser object creation
---------------------------------------- *)
CreateParser[grammarPath_, lexiconPath_, beam_: 8] := Module[
  {p, g, l, start},
  p = <|
    "beam" -> beam,
    "sym2id" -> <||>,
    "id2sym" -> <||>,
    "nextSymId" -> 0,
    "startSym" -> 0,
    "lexByWord" -> <||>,
    "rules" -> {},
    "unaryIndex" -> <||>,
    "binaryIndex" -> <||>,
    "arena" -> <||>,
    "incTokens" -> {},
    "incChart" -> {}
  |>;

  g = ReadJSON[grammarPath];
  l = ReadJSON[lexiconPath];
  start = Lookup[g, "start", Lookup[g, "root", Lookup[g, "start_symbol", "S"]]];
  p["startSym"] = Intern[p, start];

  LoadLexicon[p, l];
  LoadGrammar[p, g];
  BuildRuleIndex[p];

  (* reset incremental arena/chart *)
  ResetIncremental[p];
  p
];

(* ---------------------------------------
   Batch parsing
---------------------------------------- *)
ParseSentence[p_, sentence_] := Module[{toks0, toks, n, chart, i, cell, span, j, k, left, right, best},
  toks0 = Tokenize[sentence];
  toks = SplitMorphology[toks0];
  n = Length[toks];
  If[n == 0, Return[""]];

  p["arena"] = <||>;  (* reset arena *)
  (* chart as association of rows: chart[i][j] *)
  chart = Table[Association[], {n}];

  (* seed *)
  Do[
    cell = InitCell[];
    cell = SeedLexical[p, cell, toks[[i]]];
    cell = UnaryClosure[p, cell];
    chart[[i]][i + 1] = cell,
  {i, n}];

  (* CKY *)
  Do[
    Do[
      j = i + span;
      cell = InitCell[];
      Do[
        left = Lookup[chart[[i]], k, Null];
        right = Lookup[chart[[k]], j, Null];
        If[left === Null || right === Null, Continue[]];
        cell = CombineCells[p, cell, left, right],
      {k, i + 1, j - 1}];
      cell = UnaryClosure[p, cell];
      chart[[i]][j] = cell,
    {i, 1, n - span + 1}],
  {span, 2, n}];

  best = PickBestRoot[p, Lookup[chart[[1]], n + 1, Null]];
  If[best === None, "", RenderTree[p, best["node"]]]
];

(* ---------------------------------------
   Incremental real
---------------------------------------- *)
ResetIncremental[p_] := Module[{},
  p["incTokens"] = {};
  p["incChart"] = {};
  p["arena"] = <||>;
  p
];

BestTree[p_] := Module[{n, cell, best},
  n = Length[p["incTokens"]];
  If[n == 0, Return[""]];
  cell = Lookup[Lookup[p["incChart"], 1, <||>], n + 1, Null];
  best = PickBestRoot[p, cell];
  If[best === None, "", RenderTree[p, best["node"]]]
];

StepOne[p_, tok_] := Module[{n, j, iNew, cell, i, cellIJ, k, left, right},
  p["incTokens"] = Append[p["incTokens"], tok];
  n = Length[p["incTokens"]];
  j = n + 1;
  iNew = n;

  (* ensure row for iNew *)
  If[!KeyExistsQ[p["incChart"], iNew], p["incChart"][iNew] = <||>];

  (* lexical cell [n, n+1) *)
  cell = InitCell[];
  cell = SeedLexical[p, cell, tok];
  cell = UnaryClosure[p, cell];
  p["incChart"][iNew][j] = cell;

  (* build spans ending at j: i = n-1 .. 1 *)
  Do[
    If[i < 1, Continue[]];
    If[!KeyExistsQ[p["incChart"], i], p["incChart"][i] = <||>];
    cellIJ = InitCell[];

    Do[
      left  = Lookup[p["incChart"][i], k, Null];
      right = Lookup[p["incChart"][k], j, Null];
      If[left === Null || right === Null, Continue[]];
      cellIJ = CombineCells[p, cellIJ, left, right],
    {k, i + 1, j - 1}];

    cellIJ = UnaryClosure[p, cellIJ];
    p["incChart"][i][j] = cellIJ,
  {i, n - 1, 1, -1}];

  p
];

Step[p_, token_] := Module[{parts, toks},
  parts = Tokenize[token];
  If[Length[parts] == 0, Return[BestTree[p]]];
  toks = SplitMorphology[parts];
  Do[StepOne[p, toks[[i]]], {i, Length[toks]}];
  BestTree[p]
];

ParseSentenceIncremental[p_, sentence_] := Module[{toks0, toks, i},
  ResetIncremental[p];
  toks0 = Tokenize[sentence];
  toks = SplitMorphology[toks0];
  Do[StepOne[p, toks[[i]]], {i, Length[toks]}];
  BestTree[p]
];

End[];
EndPackage[];
