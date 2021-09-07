(* ShortCode1952v2.wl
   VM "Short-Code-like" (opcodes 2 letras) con memoria numérica + subrutinas.
   Incluye:
   - LoadProgram / Run
   - EmitDeckFromResources (convierte grammar.json/lexicon.json + sentence -> data deck .sc)
   - DecodeTree (reconstruye árbol desde memoria usando backpointers)
*)

BeginPackage["ShortCode1952v2`"];

LoadProgram::usage = "LoadProgram[path] carga un .sc, resuelve labels y devuelve VM.";
Run::usage = "Run[vm, opts] ejecuta. opts: \"MemSize\"->int, \"Trace\"->False.";
EmitDeckFromResources::usage =
  "EmitDeckFromResources[grammarPath, lexiconPath, sentence, outDeckPath, opts] escribe un .sc con DM (data deck).";
DecodeTree::usage =
  "DecodeTree[state, meta] reconstruye un árbol bracketed desde memoria. meta sale de EmitDeckFromResources.";

Begin["`Private`"];

(* ---------------------------
   Utils
--------------------------- *)
ToNum[x_] := Module[{n},
  Quiet@Check[n = ToExpression[x], n = 0];
  N @ If[NumberQ[n], n, 0]
];

IsReg[s_] := StringMatchQ[s, RegularExpression["R\\d+"]];

RegIndex[s_] := ToExpression @ StringDrop[s, 1];

(* tokenización de líneas .sc: soporta strings "..." y tokens sueltos *)
TrimComment[line_] := StringTrim @ StringSplit[line, ";", 2][[1]];

TokenizeLine[line_] := Module[{s = TrimComment[line]},
  If[s === "", {},
    StringCases[s, RegularExpression["\"([^\"]*)\"|\\S+"] :>
      (StringReplace[#, RegularExpression["^\"|\"$"] -> ""] &)
    ]
  ]
];

(* ---------------------------
   Assembler
--------------------------- *)
ParseProgram[path_] := Module[{lines, prog = {}, labels = <||>, toks},
  lines = ReadList[path, String, CharacterEncoding -> "UTF-8"];
  Do[
    toks = TokenizeLine[lines[[i]]];
    If[toks === {}, Continue[]];
    If[ToUpperCase@toks[[1]] === "LB",
      labels[toks[[2]]] = Length[prog] + 1; (* la próxima instrucción *)
      Continue[];
    ];
    prog = Append[prog, toks],
  {i, Length[lines]}];
  <|"prog" -> prog, "labels" -> labels|>
];

LoadProgram[path_] := Module[{pp = ParseProgram[path]},
  <|
    "prog" -> pp["prog"],
    "labels" -> pp["labels"],
    "pc" -> 1,
    "halted" -> False,
    "out" -> {},
    "callstack" -> {},
    "regs" -> ConstantArray[0.0, 32],
    "mem" -> {}
  |>
];

(* ---------------------------
   Memory helpers (addr inmediato o registro)
--------------------------- *)
AddrValue[state_, a_] := If[IsReg[a],
  Round @ state["regs"][[RegIndex[a] + 1]],
  Round @ ToNum[a]
];

GetVal[state_, x_] := If[IsReg[x],
  state["regs"][[RegIndex[x] + 1]],
  ToNum[x]
];

MemGet[state_, addr_] := Module[{a = AddrValue[state, addr]},
  If[a < 1 || a > Length[state["mem"]], 0.0, state["mem"][[a]]]
];

MemSet[state_, addr_, v_] := Module[{a = AddrValue[state, addr]},
  If[a < 1, Return[state]];
  If[a > Length[state["mem"]],
    state["mem"] = PadRight[state["mem"], a, 0.0];
  ];
  state["mem"][[a]] = N[v];
  state
];

JumpTo[state_, label_] := Module[{pc2 = Lookup[state["labels"], label, Missing[]]},
  If[pc2 === Missing[], state, (state["pc"] = pc2; state)]
];

(* ---------------------------
   VM step
--------------------------- *)
StepVM[state_] := Module[
  {prog = state["prog"], pc = state["pc"], ins, op, rA, rB, rC, vB, vC, addr, trace = False},

  If[state["halted"], Return[state]];
  If[pc < 1 || pc > Length[prog], state["halted"] = True; Return[state]];

  ins = prog[[pc]];
  op = ToUpperCase @ ins[[1]];

  (* default: avanzar *)
  state["pc"] = pc + 1;

  Switch[op,

    "HL",
      state["halted"] = True,

    "LI",
      (* LI Rk imm *)
      rA = RegIndex[ins[[2]]] + 1;
      state["regs"][[rA]] = ToNum[ins[[3]]],

    "CP",
      (* CP Ra Rb/imm *)
      rA = RegIndex[ins[[2]]] + 1;
      state["regs"][[rA]] = GetVal[state, ins[[3]]],

    "LM",
      (* LM Ra addr  (addr puede ser número o registro) *)
      rA = RegIndex[ins[[2]]] + 1;
      state["regs"][[rA]] = MemGet[state, ins[[3]]],

    "SM",
      (* SM addr Rb/imm  (addr puede ser número o registro) *)
      addr = ins[[2]];
      vB = GetVal[state, ins[[3]]];
      state = MemSet[state, addr, vB],

    "DM",
      (* DM addr imm  (data-move inmediato; útil para el deck) *)
      state = MemSet[state, ins[[2]], ToNum[ins[[3]]]],

    "AD" | "SB" | "ML" | "DV",
      (* OP Ra x y *)
      rA = RegIndex[ins[[2]]] + 1;
      vB = GetVal[state, ins[[3]]];
      vC = GetVal[state, ins[[4]]];
      state["regs"][[rA]] = Switch[op,
        "AD", vB + vC,
        "SB", vB - vC,
        "ML", vB * vC,
        "DV", If[vC == 0, 0.0, vB / vC]
      ],

    "JM",
      state = JumpTo[state, ins[[2]]],

    "JE" | "JG" | "JL",
      (* JE x y label (numérico) *)
      vB = GetVal[state, ins[[2]]];
      vC = GetVal[state, ins[[3]]];
      If[
        Switch[op, "JE", vB == vC, "JG", vB > vC, "JL", vB < vC],
        state = JumpTo[state, ins[[4]]]
      ],

    "JS",
      (* call *)
      state["callstack"] = Append[state["callstack"], state["pc"]];
      state = JumpTo[state, ins[[2]]],

    "RT",
      If[Length[state["callstack"]] == 0,
        state["halted"] = True,
        state["pc"] = Last[state["callstack"]];
        state["callstack"] = Most[state["callstack"]];
      ],

    "PR",
      (* PR x *)
      state["out"] = Append[state["out"], ToString@GetVal[state, ins[[2]]]],

    _,
      state["out"] = Append[state["out"], "ERR opcode " <> op];
      state["halted"] = True
  ];

  state
];

Run[state_, opts : OptionsPattern[{"MemSize" -> 120000, "Trace" -> False}]] := Module[
  {st = state, memSize = OptionValue["MemSize"], trace = OptionValue["Trace"], steps = 0},
  If[st["mem"] === {} || Length[st["mem"]] < memSize, st["mem"] = ConstantArray[0.0, memSize]];
  While[!TrueQ[st["halted"]],
    st = StepVM[st];
    steps++;
    If[TrueQ[trace] && Mod[steps, 1000] == 0, Print["step=", steps, " pc=", st["pc"]]];
  ];
  st
];

(* ============================================================
   DECK COMPILER (recursos -> DM lines)
   “Manual conversion step” (muy 1950):
   - Nosotros lo automatizamos, pero podés considerarlo el “coder” que arma las tarjetas.
============================================================ *)

(* Intern table, igual idea que los ports: START primero, luego lexicon, luego grammar *)
MakeIntern[] := <|"sym2id" -> <||>, "id2sym" -> <||>, "next" -> 0|>;

Intern[in_, s_] := Module[{key = ToUpperCase@StringTrim@ToString@s},
  If[key === "", 0,
    If[KeyExistsQ[in["sym2id"], key],
      in["sym2id"][key],
      in["next"]++;
      in["sym2id"][key] = in["next"];
      in["id2sym"][in["next"]] = key;
      in["next"]
    ]
  ]
];

TokenizeWords[s_] := StringCases[ToString@s, RegularExpression["[\\p{L}\\p{N}_]+"]];
SplitMorphologyWL[toks_List] := Module[{out = {}, t, encl, suf, base, last},
  encl = {"me","te","se","lo","la","los","las","le","les","nos","os"};
  Do[
    t = ToLowerCase@toks[[i]];
    If[t === "al", out = Join[out, {"a","el"}]; Continue[]];
    If[t === "del", out = Join[out, {"de","el"}]; Continue[]];
    (* enclítico simple *)
    Module[{done = False},
      Do[
        suf = encl[[j]];
        If[StringLength[t] > StringLength[suf] + 2 && StringEndsQ[t, suf],
          base = StringDrop[t, -StringLength[suf]];
          last = StringTake[base, -1];
          If[MemberQ[{"r","d","n"}, last],
            out = Join[out, {base, suf}]; done = True; Break[];
          ];
        ],
      {j, Length[encl]}];
      If[!done, out = Append[out, t]];
    ],
  {i, Length[toks]}];
  out
];

ReadJSON[path_] := Import[path, "RawJSON"];

(* Lexicon map word->best POS (máx weight) *)
BuildLexMap[in_, lex_] := Module[{entries, m = <||>, e, w, pos, wt},
  entries = Lookup[lex, "entries", Lookup[lex, "lexicon", {}]];
  If[!ListQ[entries], entries = {}];
  Do[
    e = entries[[i]];
    If[!AssociationQ[e], Continue[]];
    w = ToLowerCase@ToString[Lookup[e, "word", Lookup[e, "form", ""]]];
    If[w === "", Continue[]];
    pos = Intern[in, Lookup[e, "pos", Lookup[e, "tag", "X"]]];
    wt = N @ Lookup[e, "weight", Lookup[e, "prob", 1.0]];
    If[!KeyExistsQ[m, w] || wt > m[w, "wt"],
      m[w] = <|"pos" -> pos, "wt" -> wt|>
    ],
  {i, Length[entries]}];
  m
];

(* Grammar arrays (sin constraints para mantener el deck/programa acotado y “punch-card friendly”) *)
BuildRules[in_, g_] := Module[{rules, out = {}, r, lhs, rhs, rhsArr, rhsIds, w},
  rules = Lookup[g, "rules", Lookup[g, "productions", {}]];
  If[!ListQ[rules], rules = {}];

  Do[
    r = rules[[i]];
    If[!AssociationQ[r], Continue[]];
    lhs = Intern[in, Lookup[r, "lhs", "<?>"]];

    rhs = Lookup[r, "rhs", Lookup[r, "rhs_symbols", Missing[]]];
    rhsArr =
      If[ListQ[rhs], ToString /@ rhs,
        DeleteCases[{Lookup[r, "rhs1", ""], Lookup[r, "rhs2", ""]}, "" | " "]
      ];
    If[Length[rhsArr] < 1 || Length[rhsArr] > 2, Continue[]];

    rhsIds = Intern[in, #] & /@ rhsArr;
    w = N @ Lookup[r, "weight", Lookup[r, "prob", 1.0]];

    out = Append[out, <|
      "lhs" -> lhs,
      "len" -> Length[rhsIds],
      "rhs1" -> rhsIds[[1]],
      "rhs2" -> If[Length[rhsIds] == 2, rhsIds[[2]], 0],
      "logw" -> Log[Max[10^-12, w]]
    |>],
  {i, Length[rules]}];

  out
];

(* Emite DM lines *)
EmitDeckFromResources[
  grammarPath_, lexiconPath_, sentence_, outDeckPath_,
  opts : OptionsPattern[{"Start" -> "S", "Beam" -> 4, "MaxN" -> 64}]
] := Module[
  {g, l, in, startId, lexMap, words, toks, n, rules,
   lines = {}, START=1, NRULES=2, NADDR=3,
   TOKBASE=100, TSCBASE=200,
   LHSBASE=10000, RHS1BASE=11000, RHS2BASE=12000, LENBASE=13000, LOGWBASE=14000,
   MAXN = OptionValue["MaxN"], BEAM = OptionValue["Beam"],
   id2sym},

  g = ReadJSON[grammarPath];
  l = ReadJSON[lexiconPath];

  in = MakeIntern[];
  (* START primero *)
  startId = Intern[in, Lookup[g, "start", Lookup[g, "root", Lookup[g, "start_symbol", OptionValue["Start"]]]]];

  (* lexicon -> intern POS *)
  lexMap = BuildLexMap[in, l];

  (* grammar -> intern symbols *)
  rules = BuildRules[in, g];

  id2sym = in["id2sym"];

  (* sentence -> tokens -> POS ids *)
  words = SplitMorphologyWL @ TokenizeWords[sentence];
  toks = {};
  Do[
    Module[{w = words[[i]], pos},
      If[KeyExistsQ[lexMap, w],
        pos = lexMap[w]["pos"],
        (* OOV: mayúscula => PROPN, sino NOUN *)
        pos = Intern[in, If[StringMatchQ[StringTake[words[[i]], 1], RegularExpression["\\p{Lu}"]], "PROPN", "NOUN"]];
      ];
      toks = Append[toks, pos];
    ],
  {i, Length[words]}];

  n = Length[toks];
  If[n > MAXN, Print["WARN: sentence truncada a MaxN=", MAXN]; toks = Take[toks, MAXN]; n = MAXN];

  (* Header constants *)
  lines = Join[lines, {
    "DM " <> ToString[START] <> " " <> ToString[startId],
    "DM " <> ToString[NRULES] <> " " <> ToString[Length[rules]],
    "DM " <> ToString[NADDR] <> " " <> ToString[n]
  }];

  (* Tokens: cat + score(0) *)
  Do[
    lines = Append[lines, "DM " <> ToString[TOKBASE + i - 1] <> " " <> ToString[toks[[i]]]];
    lines = Append[lines, "DM " <> ToString[TSCBASE + i - 1] <> " 0"];
  , {i, n}];

  (* Rules arrays *)
  Do[
    lines = Append[lines, "DM " <> ToString[LHSBASE  + i - 1] <> " " <> ToString[rules[[i]]["lhs"]]];
    lines = Append[lines, "DM " <> ToString[RHS1BASE + i - 1] <> " " <> ToString[rules[[i]]["rhs1"]]];
    lines = Append[lines, "DM " <> ToString[RHS2BASE + i - 1] <> " " <> ToString[rules[[i]]["rhs2"]]];
    lines = Append[lines, "DM " <> ToString[LENBASE  + i - 1] <> " " <> ToString[rules[[i]]["len"]]];
    lines = Append[lines, "DM " <> ToString[LOGWBASE + i - 1] <> " " <> ToString[NumberForm[rules[[i]]["logw"], 20]]];
  , {i, Length[rules]}];

  Export[outDeckPath, StringRiffle[lines, "\n"], "Text", CharacterEncoding -> "UTF-8"];

  (* meta para decoder *)
  <|
    "MaxN" -> MAXN, "Beam" -> BEAM,
    "StartId" -> startId,
    "Words" -> words,
    "Id2Sym" -> id2sym,
    "MemMap" -> <|
      "START"->START,"NRULES"->NRULES,"N"->NADDR,
      "TOKBASE"->TOKBASE,"TSCBASE"->TSCBASE,
      "LHSBASE"->LHSBASE,"RHS1BASE"->RHS1BASE,"RHS2BASE"->RHS2BASE,"LENBASE"->LENBASE,"LOGWBASE"->LOGWBASE,
      "CATBASE"->20000
    |>
  |>
];

(* ============================================================
   Decoder: reconstruye árbol usando backpointers en memoria
============================================================ *)

(* Memory layout del programa (debe coincidir con CKY .sc)
   MAXN=64, BEAM=4, MAXJ=MAXN+2=66
   cellOffset = ((i-1)*MAXJ + (j-1))*BEAM + (s-1)
*)
DecodeTree[state_, meta_] := Module[
  {
    mem = state["mem"], map = meta["MemMap"], id2sym = meta["Id2Sym"], words = meta["Words"],
    MAXN = meta["MaxN"], BEAM = meta["Beam"], MAXJ, CELLN, CATBASE, SCRBASE, KNDbase, Abase, Bbase,
    startId = meta["StartId"], n, rootPtr, bestScore = -1.*^30, bestPtr = 0, s, addr, cat, scr, kind, a, b
  },

  MAXJ = MAXN + 2;
  CELLN = MAXN*MAXJ*BEAM;

  CATBASE = 20000;
  SCRBASE = CATBASE + CELLN;
  KNDbase = SCRBASE + CELLN;
  Abase   = KNDbase + CELLN;
  Bbase   = Abase   + CELLN;

  n = Round[mem[[map["N"]]]];
  If[n < 1, Return[""]];

  (* buscar mejor ROOT en cell(1, n+1) *)
  Do[
    addr = CATBASE + (((1 - 1)*MAXJ + ((n + 1) - 1))*BEAM + (s - 1));
    cat = Round[mem[[addr]]];
    scr = mem[[SCRBASE + (addr - CATBASE)]];
    If[cat == startId && scr > bestScore,
      bestScore = scr;
      bestPtr = mem[[Abase + (addr - CATBASE)]]; (* en el slot guardamos ptr “self” en A cuando inserta *)
    ];
  , {s, 1, BEAM}];

  If[bestPtr == 0, Return[""]];

  (* unpack ptr: ptr = ((i*1000)+j)*10 + s *)
  With[
    {
      getCellAddr = Function[{i, j, s},
        CATBASE + (((i - 1)*MAXJ + (j - 1))*BEAM + (s - 1))
      ],
      sym = Function[{id}, Lookup[id2sym, id, "<?>"]]
    },
    Module[{walk},
      walk[ptr_] := Module[{s1, tmp, j1, i1, a1, b1, kind1, addr1, cat1},
        s1 = Round@Mod[ptr, 10];
        tmp = Round@Floor[ptr/10];
        j1 = Round@Mod[tmp, 1000];
        i1 = Round@Floor[tmp/1000];

        addr1 = getCellAddr[i1, j1, s1];
        cat1 = Round[mem[[addr1]]];
        kind1 = Round[mem[[KNDbase + (addr1 - CATBASE)]]];
        a1 = mem[[Abase + (addr1 - CATBASE)]];
        b1 = mem[[Bbase + (addr1 - CATBASE)]];

        Switch[kind1,
          0, (* leaf: a1 = token index *)
            "(" <> sym[cat1] <> " " <> words[[Round[a1]]] <> ")",

          1, (* unary: a1 = child ptr *)
            "(" <> sym[cat1] <> " " <> walk[a1] <> ")",

          2, (* binary: a1 left, b1 right *)
            "(" <> sym[cat1] <> " " <> walk[a1] <> " " <> walk[b1] <> ")",

          _, "(" <> sym[cat1] <> " ?)"
        ]
      ];
      walk[bestPtr]
    ]
  ]
];

End[];
EndPackage[];
