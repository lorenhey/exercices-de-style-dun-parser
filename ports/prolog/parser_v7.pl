#!/usr/bin/env swipl
% Parser V7 - Prolog (SWI-Prolog)
% CKY + beam + unary-closure + tokenización (al/del + enclíticos 1)
% Consume lexicon.json + grammar.json (archivos neutrales).
%
% Uso:
%   swipl -q -s parser_v7.pl -- --file corpus.txt --print --json out.json
%   swipl -q -s parser_v7.pl -- --text "..." --trees --print

:- use_module(library(http/json)).
:- use_module(library(readutil)).
:- use_module(library(assoc)).
:- use_module(library(lists)).

:- initialization(main, main).

% ============================================================================
% Helpers básicos
% ============================================================================

is_var_atom(A) :-
  atom(A),
  sub_atom(A, 0, 1, _, '?').

string_lower(S, L) :- string_lower(S, L).

suffix_string(S, Suff) :-
  string_length(S, LS),
  string_length(Suff, LF),
  LS >= LF,
  Start is LS - LF,
  sub_string(S, Start, LF, 0, Suff).

maybe_null(none, @(null)).
maybe_null(some(V), V).

% ============================================================================
% Feats: lista ordenada de (KeyAtom-ValAtom)
% ============================================================================

feats_norm(FS0, FS) :-
  sort(1, @=<, FS0, FS).

feats_find([], _, none).
feats_find([K-V|_], K, some(V)) :- !.
feats_find([_|T], K, V) :- feats_find(T, K, V).

% Unificación con variables tipo '?x' (átomos)
feats_unify(A, B, Out) :-
  empty_assoc(M0),
  feats_to_assoc(A, M0, M1),
  feats_unify_into(B, M1, M2),
  assoc_to_list(M2, Pairs),
  maplist(kv_to_feat, Pairs, FS0),
  feats_norm(FS0, Out).

kv_to_feat(K-V, K-V).

feats_to_assoc([], M, M).
feats_to_assoc([K-V|T], M0, M) :-
  put_assoc(K, M0, V, M1),
  feats_to_assoc(T, M1, M).

feats_unify_into([], M, M).
feats_unify_into([K-VB|T], M0, M) :-
  ( get_assoc(K, M0, VA) ->
      ( VA == VB ->
          M1 = M0
      ; is_var_atom(VA), \+ is_var_atom(VB) ->
          put_assoc(K, M0, VB, M1)
      ; \+ is_var_atom(VA), is_var_atom(VB) ->
          M1 = M0
      ; is_var_atom(VA), is_var_atom(VB) ->
          M1 = M0
      ; % ambos no-var y distintos
        fail
      )
  ; put_assoc(K, M0, VB, M1)
  ),
  feats_unify_into(T, M1, M).

feats_require(FS, K, V, Out) :-
  feats_unify(FS, [K-V], Out).

% Hash estable (FNV-1a 64) sobre codepoints del string "k=v;" por cada feat ordenada.
fnv64_feats(Feats, Hash) :-
  FNV_OFF is 1469598103934665603,
  fnv64_feats_(Feats, FNV_OFF, Hash).

fnv64_feats_([], H, H).
fnv64_feats_([K-V|T], H0, H) :-
  atom_string(K, KS),
  atom_string(V, VS),
  string_codes(KS, KC),
  string_codes(VS, VC),
  append(KC, [0'=|VC], C1),
  append(C1, [0';], Codes),
  fnv64_update_codes(Codes, H0, H1),
  fnv64_feats_(T, H1, H).

fnv64_update_codes([], H, H).
fnv64_update_codes([C|T], H0, H) :-
  P is 1099511628211,
  Mask is 0xFFFFFFFFFFFFFFFF,
  H1 is ((H0 xor C) * P) /\ Mask,
  fnv64_update_codes(T, H1, H).

% Reemplaza valores (p.ej. ?i -> t3) en feats
feats_replace_value([], _, _, []).
feats_replace_value([K-V|T], From, To, [K-V2|R]) :-
  ( V == From -> V2 = To ; V2 = V ),
  feats_replace_value(T, From, To, R).

% ============================================================================
% Tokenización (DCG): palabras con letras y opcional -/' internos
% ============================================================================

scan_words(String, Words) :-
  string_codes(String, Codes),
  phrase(words(Words), Codes).

words([W|Ws]) --> skip_to_alpha, word(W), !, words(Ws).
words([])     --> skip_to_alpha, [].

skip_to_alpha --> [C], { \+ code_type(C, alpha) }, !, skip_to_alpha.
skip_to_alpha --> [].

word(W) --> letters(L1), word_tail(L2), { append(L1, L2, L), string_codes(W, L) }.

letters([C|Cs]) --> [C], { code_type(C, alpha) }, letters_rest(Cs).
letters_rest([C|Cs]) --> [C], { code_type(C, alpha) }, !, letters_rest(Cs).
letters_rest([]) --> [].

word_tail([C|Rest]) --> connector(C), letters(Ls), !, word_tail(Rest2), { append(Ls, Rest2, Rest) }.
word_tail([]) --> [].

connector(0'-) --> [0'-].
connector(0'') --> [0''].

split_sentences(Text, Sents) :-
  split_string(Text, ".\n\r", " \t", Raw),
  exclude(=(""), Raw, Sents).

tokenize_sentence(Sent, Tokens) :-
  scan_words(Sent, RawWords),
  build_tokens(RawWords, 0, Tokens).

build_tokens([], _, []).
build_tokens([Raw|T], I0, Tokens) :-
  string_lower(Raw, Low),
  ( Low = "al" ->
      T1 = token("a",  "a",  I0),
      I1 is I0 + 1,
      T2 = token("el", "el", I1),
      I2 is I1 + 1,
      build_tokens(T, I2, Rest),
      Tokens = [T1,T2|Rest]
  ; Low = "del" ->
      T1 = token("de", "de", I0),
      I1 is I0 + 1,
      T2 = token("el", "el", I1),
      I2 is I1 + 1,
      build_tokens(T, I2, Rest),
      Tokens = [T1,T2|Rest]
  ; enclitic_split(Raw, Low, I0, MaybePair) ->
      MaybePair = [TokA, TokB],
      I2 is I0 + 2,
      build_tokens(T, I2, Rest),
      Tokens = [TokA,TokB|Rest]
  ; % normal
    Tok = token(Raw, Low, I0),
    I1 is I0 + 1,
    build_tokens(T, I1, Rest),
    Tokens = [Tok|Rest]
  ).

enclitic_split(Raw, Low, I0, [token(RawBase, BaseLow, I0), token(RawCl, ClLow, I1)]) :-
  Clitics = ["me","te","se","lo","la","los","las","le","les","nos","os"],
  longest_suffix(Low, Clitics, Cl),
  string_length(Low, Lw),
  string_length(Cl, Lc),
  BaseLen is Lw - Lc,
  BaseLen > 2,
  sub_string(Low, 0, BaseLen, _, BaseLow),
  looks_verb(BaseLow),
  sub_string(Raw, 0, BaseLen, _, RawBase),
  sub_string(Raw, BaseLen, Lc, 0, RawCl),
  string_lower(RawCl, ClLow),
  I1 is I0 + 1.

looks_verb(B) :-
  suffix_string(B,"ar"); suffix_string(B,"er"); suffix_string(B,"ir");
  suffix_string(B,"ando"); suffix_string(B,"iendo").

longest_suffix(Word, Candidates, Best) :-
  findall(C, (member(C, Candidates), suffix_string(Word, C)), L),
  L \= [],
  map_list_to_pairs(string_length, L, Pairs),
  keysort(Pairs, Sorted),
  last(Sorted, _-Best).

% ============================================================================
% Lexicon / Grammar loading
% ============================================================================

% LexEntry: lex_entry(PosAtom, Weight, Feats)
load_lexicon(Path, LexAssoc) :-
  open(Path, read, S, [encoding(utf8)]),
  json_read_dict(S, D),
  close(S),
  Entries = D.entries,
  empty_assoc(A0),
  dict_pairs(Entries, _, Pairs),
  foldl(add_lex_word, Pairs, A0, LexAssoc).

add_lex_word(WordStr-Arr, A0, A) :-
  maplist(lex_entry_from_dict, Arr, LEs),
  put_assoc(WordStr, A0, LEs, A).

lex_entry_from_dict(Obj, lex_entry(Pos, W, Feats)) :-
  Pos = Obj.pos,                 % ya viene como string en dict
  atom_string(PosAtom, Pos),
  W is Obj.weight,
  ( get_dict(feats, Obj, FDict) -> true ; FDict = _{} ),
  dict_pairs(FDict, _, FPairs),
  maplist(feat_pair_to_kv, FPairs, FS0),
  feats_norm(FS0, Feats),
  Pos = Pos, % suppress singleton
  PosAtom = PosAtom.

feat_pair_to_kv(KS-VS, K-V) :-
  atom_string(K, KS),
  atom_string(V, VS).

% Rule: rule(LHS, RHS1, RHS2opt, Len, Weight, Op, ArgKey, ArgVal, ArgType, PropIdxRight)
load_grammar(Path, UnaryIdx, BinaryIdx) :-
  open(Path, read, S, [encoding(utf8)]),
  json_read_dict(S, D),
  close(S),
  RulesD = D.rules,
  maplist(rule_from_dict, RulesD, Rules),
  build_rule_indexes(Rules, UnaryIdx, BinaryIdx).

rule_from_dict(Obj, rule(LHS, R1, R2, Len, W, Op, AK, AV, AT, Prop)) :-
  atom_string(LHS, Obj.lhs),
  RHS = Obj.rhs,
  ( RHS = [S1] ->
      atom_string(R1, S1), R2 = none, Len = 1
  ; RHS = [S1,S2] ->
      atom_string(R1, S1), atom_string(R2a, S2), R2 = some(R2a), Len = 2
  ; throw(error(invalid_rhs_length, Obj))
  ),
  W is Obj.weight,
  ( get_dict(op, Obj, OpS) -> true ; OpS = "EMPTY" ),
  atom_string(Op, OpS),
  ( get_dict(args, Obj, Args) -> true ; Args = _{} ),
  ( get_dict(key, Args, K) -> atom_string(AK, K) ; AK = none ),
  ( get_dict(value, Args, V) -> atom_string(AV, V) ; AV = none ),
  ( get_dict(type, Args, T) -> atom_string(AT, T) ; AT = none ),
  ( get_dict(post, Obj, Post) -> true ; Post = [] ),
  ( member("PROPAGATE_IDX_TO_RIGHT", Post) -> Prop = true ; Prop = false ).

build_rule_indexes(Rules, UnaryIdx, BinaryIdx) :-
  empty_assoc(U0),
  empty_assoc(B0),
  foldl(idx_rule, Rules, (U0,B0), (UnaryIdx,BinaryIdx)).

idx_rule(rule(LHS,R1,R2,Len,W,Op,AK,AV,AT,Prop), (U0,B0), (U,B)) :-
  ( Len =:= 1 ->
      ( get_assoc(R1, U0, L0) -> true ; L0 = [] ),
      put_assoc(R1, U0, [rule(LHS,R1,R2,Len,W,Op,AK,AV,AT,Prop)|L0], U),
      B = B0
  ; Len =:= 2, R2 = some(R2a) ->
      Key = pair(R1,R2a),
      ( get_assoc(Key, B0, L0) -> true ; L0 = [] ),
      put_assoc(Key, B0, [rule(LHS,R1,R2,Len,W,Op,AK,AV,AT,Prop)|L0], B),
      U = U0
  ).

% ============================================================================
% Constants
% ============================================================================

constants(C) :-
  C = c{
    idx:'idx', qi:'?i', gap:'gap', obl:'obl', yes:'yes', fin:'fin', no:'no',
    gen:'gen', num:'num', sg:'sg',
    TOK:'TOK', S:'S', VP_FIN:'VP_FIN', Pinf:'Pinf', VP_NF:'VP_NF', VP:'VP', Cl:'Cl',
    N:'N', PropN:'PropN', Pron:'Pron', Adv:'Adv', Vi:'Vi', Vt:'Vt'
  }.

% ============================================================================
% Arena
% ============================================================================

arena_empty(arena(0, A)) :- empty_assoc(A).

arena_add(arena(N,A0), Node, Id, arena(N1,A1)) :-
  Id = N,
  N1 is N + 1,
  put_assoc(Id, A0, Node, A1).

arena_get(arena(_,A), Id, Node) :- get_assoc(Id, A, Node).

arena_clone_replace(A0, Root, From, To, NewId, A) :-
  arena_get(A0, Root, node(L,Leaf,Raw,Feats,Score,Left,Right,Child)),
  feats_replace_value(Feats, From, To, Feats2),
  ( Child == none -> Child2 = none, A1 = A0
  ; arena_clone_replace(A0, Child, From, To, CId, A1), Child2 = some(CId)
  ),
  ( Left == none -> Left2 = none, A2 = A1
  ; arena_clone_replace(A1, Left, From, To, LId, A2), Left2 = some(LId)
  ),
  ( Right == none -> Right2 = none, A3 = A2
  ; arena_clone_replace(A2, Right, From, To, RId, A3), Right2 = some(RId)
  ),
  arena_add(A3, node(L,Leaf,Raw,Feats2,Score,Left2,Right2,Child2), NewId, A).

arena_pretty(Arena, C, Root, OutString) :-
  with_output_to(string(OutString), arena_pretty_(Arena, C, Root, 0)).

arena_pretty_(Arena, _C, Id, Ind) :-
  arena_get(Arena, Id, node(Label, IsLeaf, Raw, Feats, Score, Left, Right, Child)),
  PadN is Ind * 2,
  tab(PadN),
  ( IsLeaf == true ->
      format("~w~n", [Raw])
  ; format("~w", [Label]),
    ( Feats == [] -> true
    ; format(" [", []),
      print_feats(Feats),
      format("]", [])
    ),
    format("  (score=~3f)~n", [Score]),
    ( Child = some(CId) ->
        arena_pretty_(Arena, _C, CId, Ind+1)
    ; (Left = some(LId) -> arena_pretty_(Arena, _C, LId, Ind+1) ; true),
      (Right = some(RId) -> arena_pretty_(Arena, _C, RId, Ind+1) ; true)
    )
  ).

print_feats([]).
print_feats([K-V]) :- format("~w=~w", [K,V]).
print_feats([K-V|T]) :- format("~w=~w, ", [K,V]), print_feats(T).

% ============================================================================
% Bucket/Cell/Chart (assoc)
% ============================================================================

bucket_empty(bucket([], H)) :- empty_assoc(H).

bucket_has_hash(bucket(_,H), Hash) :-
  get_assoc(Hash, H, _).

insert_desc(Item, [], [Item]).
insert_desc(Item, [X|Xs], [Item,X|Xs]) :-
  Item = item(_,_,_,S,_),
  X    = item(_,_,_,Sx,_),
  S > Sx, !.
insert_desc(Item, [X|Xs], [X|Ys]) :-
  insert_desc(Item, Xs, Ys).

bucket_insert(Beam, Item, bucket(Items0, H0), bucket(Items1, H1), Pruned) :-
  Item = item(_,_,Hash,_,_),
  put_assoc(Hash, H0, true, H1),
  insert_desc(Item, Items0, ItemsX),
  length(ItemsX, L),
  ( L =< Beam ->
      Items1 = ItemsX,
      Pruned = 0
  ; prefix_length(ItemsX, Items1, Beam),
    Pruned is L - Beam
  ).

cell_empty(Cell) :- empty_assoc(Cell).

cell_add_item(Beam, Item, Cell0, Cell, Pruned, Changed) :-
  Item = item(Cat,_,Hash,_,_),
  ( get_assoc(Cat, Cell0, B0) -> true ; bucket_empty(B0) ),
  ( bucket_has_hash(B0, Hash) ->
      Cell = Cell0, Pruned = 0, Changed = false
  ; bucket_insert(Beam, Item, B0, B1, Pruned),
    put_assoc(Cat, Cell0, B1, Cell),
    Changed = true
  ).

chart_empty(Chart) :- empty_assoc(Chart).

chart_key(I,J,N,Key) :- Key is I*(N+1) + J.

chart_get(Chart, I, J, N, Cell) :-
  chart_key(I,J,N,K),
  ( get_assoc(K, Chart, Cell) -> true ; cell_empty(Cell) ).

chart_set(Chart0, I, J, N, Cell, Chart) :-
  chart_key(I,J,N,K),
  put_assoc(K, Chart0, Cell, Chart).

% ============================================================================
% OOV guess
% ============================================================================

det_word(Low) :- member(Low, ["el","la","los","las"]).

guess_lex(C, token(Raw, Low, _), Entries) :-
  findall(lex_entry(Pos, W, Feats),
    ( % PropN
      string_codes(Raw, [C0|_]),
      code_type(C0, upper),
      \+ det_word(Low),
      Pos = C.PropN, W = 0.03,
      feats_norm([C.num-C.sg], Feats)
    ), P1),
  findall(lex_entry(Pos, W, Feats),
    ( suffix_string(Low,"mente"),
      Pos = C.Adv, W = -0.03, Feats = []
    ), P2),
  findall(lex_entry(Pos, W, Feats),
    ( (suffix_string(Low,"ar");suffix_string(Low,"er");suffix_string(Low,"ir")),
      Base = [C.fin-C.no, C.obl-C.no], feats_norm(Base, Feats),
      (Pos=C.Vi, W=-0.12 ; Pos=C.Vt, W=-0.14)
    ; (suffix_string(Low,"ando");suffix_string(Low,"iendo")),
      Base = [C.fin-C.no, C.obl-C.no], feats_norm(Base, Feats),
      (Pos=C.Vi, W=-0.14 ; Pos=C.Vt, W=-0.16)
    ), P3),
  append([P1,P2,P3], Guess),
  ( Guess == [] ->
      feats_norm([C.gen-'?g', C.num-'?n'], FN),
      Entries = [lex_entry(C.N, -0.35, FN)]
  ; Entries = Guess
  ).

% ============================================================================
% Ops DSL
% ============================================================================

apply_op(_C, _Op, _LF, _RF, _Rule, none) :- fail.

apply_op(C, Op, LF, RF, rule(_LHS,_R1,_R2,_Len,_W,Op,AK,AV,AT,_Prop), Out) :-
  ( Op == 'EMPTY' ->
      Out = []
  ; Op == 'LEFT'  ->
      Out = LF
  ; Op == 'RIGHT' ->
      Out = RF
  ; Op == 'UNIFY' ->
      feats_unify(LF, RF, Out)
  ; Op == 'REQUIRE_LEFT' ->
      AK \== none, AV \== none,
      feats_require(LF, AK, AV, Out)
  ; Op == 'REQUIRE_RIGHT' ->
      AK \== none, AV \== none,
      feats_require(RF, AK, AV, Out)
  ; Op == 'MAKE_GAP' ->
      AT \== none,
      feats_norm([C.idx-C.qi, C.gap-AT], Out)
  ; Op == 'RELCLAUSE_OBL' ->
      feats_require(RF, C.obl, C.yes, _),
      feats_unify(LF, [C.gap-C.obl], Out)
  ; fail
  ).

% ============================================================================
% Unary closure
% ============================================================================

unary_closure(Cell0, C, UnaryIdx, Beam, Arena0, Cell, Arena, Pruned, UnaryApps) :-
  unary_loop(Cell0, C, UnaryIdx, Beam, Arena0, Cell, Arena, 0, Pruned, 0, UnaryApps).

unary_loop(Cell0, C, UnaryIdx, Beam, Arena0, Cell, Arena, P0, P, U0, U) :-
  assoc_to_list(Cell0, Cats),
  unary_pass(Cats, Cell0, C, UnaryIdx, Beam, Arena0, Cell1, Arena1, Pinc, Uinc, Changed),
  P1 is P0 + Pinc,
  U1 is U0 + Uinc,
  ( Changed == true ->
      unary_loop(Cell1, C, UnaryIdx, Beam, Arena1, Cell, Arena, P1, P, U1, U)
  ; Cell = Cell1, Arena = Arena1, P = P1, U = U1
  ).

unary_pass([], Cell, _C, _UnaryIdx, _Beam, Arena, Cell, Arena, 0, 0, false).
unary_pass([RhsCat-Bucket|T], Cell0, C, UnaryIdx, Beam, Arena0, Cell, Arena, P, U, Changed) :-
  ( get_assoc(RhsCat, UnaryIdx, Rules) -> true ; Rules = [] ),
  Bucket = bucket(Items, _),
  unary_apply_rules(Rules, Items, Cell0, C, Beam, Arena0, Cell1, Arena1, P1, U1, Ch1),
  unary_pass(T, Cell1, C, UnaryIdx, Beam, Arena1, Cell, Arena, P2, U2, Ch2),
  P is P1 + P2,
  U is U1 + U2,
  (Ch1 == true ; Ch2 == true -> Changed = true ; Changed = false).

unary_apply_rules([], _Items, Cell, _C, _Beam, Arena, Cell, Arena, 0, 0, false).
unary_apply_rules([Rule|RT], Items, Cell0, C, Beam, Arena0, Cell, Arena, P, U, Changed) :-
  unary_apply_items(Items, Rule, Cell0, C, Beam, Arena0, Cell1, Arena1, P1, U1, Ch1),
  unary_apply_rules(RT, Items, Cell1, C, Beam, Arena1, Cell, Arena, P2, U2, Ch2),
  P is P1 + P2,
  U is U1 + U2,
  (Ch1 == true ; Ch2 == true -> Changed = true ; Changed = false).

unary_apply_items([], _Rule, Cell, _C, _Beam, Arena, Cell, Arena, 0, 0, false).
unary_apply_items([item(_Cat, Feats, _H, Score, NodeId)|T], Rule, Cell0, C, Beam, Arena0, Cell, Arena, P, U, Changed) :-
  Rule = rule(LHS, _R1, _R2, _Len, W, Op, _AK, _AV, _AT, _Prop),
  ( apply_op(C, Op, Feats, [], Rule, PFeats) ->
      U1 is 1,
      S1 is Score + W,
      arena_add(Arena0, node(LHS, false, "", PFeats, S1, none, none, some(NodeId)), NewNode, ArenaA),
      fnv64_feats(PFeats, H),
      Item = item(LHS, PFeats, H, S1, NewNode),
      cell_add_item(Beam, Item, Cell0, CellA, Pr, ChA),
      P1 is Pr,
      ChThis = ChA,
      ArenaB = ArenaA
  ; P1 is 0, U1 is 0, CellA = Cell0, ArenaB = Arena0, ChThis = false
  ),
  unary_apply_items(T, Rule, CellA, C, Beam, ArenaB, Cell, Arena, P2, U2, Ch2),
  P is P1 + P2,
  U is U1 + U2,
  (ChThis == true ; Ch2 == true -> Changed = true ; Changed = false).

% ============================================================================
% Sanity checks
% ============================================================================

has_desc_label(Arena, NodeId, Label) :-
  arena_get(Arena, NodeId, node(L, IsLeaf, _Raw, _F, _S, Left, Right, Child)),
  ( IsLeaf == false, L == Label
  ; Child = some(CId), has_desc_label(Arena, CId, Label)
  ; Left  = some(LId), has_desc_label(Arena, LId, Label)
  ; Right = some(RId), has_desc_label(Arena, RId, Label)
  ).

sanity_s_has_vpfin(Arena, C, Root) :-
  S = C.S, VPFin = C.VP_FIN,
  sanity_s_has_vpfin_(Arena, S, VPFin, Root).

sanity_s_has_vpfin_(Arena, S, VPFin, NodeId) :-
  arena_get(Arena, NodeId, node(L, IsLeaf, _Raw, _F, _Sc, Left, Right, Child)),
  ( IsLeaf == false, L == S,
    ( (Child = some(CId), arena_get(Arena, CId, node(VPFin,_,_,_,_,_,_,_)))
    ; (Left  = some(LId), arena_get(Arena, LId, node(VPFin,_,_,_,_,_,_,_)))
    ; (Right = some(RId), arena_get(Arena, RId, node(VPFin,_,_,_,_,_,_,_)))
    )
  ; Child = some(CId2), sanity_s_has_vpfin_(Arena, S, VPFin, CId2)
  ; Left  = some(LId2), sanity_s_has_vpfin_(Arena, S, VPFin, LId2)
  ; Right = some(RId2), sanity_s_has_vpfin_(Arena, S, VPFin, RId2)
  ).

sanity_sin_takes_vpnf(Arena, C, Root) :-
  sanity_sin_takes_vpnf_(Arena, C.Pinf, C.VP_NF, Root).

sanity_sin_takes_vpnf_(Arena, Pinf, VPNF, NodeId) :-
  arena_get(Arena, NodeId, node(L, IsLeaf, _Raw, _F, _Sc, Left, Right, Child)),
  ( IsLeaf == false, L == Pinf ->
      has_desc_label(Arena, NodeId, VPNF),
      cont(Arena, Pinf, VPNF, Child, Left, Right)
  ; cont(Arena, Pinf, VPNF, Child, Left, Right)
  ).

cont(_Arena, _P, _V, none, none, none).
cont(Arena, P, V, Child, Left, Right) :-
  ( Child = some(CId) -> sanity_sin_takes_vpnf_(Arena, P, V, CId) ; true ),
  ( Left  = some(LId) -> sanity_sin_takes_vpnf_(Arena, P, V, LId) ; true ),
  ( Right = some(RId) -> sanity_sin_takes_vpnf_(Arena, P, V, RId) ; true ).

sanity_enclitic_only_nf(Arena, C, Root) :-
  sanity_enclitic_only_nf_(Arena, C.VP, C.Cl, C.Vt, C.Vi, Root).

sanity_enclitic_only_nf_(Arena, VP, Cl, Vt, Vi, NodeId) :-
  arena_get(Arena, NodeId, node(L, IsLeaf, _Raw, _F, _Sc, Left, Right, Child)),
  ( IsLeaf == false, L == VP, Left = some(LId), Right = some(RId) ->
      arena_get(Arena, LId, node(LL, LLLeaf, _, _, _, _, _, _)),
      arena_get(Arena, RId, node(RL, RLLeaf, _, _, _, _, _, _)),
      \+ ( RLLeaf == false, RL == Cl,
           LLLeaf == false, (LL == Vt ; LL == Vi)
         ),
      cont_encl(Arena, VP, Cl, Vt, Vi, Child, Left, Right)
  ; cont_encl(Arena, VP, Cl, Vt, Vi, Child, Left, Right)
  ).

cont_encl(_Arena, _VP, _Cl, _Vt, _Vi, none, none, none).
cont_encl(Arena, VP, Cl, Vt, Vi, Child, Left, Right) :-
  ( Child = some(CId) -> sanity_enclitic_only_nf_(Arena, c{VP:VP,Cl:Cl,Vt:Vt,Vi:Vi}.VP, Cl, Vt, Vi, CId) ; true ),
  ( Left  = some(LId) -> sanity_enclitic_only_nf_(Arena, VP, Cl, Vt, Vi, LId) ; true ),
  ( Right = some(RId) -> sanity_enclitic_only_nf_(Arena, VP, Cl, Vt, Vi, RId) ; true ).

% ============================================================================
% Parse sentence (CKY)
% ============================================================================

apply_binary_rules(_Rules, _ItemsL, _ItemsR, _C, _Beam, _Arena0, Cell, Arena, 0, false) :-
  cell_empty(Cell), arena_empty(Arena), fail.

parse_sentence(Sent, LexAssoc, UnaryIdx, BinaryIdx, C, Beam, TopK, WantTrees, WantPrint, RowDict) :-
  statistics(walltime, [T0,_]),
  tokenize_sentence(Sent, Tokens),
  length(Tokens, N),
  ( N =:= 0 ->
      RowDict = _{ sentence:Sent, tokens:0, oovTokens:0, parsed:false, nParsesReturned:0,
                   bestScore: @(null), timeMs:0.0, chartItemsTotal:0, chartItemsMaxCell:0,
                   prunedByBeam:0, unaryApplications:0, ambiguousCells:0,
                   sanitySHasVpFin:false, sanitySinTakesVpNf:false, sanityEncliticOnlyNf:false,
                   notes:["empty"], bestTree:@(null) }
  ; % tids
    findall(I-TAtom, (between(0, N-1, I), number_string(I,S), string_concat("t",S,TS), atom_string(TAtom, TS)), TPairs),
    list_to_assoc(TPairs, TIds),

    chart_empty(Chart0),
    arena_empty(Arena0),

    init_lex_cells(Tokens, 0, N, LexAssoc, C, Beam, TIds, UnaryIdx, Chart0, Chart1, Arena0, Arena1, OOV, Pruned1, Unary1),

    cky_spans(2, N, Tokens, N, C, Beam, UnaryIdx, BinaryIdx, Chart1, Chart2, Arena1, Arena2, Pruned1, Pruned2, Unary1, Unary2),

    % metrics
    chart_metrics(Chart2, N, ItemsTotal, MaxCell, AmbCells),

    % best S at (0,N)
    chart_get(Chart2, 0, N, N, CellSN),
    ( get_assoc(C.S, CellSN, bucket(ItemsS, _)) , ItemsS \= [] ->
        ItemsS = [item(_,_,_,BestScore,BestNode)|_],
        length(ItemsS, LS),
        ( LS < TopK -> NRet = LS ; NRet = TopK ),
        sanity_s_has_vpfin(Arena2, C, BestNode) -> S1=true ; S1=false,
        ( sanity_sin_takes_vpnf(Arena2, C, BestNode) -> S2=true ; S2=false ),
        ( sanity_enclitic_only_nf(Arena2, C, BestNode) -> S3=true ; S3=false ),
        findall(Note,
          ( (S1==false, Note="WARN: S sin VP_FIN visible")
          ; (S2==false, Note="WARN: 'sin' sin VP_NF bajo Pinf")
          ; (S3==false, Note="WARN: enclítico con verbo finito")
          ), Notes0),
        ( WantTrees == true ->
            arena_pretty(Arena2, C, BestNode, TreeStr),
            BestTree = TreeStr
        ; BestTree = @(null)
        ),
        Parsed = true,
        BestScoreOut = BestScore,
        Notes = Notes0
    ; Parsed = false,
      NRet = 0,
      BestScoreOut = @(null),
      Notes = ["NO_PARSE"],
      BestTree = @(null),
      S1=false,S2=false,S3=false
    ),

    statistics(walltime, [T1,_]),
    TimeMs is float(T1 - T0),

    RowDict = _{
      sentence:Sent, tokens:N, oovTokens:OOV,
      parsed:Parsed, nParsesReturned:NRet,
      bestScore:BestScoreOut, timeMs:TimeMs,
      chartItemsTotal:ItemsTotal, chartItemsMaxCell:MaxCell,
      prunedByBeam:Pruned2, unaryApplications:Unary2,
      ambiguousCells:AmbCells,
      sanitySHasVpFin:S1, sanitySinTakesVpNf:S2, sanityEncliticOnlyNf:S3,
      notes:Notes, bestTree:BestTree
    },

    ( WantPrint == true ->
        format("==============================================================================~n", []),
        format("~w~n", [Sent]),
        format("tokens=~d  oov=~d  parsed=~d  parses=~d  bestScore=~w  time_ms=~1f~n",
          [N,OOV,(Parsed->1;0),NRet,BestScoreOut,TimeMs]),
        format("chart_items=~d  max_cell=~d  pruned=~d  unary_apps=~d  amb_cells=~d~n",
          [ItemsTotal,MaxCell,Pruned2,Unary2,AmbCells]),
        ( Notes \= [] -> format("notes: ~w~n", [Notes]) ; true ),
        ( WantTrees == true, BestTree \= @(null) -> format("~s", [BestTree]) ; true )
      ; true
    )
  ).

init_lex_cells([], _I, _N, _Lex, _C, _Beam, _TIds, _UnaryIdx, Chart, Chart, Arena, Arena, 0, 0, 0).
init_lex_cells([Tok|T], I, N, Lex, C, Beam, TIds, UnaryIdx, Chart0, Chart, Arena0, Arena, OOV, Pruned, UnaryApps) :-
  Tok = token(Raw, Low, Idx),
  chart_get(Chart0, I, I1, N, Cell0), I1 is I+1,

  ( get_assoc(Low, Lex, LEs0) ->
      OOV1 = 0,
      maplist(lex_entry_atompos, LEs0, LEs)
  ; OOV1 = 1,
    guess_lex(C, Tok, LEs)
  ),

  emit_lex_entries(LEs, Tok, C, TIds, Beam, Arena0, Arena1, Cell0, Cell1, Pr1),

  unary_closure(Cell1, C, UnaryIdx, Beam, Arena1, Cell2, Arena2, Pr2, U2),

  chart_set(Chart0, I, I1, N, Cell2, Chart1),

  I2 is I + 1,
  init_lex_cells(T, I2, N, Lex, C, Beam, TIds, UnaryIdx, Chart1, Chart, Arena2, Arena, OOVR, PrR, UR),

  OOV is OOV1 + OOVR,
  Pruned is Pr1 + Pr2 + PrR,
  UnaryApps is U2 + UR.

lex_entry_atompos(lex_entry(PosStr,W,F), lex_entry(Pos,W,F)) :-
  atom_string(Pos, PosStr).

emit_lex_entries([], _Tok, _C, _TIds, _Beam, Arena, Arena, Cell, Cell, 0).
emit_lex_entries([lex_entry(Pos,W,Feats0)|T], Tok, C, TIds, Beam, Arena0, Arena, Cell0, Cell, Pruned) :-
  Tok = token(Raw, _Low, Idx),
  ( (Pos == C.N ; Pos == C.PropN ; Pos == C.Pron),
    feats_find(Feats0, C.idx, none),
    get_assoc(Idx, TIds, TId),
    feats_unify(Feats0, [C.idx-TId], Feats1) ->
      true
  ; Feats1 = Feats0
  ),
  arena_add(Arena0, node(C.TOK, true, Raw, [], W, none, none, none), LeafId, Arena1),
  arena_add(Arena1, node(Pos, false, "", Feats1, W, none, none, some(LeafId)), PreId, Arena2),
  fnv64_feats(Feats1, H),
  Item = item(Pos, Feats1, H, W, PreId),
  cell_add_item(Beam, Item, Cell0, Cell1, Pr1, _Ch),
  emit_lex_entries(T, Tok, C, TIds, Beam, Arena2, Arena, Cell1, Cell, Pr2),
  Pruned is Pr1 + Pr2.

cky_spans(Span, N, _Tokens, _NN, _C, _Beam, _UnaryIdx, _BinaryIdx, Chart, Chart, Arena, Arena, Pr, Pr, U, U) :-
  Span > N, !.
cky_spans(Span, N, Tokens, NN, C, Beam, UnaryIdx, BinaryIdx, Chart0, Chart, Arena0, Arena, Pr0, Pr, U0, U) :-
  MaxI is N - Span,
  cky_cells(0, MaxI, Span, N, Tokens, C, Beam, UnaryIdx, BinaryIdx, Chart0, Chart1, Arena0, Arena1, Pr0, Pr1, U0, U1),
  Span2 is Span + 1,
  cky_spans(Span2, N, Tokens, NN, C, Beam, UnaryIdx, BinaryIdx, Chart1, Chart, Arena1, Arena, Pr1, Pr, U1, U).

cky_cells(I, MaxI, _Span, _N, _Tokens, _C, _Beam, _UnaryIdx, _BinaryIdx, Chart, Chart, Arena, Arena, Pr, Pr, U, U) :-
  I > MaxI, !.
cky_cells(I, MaxI, Span, N, Tokens, C, Beam, UnaryIdx, BinaryIdx, Chart0, Chart, Arena0, Arena, Pr0, Pr, U0, U) :-
  J is I + Span,
  chart_get(Chart0, I, J, N, Cell0),
  splits(I, J, Ks),
  foldl(cky_split(I,J,N,C,Beam,BinaryIdx,Chart0), Ks, (Cell0,Arena0,Pr0), (Cell1,Arena1,Pr1)),
  unary_closure(Cell1, C, UnaryIdx, Beam, Arena1, Cell2, Arena2, Pr2, U2),
  chart_set(Chart0, I, J, N, Cell2, Chart1),
  I2 is I + 1,
  U1 is U0 + U2,
  PrX is Pr1 + Pr2,
  cky_cells(I2, MaxI, Span, N, Tokens, C, Beam, UnaryIdx, BinaryIdx, Chart1, Chart, Arena2, Arena, PrX, Pr, U1, U).

splits(I,J,Ks) :-
  I1 is I + 1,
  J1 is J - 1,
  findall(K, between(I1, J1, K), Ks).

cky_split(I,J,N,C,Beam,BinaryIdx,Chart, K, (Cell0,Arena0,Pr0), (Cell,Arena,Pr)) :-
  chart_get(Chart, I, K, N, LCell),
  chart_get(Chart, K, J, N, RCell),
  ( assoc_empty(LCell) ; assoc_empty(RCell) ) ->
      Cell = Cell0, Arena = Arena0, Pr = Pr0
  ; assoc_to_list(LCell, LList),
    assoc_to_list(RCell, RList),
    foldl(combine_left(RList, C, Beam, BinaryIdx), LList, (Cell0,Arena0,Pr0), (Cell,Arena,Pr)).

combine_left(RList, C, Beam, BinaryIdx, CatL-BucketL, Acc0, Acc) :-
  BucketL = bucket(ItemsL, _),
  foldl(combine_right(CatL, ItemsL, C, Beam, BinaryIdx), RList, Acc0, Acc).

combine_right(CatL, ItemsL, C, Beam, BinaryIdx, CatR-BucketR, (Cell0,Arena0,Pr0), (Cell,Arena,Pr)) :-
  BucketR = bucket(ItemsR, _),
  Key = pair(CatL, CatR),
  ( get_assoc(Key, BinaryIdx, Rules0) -> true ; Rules0 = [] ),
  % rules fueron indexadas con push; no importa el orden
  foldl(apply_one_rule(ItemsL, ItemsR, C, Beam), Rules0, (Cell0,Arena0,Pr0), (Cell,Arena,Pr)).

apply_one_rule(ItemsL, ItemsR, C, Beam, Rule, (Cell0,Arena0,Pr0), (Cell,Arena,Pr)) :-
  foldl(apply_items_r(ItemsR, C, Beam, Rule), ItemsL, (Cell0,Arena0,Pr0), (Cell,Arena,Pr)).

apply_items_r(ItemsR, C, Beam, Rule, ItemL, (Cell0,Arena0,Pr0), (Cell,Arena,Pr)) :-
  foldl(apply_pair(C, Beam, Rule, ItemL), ItemsR, (Cell0,Arena0,Pr0), (Cell,Arena,Pr)).

apply_pair(C, Beam, Rule, item(_CL, FeatsL, _HL, ScoreL, NodeL),
           item(_CR, FeatsR, _HR, ScoreR, NodeR),
           (Cell0,Arena0,Pr0), (Cell,Arena,Pr)) :-
  Rule = rule(LHS,_R1,_R2,_Len,W,Op,_AK,_AV,_AT,Prop),
  ( apply_op(C, Op, FeatsL, FeatsR, Rule, PFeats) ->
      Score is ScoreL + ScoreR + W,
      ( Prop == true,
        feats_find(FeatsL, C.idx, some(IdxV)) ->
          arena_clone_replace(Arena0, NodeR, C.qi, IdxV, NodeR2, Arena1)
      ; NodeR2 = NodeR, Arena1 = Arena0
      ),
      arena_add(Arena1, node(LHS, false, "", PFeats, Score, some(NodeL), some(NodeR2), none), NewNode, Arena2),
      fnv64_feats(PFeats, H),
      Item = item(LHS, PFeats, H, Score, NewNode),
      cell_add_item(Beam, Item, Cell0, Cell1, Pr1, _Ch),
      Cell = Cell1, Arena = Arena2, Pr is Pr0 + Pr1
  ; Cell = Cell0, Arena = Arena0, Pr = Pr0
  ).

assoc_empty(A) :- assoc_to_list(A, L), L == [].

chart_metrics(Chart, _N, ItemsTotal, MaxCell, AmbCells) :-
  assoc_to_list(Chart, Pairs),
  foldl(cell_metrics, Pairs, (0,0,0), (ItemsTotal, MaxCell, AmbCells)).

cell_metrics(_K-Cell, (T0,M0,A0), (T,M,A)) :-
  assoc_to_list(Cell, Cats),
  length(Cats, CatN),
  ( CatN >= 2 -> A is A0 + 1 ; A = A0 ),
  findall(Len, (member(_Cat-bucket(Items,_H), Cats), length(Items, Len)), Lens),
  sum_list(Lens, CItems),
  T is T0 + CItems,
  ( CItems > M0 -> M = CItems ; M = M0 ).

% ============================================================================
% CLI + Main
% ============================================================================

default_opts(opts{
  lex:"lexicon.json",
  grammar:"grammar.json",
  file:"corpus.txt",
  text:@(null),
  json:@(null),
  beam:16,
  topk:1,
  trees:false,
  print:false
}).

parse_argv([], Opts, Opts).
parse_argv(["--lex",V|T], O0, O) :- put_dict(lex, O0, V, O1), parse_argv(T,O1,O).
parse_argv(["--grammar",V|T], O0, O) :- put_dict(grammar, O0, V, O1), parse_argv(T,O1,O).
parse_argv(["--file",V|T], O0, O) :- put_dict(file, O0, V, O1), parse_argv(T,O1,O).
parse_argv(["--text",V|T], O0, O) :- put_dict(text, O0, V, O1), parse_argv(T,O1,O).
parse_argv(["--json",V|T], O0, O) :- put_dict(json, O0, V, O1), parse_argv(T,O1,O).
parse_argv(["--beam",V|T], O0, O) :- number_string(N,V), put_dict(beam, O0, N, O1), parse_argv(T,O1,O).
parse_argv(["--topk",V|T], O0, O) :- number_string(N,V), put_dict(topk, O0, N, O1), parse_argv(T,O1,O).
parse_argv(["--trees"|T], O0, O) :- put_dict(trees, O0, true, O1), parse_argv(T,O1,O).
parse_argv(["--print"|T], O0, O) :- put_dict(print, O0, true, O1), parse_argv(T,O1,O).
parse_argv(["--help"|_], _, _) :-
  format("Uso: swipl -q -s parser_v7.pl -- [--lex lexicon.json] [--grammar grammar.json] [--file corpus.txt | --text \"...\"] [--beam 16] [--topk 1] [--trees] [--print] [--json out.json]~n", []),
  halt(0).
parse_argv([X|_], _, _) :-
  format("Arg desconocido: ~w~n", [X]),
  halt(2).

main(_Argv0) :-
  current_prolog_flag(argv, Argv),
  default_opts(D0),
  parse_argv(Argv, D0, Opts),
  constants(C),

  load_lexicon(Opts.lex, Lex),
  load_grammar(Opts.grammar, UnaryIdx, BinaryIdx),

  ( Opts.text \== @(null) ->
      Text = Opts.text
  ; read_file_to_string(Opts.file, Text, [encoding(utf8)])
  ),
  split_sentences(Text, Sents),

  maplist(run_one(Lex, UnaryIdx, BinaryIdx, C, Opts.beam, Opts.topk, Opts.trees, Opts.print), Sents, Rows),

  length(Sents, SN),
  include(row_parsed, Rows, ParsedRows),
  length(ParsedRows, PN),
  Coverage is (SN =:= 0 -> 0.0 ; PN / SN),

  findall(Tk, (member(R, Rows), Tk = R.tokens), Toks),
  sum_list(Toks, TotTok),
  AvgTok is (SN =:= 0 -> 0.0 ; TotTok / SN),

  findall(Oov, (member(R, Rows), Oov = R.oovTokens), Oovs),
  sum_list(Oovs, TotOov),
  AvgOov is (SN =:= 0 -> 0.0 ; TotOov / SN),

  findall(TM, (member(R, Rows), TM = R.timeMs), Times),
  sum_list(Times, TotTime),
  AvgTime is (SN =:= 0 -> 0.0 ; TotTime / SN),

  ( Opts.print == true ->
      format("==============================================================================~n", []),
      format("SUMMARY~n", []),
      format("sentences=~d  coverage=~3f  avg_tokens=~2f  avg_oov=~2f  avg_time_ms=~1f  beam=~d  top_k=~d~n",
        [SN,Coverage,AvgTok,AvgOov,AvgTime,Opts.beam,Opts.topk])
  ; format("SUMMARY: sentences=~d coverage=~3f avg_time_ms=~1f beam=~d top_k=~d~n",
      [SN,Coverage,AvgTime,Opts.beam,Opts.topk])
  ),

  ( Opts.json \== @(null) ->
      Summary = _{ sentences:SN, coverage:Coverage, avgTokens:AvgTok, avgOov:AvgOov,
                   totalTimeMs:TotTime, avgTimeMs:AvgTime, beam:Opts.beam, topK:Opts.topk,
                   rows:Rows },
      open(Opts.json, write, S, [encoding(utf8)]),
      json_write_dict(S, Summary, [width(0)]),
      close(S),
      format("Wrote JSON: ~w~n", [Opts.json])
  ; true
  ).

run_one(Lex, UnaryIdx, BinaryIdx, C, Beam, TopK, Trees, Print, Sent, Row) :-
  parse_sentence(Sent, Lex, UnaryIdx, BinaryIdx, C, Beam, TopK, Trees, Print, Row).

row_parsed(R) :- R.parsed == true.
