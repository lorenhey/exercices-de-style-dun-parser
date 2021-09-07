#!/usr/bin/env escript
%% -*- erlang -*-
%% Parser V7 - Erlang escript
%% CKY + beam + unary-closure + tokenización (al/del + enclíticos 1)
%% Consume lexicon.json + grammar.json (mismos archivos neutrales del experimento).
%%
%% Uso:
%%   ./parser_v7.escript --file corpus.txt --print --json out.json
%%   ./parser_v7.escript --text "..." --trees --print

-module(parser_v7).
-export([main/1]).

-define(DEFAULT_LEX, "lexicon.json").
-define(DEFAULT_GRAM, "grammar.json").
-define(DEFAULT_FILE, "corpus.txt").

-record(symtab, {m = #{}, id2s = array:new(), next = 0}).
-record(arena,  {arr = array:new(), next = 0}).

-record(rule, {
    lhs,
    rhs_len,
    rhs1,
    rhs2 = undefined,
    weight = 0.0,
    op = empty,
    arg_key = undefined,
    arg_val = undefined,
    arg_type = undefined,
    post = 0  %% bit 1 = PROPAGATE_IDX_TO_RIGHT
}).

-record(token, {raw = "", text = "", index = 0}).
-record(node, {
    label,
    is_leaf = false,
    leaf_raw = undefined,
    feats = [],
    score = 0.0,
    left = undefined,
    right = undefined,
    child = undefined
}).

-record(item, {cat, feats, feats_h, score, node}).

%% ============================================================
%% main / CLI
%% ============================================================

main(Args) ->
    Opts = parse_args(Args, #{
        lex => ?DEFAULT_LEX,
        grammar => ?DEFAULT_GRAM,
        file => ?DEFAULT_FILE,
        text => undefined,
        json => undefined,
        beam => 16,
        topk => 1,
        trees => false,
        print => false
    }),
    LexPath = maps:get(lex, Opts),
    GramPath = maps:get(grammar, Opts),

    {St1, Lexicon} = load_lexicon(symtab_new(), LexPath),
    {St2, Grammar} = load_grammar(St1, GramPath),
    {St3, Const}   = ensure_constants(St2),

    Corpus =
        case maps:get(text, Opts) of
            undefined -> read_file_utf8(maps:get(file, Opts));
            T when is_list(T) -> T
        end,

    Sentences = split_sentences(Corpus),

    Beam = maps:get(beam, Opts),
    TopK = maps:get(topk, Opts),
    WantTrees = maps:get(trees, Opts),
    WantPrint = maps:get(print, Opts),
    JsonOut = maps:get(json, Opts),

    {Rows, ParsedCount, TotTok, TotOov, TotTimeMs} =
        parse_all(Sentences, St3, Const, Lexicon, Grammar, TopK, Beam, WantTrees, WantPrint),

    SentN = length(Sentences),
    Coverage = if SentN =:= 0 -> 0.0; true -> ParsedCount / SentN end,
    AvgTok = if SentN =:= 0 -> 0.0; true -> TotTok / SentN end,
    AvgOov = if SentN =:= 0 -> 0.0; true -> TotOov / SentN end,
    AvgTime = if SentN =:= 0 -> 0.0; true -> TotTimeMs / SentN end,

    case WantPrint of
        true ->
            io:format("==============================================================================~n"),
            io:format("SUMMARY~nsentences=~p  coverage=~.3f  avg_tokens=~.2f  avg_oov=~.2f  avg_time_ms=~.1f  beam=~p  top_k=~p~n",
                      [SentN, Coverage, AvgTok, AvgOov, AvgTime, Beam, TopK]);
        false ->
            io:format("SUMMARY: sentences=~p coverage=~.3f avg_time_ms=~.1f beam=~p top_k=~p~n",
                      [SentN, Coverage, AvgTime, Beam, TopK])
    end,

    case JsonOut of
        undefined -> ok;
        Path ->
            SummaryMap = #{
                <<"sentences">> => SentN,
                <<"coverage">> => Coverage,
                <<"avgTokens">> => AvgTok,
                <<"avgOov">> => AvgOov,
                <<"totalTimeMs">> => TotTimeMs,
                <<"avgTimeMs">> => AvgTime,
                <<"beam">> => Beam,
                <<"topK">> => TopK,
                <<"rows">> => Rows
            },
            Bin = iolist_to_binary(json_encode(SummaryMap)),
            ok = file:write_file(Path, Bin),
            io:format("Wrote JSON: ~s~n", [Path])
    end.

usage() ->
    io:format(
"Uso:
  ./parser_v7.escript [--lex lexicon.json] [--grammar grammar.json]
                      [--file corpus.txt | --text \"...\"]
                      [--beam 16] [--topk 1] [--trees] [--print] [--json out.json]

Ejemplos:
  ./parser_v7.escript --file corpus.txt --print --json out.json
  ./parser_v7.escript --text \"Los científicos lo estudiaron durante décadas sin comprenderlo.\" --trees --print
").

parse_args(["--help"|_], _) -> usage(), halt(0);
parse_args(["-h"|_], _) -> usage(), halt(0);
parse_args([], Opts) -> Opts;
parse_args(["--lex", V | T], Opts) -> parse_args(T, Opts#{lex => V});
parse_args(["--grammar", V | T], Opts) -> parse_args(T, Opts#{grammar => V});
parse_args(["--file", V | T], Opts) -> parse_args(T, Opts#{file => V});
parse_args(["--text", V | T], Opts) -> parse_args(T, Opts#{text => V});
parse_args(["--json", V | T], Opts) -> parse_args(T, Opts#{json => V});
parse_args(["--beam", V | T], Opts) -> parse_args(T, Opts#{beam => list_to_integer(V)});
parse_args(["--topk", V | T], Opts) -> parse_args(T, Opts#{topk => list_to_integer(V)});
parse_args(["--trees" | T], Opts) -> parse_args(T, Opts#{trees => true});
parse_args(["--print" | T], Opts) -> parse_args(T, Opts#{print => true});
parse_args([X|_], _) -> die(io_lib:format("Arg desconocido: ~p~n", [X])).

die(MsgIolist) ->
    io:format("~s", [iolist_to_binary(MsgIolist)]),
    halt(1).

read_file_utf8(Path) ->
    case file:read_file(Path) of
        {ok, Bin} -> unicode:characters_to_list(Bin);
        _ -> die(io_lib:format("No puedo abrir: ~s~n", [Path]))
    end.

%% ============================================================
%% Symtab
%% ============================================================

symtab_new() -> #symtab{}.

symtab_intern(St = #symtab{m = M, id2s = A, next = N}, S) ->
    case maps:get(S, M, undefined) of
        undefined ->
            A2 = array:set(N, S, A),
            M2 = maps:put(S, N, M),
            {N, St#symtab{m = M2, id2s = A2, next = N + 1}};
        Id ->
            {Id, St}
    end.

symtab_str(#symtab{id2s = A}, Id) ->
    case array:get(Id, A) of
        undefined -> "?";
        S -> S
    end.

is_var(St, Id) ->
    S = symtab_str(St, Id),
    case S of
        [$?|_] -> true;
        _ -> false
    end.

%% ============================================================
%% Feats: list({KeyId, ValId}) sorted
%% ============================================================

feats_sort(Fs) ->
    lists:sort(fun({K1,V1},{K2,V2}) -> (K1 < K2) orelse (K1 =:= K2 andalso V1 < V2) end, Fs).

feats_find(Key, Fs) ->
    case lists:keyfind(Key, 1, Fs) of
        false -> none;
        {_, V} -> {ok, V}
    end.

fnv64_step(H, X) ->
    H1 = H bxor X,
    (H1 * 1099511628211) band 16#FFFFFFFFFFFFFFFF.

feats_hash(Fs) ->
    H0 = 1469598103934665603,
    lists:foldl(fun({K,V}, H) ->
        H2 = fnv64_step(H, K),
        fnv64_step(H2, V)
    end, H0, Fs).

feats_unify(St, A, B) ->
    %% A y B sorted; unificación simple con valores variables "?x"
    try
        Merged = lists:foldl(fun({K,VB}, Acc) ->
            case lists:keyfind(K, 1, Acc) of
                false ->
                    feats_sort([{K,VB}|Acc]);
                {K,VA} ->
                    if VA =:= VB ->
                        Acc;
                    true ->
                        VAvar = is_var(St, VA),
                        VBvar = is_var(St, VB),
                        case {VAvar, VBvar} of
                            {true, false} ->
                                lists:keyreplace(K, 1, Acc, {K,VB});
                            {false, true} ->
                                Acc;
                            {true, true} ->
                                Acc;
                            {false, false} ->
                                throw(conflict)
                        end
                    end
            end
        end, A, B),
        {ok, feats_sort(Merged)}
    catch
        throw:conflict -> fail
    end.

feats_require(St, Fs, Key, Val) ->
    feats_unify(St, Fs, feats_sort([{Key,Val}])).

feats_replace_value(Fs, FromVal, ToVal) ->
    feats_sort([ case V of
                    FromVal -> {K, ToVal};
                    _ -> {K, V}
                 end || {K,V} <- Fs ]).

%% ============================================================
%% JSON (decoder mínimo + encoder)
%% - decode: maps/lists/strings/numbers/bools/null
%% ============================================================

json_decode(Text) when is_list(Text) ->
    {Val, Rest} = jval(skip_ws(Text)),
    _ = skip_ws(Rest),
    Val.

skip_ws([C|T]) when C =:= $\s; C =:= $\t; C =:= $\n; C =:= $\r -> skip_ws(T);
skip_ws(L) -> L.

jval([$"|T]) -> jstr(T, []);
jval([${|T]) -> jobj(skip_ws(T), #{});
jval([$[|T]) -> jarr(skip_ws(T), []);
jval([$t,$r,$u,$e|T]) -> {true, T};
jval([$f,$a,$l,$s,$e|T]) -> {false, T};
jval([$n,$u,$l,$l|T]) -> {null, T};
jval([C|_]=L) when (C >= $0 andalso C =< $9) orelse C =:= $- -> jnum(L, []);
jval(_) -> die("JSON inválido\n").

jstr([$"|T], Acc) -> {lists:reverse(Acc), T};
jstr([$\\,C|T], Acc) ->
    case C of
        $"  -> jstr(T, [$"|Acc]);
        $\\ -> jstr(T, [$\\|Acc]);
        $/  -> jstr(T, [$/|Acc]);
        $b  -> jstr(T, [$\b|Acc]);
        $f  -> jstr(T, [$\f|Acc]);
        $n  -> jstr(T, [$\n|Acc]);
        $r  -> jstr(T, [$\r|Acc]);
        $t  -> jstr(T, [$\t|Acc]);
        $u  ->
            {Cp, T2} = ju4(T),
            jstr(T2, [Cp|Acc]);
        _ -> die("JSON string escape inválido\n")
    end;
jstr([C|T], Acc) -> jstr(T, [C|Acc]);
jstr([], _) -> die("JSON string sin cierre\n").

ju4([A,B,C,D|T]) ->
    {hex4(A,B,C,D), T};
ju4(_) -> die("JSON \\uXXXX inválido\n").

hexv(H) when H >= $0, H =< $9 -> H - $0;
hexv(H) when H >= $A, H =< $F -> 10 + (H - $A);
hexv(H) when H >= $a, H =< $f -> 10 + (H - $a);
hexv(_) -> die("Hex inválido\n").

hex4(A,B,C,D) ->
    (hexv(A) bsl 12) bor (hexv(B) bsl 8) bor (hexv(C) bsl 4) bor hexv(D).

jobj([$}|T], Obj) -> {Obj, T};
jobj(L, Obj) ->
    {K, L1} = jval(L),
    L2 = skip_ws(L1),
    case L2 of
        [$:|L3] ->
            {V, L4} = jval(skip_ws(L3)),
            Obj2 = maps:put(K, V, Obj),
            L5 = skip_ws(L4),
            case L5 of
                [$,|L6] -> jobj(skip_ws(L6), Obj2);
                [$}|L6] -> {Obj2, L6};
                _ -> die("JSON objeto: falta , o }\n")
            end;
        _ -> die("JSON objeto: falta :\n")
    end.

jarr([$]|T], Acc) -> {lists:reverse(Acc), T};
jarr(L, Acc) ->
    {V, L1} = jval(L),
    L2 = skip_ws(L1),
    case L2 of
        [$,|L3] -> jarr(skip_ws(L3), [V|Acc]);
        [$]|L3] -> {lists:reverse([V|Acc]), L3};
        _ -> die("JSON array: falta , o ]\n")
    end.

jnum([C|T], Acc) when (C >= $0 andalso C =< $9) orelse C=:= $- orelse C=:= $+ orelse
                      C=:= $. orelse C=:= $e orelse C=:= $E ->
    jnum(T, [C|Acc]);
jnum(Rest, AccRev) ->
    S = lists:reverse(AccRev),
    IsFloat = lists:member($., S) orelse lists:member($e, S) orelse lists:member($E, S),
    N =
        case IsFloat of
            true -> list_to_float(S);
            false -> list_to_integer(S) * 1.0
        end,
    {N, Rest}.

json_encode(V) ->
    jenc(V).

jenc(null) -> <<"null">>;
jenc(true) -> <<"true">>;
jenc(false) -> <<"false">>;
jenc(N) when is_integer(N) -> integer_to_binary(N);
jenc(F) when is_float(F) ->
    list_to_binary(io_lib:format("~.15g", [F]));
jenc(S) when is_list(S) ->
    %% string
    [$"|jesc(S, [])] ++ [$"];
jenc(B) when is_binary(B) ->
    %% treat as string
    jenc(unicode:characters_to_list(B));
jenc(L) when is_list(L), L =:= [] ->
    <<"[]">>;
jenc(L) when is_list(L), is_tuple(hd(L)), tuple_size(hd(L)) =:= 2 ->
    %% proplist as object
    jenc_obj_from_pairs(L);
jenc(L) when is_list(L) ->
    %% array
    [$[, jenc_join([jenc(X) || X <- L], ",") , $]];
jenc(M) when is_map(M) ->
    Pairs = maps:to_list(M),
    jenc_obj_from_pairs(Pairs);
jenc(_) -> <<"null">>.

jenc_obj_from_pairs(Pairs) ->
    Inner = jenc_join([ [jenc_key(K), $:, jenc(V)] || {K,V} <- Pairs ], ","),
    [$\{, Inner, $\}].

jenc_key(K) when is_binary(K) -> jenc(K);
jenc_key(K) when is_list(K) -> jenc(K);
jenc_key(K) -> jenc(io_lib:format("~p",[K])).

jenc_join([], _) -> [];
jenc_join([X], _) -> X;
jenc_join([X|T], Sep) -> [X, Sep, jenc_join(T, Sep)].

jesc([], Acc) -> lists:reverse(Acc);
jesc([C|T], Acc) ->
    Esc =
        case C of
            $"  -> "\\\"";
            $\\ -> "\\\\";
            $\n -> "\\n";
            $\r -> "\\r";
            $\t -> "\\t";
            _   -> [C]
        end,
    jesc(T, lists:reverse(Esc) ++ Acc).

%% ============================================================
%% Load lexicon.json / grammar.json
%% ============================================================

load_lexicon(St0, Path) ->
    Text = read_file_utf8(Path),
    Root = json_decode(Text),
    Entries0 = maps:get("entries", Root, undefined),
    Entries0 =:= undefined andalso die("lexicon.json: falta entries{}\n"),
    {St1, Lex} =
        maps:fold(fun(Word, Arr, {StAcc, LxAcc}) ->
            {St2, List} = load_lex_entries(StAcc, Arr),
            {St2, maps:put(Word, List, LxAcc)}
        end, {St0, #{}}, Entries0),
    {St1, Lex}.

load_lex_entries(St0, Arr) when is_list(Arr) ->
    lists:foldl(fun(Obj, {StAcc, Out}) ->
        PosS = maps:get("pos", Obj),
        W = maps:get("weight", Obj),
        FeatsObj = maps:get("feats", Obj, #{}),
        {PosId, St1} = symtab_intern(StAcc, PosS),
        {St2, Feats} = load_feats(St1, FeatsObj),
        Entry = #{pos => PosId, weight => W, feats => Feats},
        {St2, [Entry|Out]}
    end, {St0, []}, Arr).

load_feats(St0, FeatsObj) when is_map(FeatsObj) ->
    maps:fold(fun(K, V, {StAcc, FsAcc}) ->
        {KId, St1} = symtab_intern(StAcc, K),
        {VId, St2} = symtab_intern(St1, V),
        {St2, [{KId, VId} | FsAcc]}
    end, {St0, []}, FeatsObj) of
        {StX, Fs} -> {StX, feats_sort(Fs)}
    end.

load_grammar(St0, Path) ->
    Text = read_file_utf8(Path),
    Root = json_decode(Text),
    Rules0 = maps:get("rules", Root, undefined),
    Rules0 =:= undefined andalso die("grammar.json: falta rules[]\n"),
    {St1, RulesList} =
        lists:foldl(fun(RObj, {StAcc, Out}) ->
            {St2, Rule} = load_rule(StAcc, RObj),
            {St2, [Rule|Out]}
        end, {St0, []}, Rules0),
    Rules = lists:reverse(RulesList),

    %% Índices para acelerar:
    UnaryIdx = lists:foldl(fun(#rule{rhs_len=1, rhs1=R1}=R, M) ->
                                maps:update_with(R1, fun(L) -> [R|L] end, [R], M);
                              (_, M) -> M
                           end, #{}, Rules),
    BinIdx = lists:foldl(fun(#rule{rhs_len=2, rhs1=A, rhs2=B}=R, M) ->
                                maps:update_with({A,B}, fun(L) -> [R|L] end, [R], M);
                              (_, M) -> M
                         end, #{}, Rules),

    Grammar = #{rules => Rules, unary => UnaryIdx, binary => BinIdx},
    {St1, Grammar}.

load_rule(St0, Obj) ->
    LhsS = maps:get("lhs", Obj),
    Rhs = maps:get("rhs", Obj),
    Weight = maps:get("weight", Obj),
    OpS = maps:get("op", Obj, "EMPTY"),
    Args = maps:get("args", Obj, #{}),
    Post = maps:get("post", Obj, []),

    {Lhs, St1} = symtab_intern(St0, LhsS),
    [R1|Rest] = Rhs,
    {Rhs1, St2} = symtab_intern(St1, R1),

    {RhsLen, Rhs2, St3} =
        case Rest of
            [] -> {1, undefined, St2};
            [R2] ->
                {Id2, StX} = symtab_intern(St2, R2),
                {2, Id2, StX};
            _ -> die("grammar.json: rhs len debe ser 1 o 2\n")
        end,

    Op = op_atom(OpS),

    {ArgKey, St4} = maybe_intern(St3, maps:get("key", Args, undefined)),
    {ArgVal, St5} = maybe_intern(St4, maps:get("value", Args, undefined)),
    {ArgType, St6} = maybe_intern(St5, maps:get("type", Args, undefined)),

    PostFlag = lists:foldl(fun(S, F) ->
        case S of
            "PROPAGATE_IDX_TO_RIGHT" -> F bor 1;
            _ -> F
        end
    end, 0, Post),

    {St6, #rule{
        lhs = Lhs, rhs_len = RhsLen, rhs1 = Rhs1, rhs2 = Rhs2,
        weight = Weight, op = Op, arg_key = ArgKey, arg_val = ArgVal, arg_type = ArgType,
        post = PostFlag
    }}.

maybe_intern(St, undefined) -> {undefined, St};
maybe_intern(St, S) -> symtab_intern(St, S).

op_atom("EMPTY") -> empty;
op_atom("LEFT") -> left;
op_atom("RIGHT") -> right;
op_atom("UNIFY") -> unify;
op_atom("REQUIRE_LEFT") -> require_left;
op_atom("REQUIRE_RIGHT") -> require_right;
op_atom("MAKE_GAP") -> make_gap;
op_atom("RELCLAUSE_OBL") -> relclause_obl;
op_atom(X) -> die(io_lib:format("op desconocido: ~p~n", [X])).

%% ============================================================
%% Constants / common symbols
%% ============================================================

ensure_constants(St0) ->
    Need = ["idx","?i","gap","obl","yes","fin","no","gen","num","sg",
            "TOK","S","VP_FIN","Pinf","VP_NF","VP","Cl",
            "N","PropN","Pron","Adv","Vi","Vt",
            "el","la","los","las","de","a"],
    {Ids, St1} = lists:foldl(fun(S, {Acc, St}) ->
        {Id, St2} = symtab_intern(St, S),
        {[{S,Id}|Acc], St2}
    end, {[], St0}, Need),
    Const = maps:from_list(Ids),
    {St1, Const}.

cid(Const, Key) ->
    maps:get(Key, Const).

%% ============================================================
%% Tokenización y split de oraciones
%% ============================================================

split_sentences(Corpus) ->
    split_sentences(Corpus, [], []).
split_sentences([], Cur, Acc) ->
    S = string:trim(lists:reverse(Cur)),
    Acc2 = if S =:= "" -> Acc; true -> [S|Acc] end,
    lists:reverse(Acc2);
split_sentences([$.|T], Cur, Acc) ->
    S = string:trim(lists:reverse(Cur)),
    Acc2 = if S =:= "" -> Acc; true -> [S|Acc] end,
    split_sentences(T, [], Acc2);
split_sentences([$\n|T], Cur, Acc) ->
    S = string:trim(lists:reverse(Cur)),
    Acc2 = if S =:= "" -> Acc; true -> [S|Acc] end,
    split_sentences(T, [], Acc2);
split_sentences([C|T], Cur, Acc) ->
    split_sentences(T, [C|Cur], Acc).

tokenize(Sent) ->
    %% Regex Unicode words: \p{L}+ ([-']\p{L}+)*  (global)
    {ok, RE} = re:compile("[\\p{L}]+(?:[-'][\\p{L}]+)*", [unicode]),
    case re:run(Sent, RE, [global, {capture, all, index}]) of
        nomatch -> [];
        {match, Caps} ->
            %% Caps = [[{Start,Len}], ...]  (en índices sobre lista)
            toks_from_caps(Sent, Caps, 0, [])
    end.

toks_from_caps(_Sent, [], _Idx, Acc) ->
    lists:reverse(Acc);
toks_from_caps(Sent, [[{Start,Len}]|T], Idx, Acc) ->
    Raw = lists:sublist(Sent, Start+1, Len),
    Low = string:lowercase(Raw),
    %% al/del
    case Low of
        "al" ->
            T1 = #token{raw="a", text="a", index=Idx},
            T2 = #token{raw="el", text="el", index=Idx+1},
            toks_from_caps(Sent, T, Idx+2, [T2,T1|Acc]);
        "del" ->
            T1 = #token{raw="de", text="de", index=Idx},
            T2 = #token{raw="el", text="el", index=Idx+1},
            toks_from_caps(Sent, T, Idx+2, [T2,T1|Acc]);
        _ ->
            {MaybeSplit, NewAcc, NewIdx} = enclitic_split(Raw, Low, Idx, Acc),
            case MaybeSplit of
                true -> toks_from_caps(Sent, T, NewIdx, NewAcc);
                false ->
                    toks_from_caps(Sent, T, Idx+1, [#token{raw=Raw, text=Low, index=Idx}|Acc])
            end
    end.

enclitic_split(Raw, Low, Idx, Acc) ->
    Clitics = ["me","te","se","lo","la","los","las","le","les","nos","os"],
    Best = longest_suffix(Low, Clitics, none),
    case Best of
        none -> {false, Acc, Idx};
        Cl ->
            ClLen = length(Cl),
            BaseLen = length(Low) - ClLen,
            if BaseLen > 2 ->
                Base = lists:sublist(Low, BaseLen),
                LooksVerb =
                    lists:suffix("ar", Base) orelse lists:suffix("er", Base) orelse lists:suffix("ir", Base) orelse
                    lists:suffix("ando", Base) orelse lists:suffix("iendo", Base),
                case LooksVerb of
                    true ->
                        RawBase = lists:sublist(Raw, BaseLen),
                        RawCl = lists:nthtail(BaseLen, Raw),
                        T1 = #token{raw=RawBase, text=string:lowercase(RawBase), index=Idx},
                        T2 = #token{raw=RawCl, text=string:lowercase(RawCl), index=Idx+1},
                        {true, [T2,T1|Acc], Idx+2};
                    false ->
                        {false, Acc, Idx}
                end;
               true ->
                {false, Acc, Idx}
            end
    end.

longest_suffix(_Word, [], Best) -> Best;
longest_suffix(Word, [C|T], Best) ->
    case lists:suffix(C, Word) of
        true ->
            Best2 = case Best of none -> C; B -> if length(C) > length(B) -> C; true -> B end end,
            longest_suffix(Word, T, Best2);
        false ->
            longest_suffix(Word, T, Best)
    end.

%% ============================================================
%% Arena helpers
%% ============================================================

arena_add(A = #arena{arr = Arr, next = N}, Node) ->
    Arr2 = array:set(N, Node, Arr),
    {N, A#arena{arr = Arr2, next = N+1}}.

arena_get(#arena{arr = Arr}, Id) ->
    array:get(Id, Arr).

arena_clone_replace(Arena0, NodeId, FromVal, ToVal) ->
    Node0 = arena_get(Arena0, NodeId),
    Feats2 = feats_replace_value(Node0#node.feats, FromVal, ToVal),

    {Child2, A1} =
        case Node0#node.child of
            undefined -> {undefined, Arena0};
            Cid -> arena_clone_replace(Arena0, Cid, FromVal, ToVal)
        end,
    {Left2, A2} =
        case Node0#node.left of
            undefined -> {undefined, A1};
            Lid -> arena_clone_replace(A1, Lid, FromVal, ToVal)
        end,
    {Right2, A3} =
        case Node0#node.right of
            undefined -> {undefined, A2};
            Rid -> arena_clone_replace(A2, Rid, FromVal, ToVal)
        end,

    Node2 = Node0#node{feats = Feats2, child = Child2, left = Left2, right = Right2},
    arena_add(A3, Node2).

arena_pretty(Arena, St, NodeId) ->
    iolist_to_binary(pretty_rec(Arena, St, NodeId, 0)).

pretty_rec(Arena, St, NodeId, Ind) ->
    N = arena_get(Arena, NodeId),
    Pad = lists:duplicate(Ind*2, $\s),
    case N#node.is_leaf of
        true ->
            [Pad, N#node.leaf_raw, $\n];
        false ->
            Label = symtab_str(St, N#node.label),
            FeatStr =
                case N#node.feats of
                    [] -> "";
                    Fs ->
                        Inside = string:join([ feat_kv(St, K, V) || {K,V} <- Fs ], ", "),
                        [" [", Inside, "]"]
                end,
            Head = [Pad, Label, FeatStr, io_lib:format("  (score=~.3f)~n", [N#node.score])],
            case N#node.child of
                undefined ->
                    L = case N#node.left of undefined -> []; Lid -> pretty_rec(Arena, St, Lid, Ind+1) end,
                    R = case N#node.right of undefined -> []; Rid -> pretty_rec(Arena, St, Rid, Ind+1) end,
                    [Head, L, R];
                Cid ->
                    [Head, pretty_rec(Arena, St, Cid, Ind+1)]
            end
    end.

feat_kv(St, K, V) ->
    lists:flatten([symtab_str(St, K), "=", symtab_str(St, V)]).

%% ============================================================
%% OOV Guess
%% ============================================================

is_det("el") -> true;
is_det("la") -> true;
is_det("los") -> true;
is_det("las") -> true;
is_det(_) -> false.

guess_lex(St0, Const, #token{raw=Raw, text=Low}) ->
    %% devuelve {St1, [EntryMaps]}
    {Prop, St1} =
        case Raw of
            [C|_] when C >= $A, C =< $Z, not is_det(Low) ->
                Pos = cid(Const, "PropN"),
                Num = cid(Const, "num"),
                Sg = cid(Const, "sg"),
                {#{pos => Pos, weight => 0.03, feats => feats_sort([{Num,Sg}])}, St0};
            _ ->
                {none, St0}
        end,

    {Adv, St2} =
        case lists:suffix("mente", Low) of
            true ->
                Pos = cid(Const, "Adv"),
                {#{pos => Pos, weight => -0.03, feats => []}, St1};
            false ->
                {none, St1}
        end,

    %% verb guesses
    {ViId, _} = symtab_intern(St2, "Vi"),
    {VtId, St3} = symtab_intern(St2, "Vt"),
    Fin = cid(Const, "fin"),
    No  = cid(Const, "no"),
    Obl = cid(Const, "obl"),
    BaseVFeats = feats_sort([{Fin,No},{Obl,No}]),

    Verb =
        case lists:suffix("ar", Low) orelse lists:suffix("er", Low) orelse lists:suffix("ir", Low) of
            true ->
                [#{pos => ViId, weight => -0.12, feats => BaseVFeats},
                 #{pos => VtId, weight => -0.14, feats => BaseVFeats}];
            false ->
                case lists:suffix("ando", Low) orelse lists:suffix("iendo", Low) of
                    true ->
                        [#{pos => ViId, weight => -0.14, feats => BaseVFeats},
                         #{pos => VtId, weight => -0.16, feats => BaseVFeats}];
                    false ->
                        []
                end
        end,

    %% fallback N
    Entries0 = [X || X <- [Prop, Adv], X =/= none] ++ Verb,
    {Entries, St4} =
        case Entries0 of
            [] ->
                Npos = cid(Const, "N"),
                Gen = cid(Const, "gen"),
                Num = cid(Const, "num"),
                {Vg, StA} = symtab_intern(St3, "?g"),
                {Vn, StB} = symtab_intern(StA, "?n"),
                { [#{pos => Npos, weight => -0.35, feats => feats_sort([{Gen,Vg},{Num,Vn}])}], StB };
            _ ->
                {Entries0, St3}
        end,
    {St4, Entries}.

%% ============================================================
%% Parser ops / sanity
%% ============================================================

apply_op(St, Const, #rule{op=empty}, _LF, _RF) ->
    {ok, []};
apply_op(_St, _Const, #rule{op=left}, LF, _RF) ->
    {ok, LF};
apply_op(_St, _Const, #rule{op=right}, _LF, RF) ->
    {ok, RF};
apply_op(St, _Const, #rule{op=unify}, LF, RF) ->
    feats_unify(St, LF, RF);
apply_op(St, _Const, #rule{op=require_left, arg_key=K, arg_val=V}, LF, _RF) when K =/= undefined, V =/= undefined ->
    feats_require(St, LF, K, V);
apply_op(St, _Const, #rule{op=require_right, arg_key=K, arg_val=V}, _LF, RF) when K =/= undefined, V =/= undefined ->
    feats_require(St, RF, K, V);
apply_op(St, Const, #rule{op=make_gap, arg_type=T}, _LF, _RF) when T =/= undefined ->
    Idx = cid(Const, "idx"),
    Qi  = cid(Const, "?i"),
    Gap = cid(Const, "gap"),
    {ok, feats_sort([{Idx,Qi},{Gap,T}])};
apply_op(St, Const, #rule{op=relclause_obl}, LF, RF) ->
    Obl = cid(Const, "obl"),
    Yes = cid(Const, "yes"),
    case feats_require(St, RF, Obl, Yes) of
        fail -> fail;
        {ok, _} ->
            Gap = cid(Const, "gap"),
            Vobl = cid(Const, "obl"),
            feats_unify(St, LF, feats_sort([{Gap, Vobl}]))
    end;
apply_op(_, _, _, _, _) ->
    fail.

%% sanity helpers
has_desc_label(Arena, NodeId, Label) ->
    N = arena_get(Arena, NodeId),
    case (not N#node.is_leaf) andalso (N#node.label =:= Label) of
        true -> true;
        false ->
            (N#node.child =/= undefined andalso has_desc_label(Arena, N#node.child, Label))
            orelse (N#node.left =/= undefined andalso has_desc_label(Arena, N#node.left, Label))
            orelse (N#node.right =/= undefined andalso has_desc_label(Arena, N#node.right, Label))
    end.

sanity_s_has_vpfin(Arena, St, Const, TreeId) ->
    S = cid(Const, "S"),
    VPFIN = cid(Const, "VP_FIN"),
    walk_s(Arena, TreeId, S, VPFIN).

walk_s(Arena, NodeId, S, VPFIN) ->
    N = arena_get(Arena, NodeId),
    FoundHere =
        (not N#node.is_leaf) andalso (N#node.label =:= S) andalso
        ((N#node.child =/= undefined andalso (arena_get(Arena, N#node.child))#node.label =:= VPFIN)
         orelse (N#node.left =/= undefined andalso (arena_get(Arena, N#node.left))#node.label =:= VPFIN)
         orelse (N#node.right =/= undefined andalso (arena_get(Arena, N#node.right))#node.label =:= VPFIN)),
    FoundHere orelse
        (N#node.child =/= undefined andalso walk_s(Arena, N#node.child, S, VPFIN))
        orelse (N#node.left =/= undefined andalso walk_s(Arena, N#node.left, S, VPFIN))
        orelse (N#node.right =/= undefined andalso walk_s(Arena, N#node.right, S, VPFIN)).

sanity_sin_takes_vpnf(Arena, Const, TreeId) ->
    Pinf = cid(Const, "Pinf"),
    VPNF = cid(Const, "VP_NF"),
    sanity_pinf(Arena, TreeId, Pinf, VPNF).

sanity_pinf(Arena, NodeId, Pinf, VPNF) ->
    N = arena_get(Arena, NodeId),
    OkHere =
        case (not N#node.is_leaf) andalso (N#node.label =:= Pinf) of
            true -> has_desc_label(Arena, NodeId, VPNF);
            false -> true
        end,
    OkHere andalso
        (N#node.child =:= undefined orelse sanity_pinf(Arena, N#node.child, Pinf, VPNF))
        andalso (N#node.left =:= undefined orelse sanity_pinf(Arena, N#node.left, Pinf, VPNF))
        andalso (N#node.right =:= undefined orelse sanity_pinf(Arena, N#node.right, Pinf, VPNF)).

sanity_enclitic_only_nf(Arena, Const, TreeId) ->
    VP = cid(Const, "VP"),
    Cl = cid(Const, "Cl"),
    Vt = cid(Const, "Vt"),
    Vi = cid(Const, "Vi"),
    sanity_enclitic(Arena, TreeId, VP, Cl, Vt, Vi).

sanity_enclitic(Arena, NodeId, VP, Cl, Vt, Vi) ->
    N = arena_get(Arena, NodeId),
    BadHere =
        (not N#node.is_leaf) andalso (N#node.label =:= VP) andalso
        (N#node.left =/= undefined) andalso (N#node.right =/= undefined) andalso
        begin
            LN = arena_get(Arena, N#node.left),
            RN = arena_get(Arena, N#node.right),
            (not RN#node.is_leaf) andalso (RN#node.label =:= Cl) andalso
            (not LN#node.is_leaf) andalso ((LN#node.label =:= Vt) orelse (LN#node.label =:= Vi))
        end,
    (not BadHere) andalso
        (N#node.child =:= undefined orelse sanity_enclitic(Arena, N#node.child, VP, Cl, Vt, Vi))
        andalso (N#node.left =:= undefined orelse sanity_enclitic(Arena, N#node.left, VP, Cl, Vt, Vi))
        andalso (N#node.right =:= undefined orelse sanity_enclitic(Arena, N#node.right, VP, Cl, Vt, Vi)).

%% ============================================================
%% Chart / beam
%% Cell = map CatId -> #{items => [#item], hashes => [u64]}
%% ============================================================

cell_get_bucket(Cell, Cat) ->
    maps:get(Cat, Cell, undefined).

cell_put_bucket(Cell, Cat, Bk) ->
    maps:put(Cat, Bk, Cell).

bucket_new() -> #{items => [], hashes => []}.

bucket_has_hash(Bk, H) ->
    lists:member(H, maps:get(hashes, Bk)).

bucket_insert_sorted(Bk, It, Beam, Pruned0) ->
    Items0 = maps:get(items, Bk),
    Hashes0 = maps:get(hashes, Bk),
    {Items1, Hashes1} = insert_desc(Items0, Hashes0, It),
    {Items2, Hashes2, Pruned1} =
        case length(Items1) > Beam of
            true ->
                Drop = length(Items1) - Beam,
                {lists:sublist(Items1, Beam), lists:sublist(Hashes1, Beam), Pruned0 + Drop};
            false ->
                {Items1, Hashes1, Pruned0}
        end,
    {Bk#{items => Items2, hashes => Hashes2}, Pruned1}.

insert_desc([], [], It) -> {[It], [It#item.feats_h]};
insert_desc([HIt|TIt]=Items, [HH|TH]=Hashes, It) ->
    case It#item.score > HIt#item.score of
        true -> {[It|Items], [It#item.feats_h|Hashes]};
        false ->
            {T2, TH2} = insert_desc(TIt, TH, It),
            {[HIt|T2], [HH|TH2]}
    end.

cell_add_item(Cell, It, Beam, Pruned0) ->
    Cat = It#item.cat,
    Bk0 = case cell_get_bucket(Cell, Cat) of undefined -> bucket_new(); B -> B end,
    case bucket_has_hash(Bk0, It#item.feats_h) of
        true -> {Cell, Pruned0};
        false ->
            {Bk1, Pruned1} = bucket_insert_sorted(Bk0, It, Beam, Pruned0),
            {cell_put_bucket(Cell, Cat, Bk1), Pruned1}
    end.

%% ============================================================
%% Unary closure (indexado)
%% ============================================================

unary_closure(Cell0, St, Const, Grammar, Arena0, Beam, Pruned0, UnaryApps0) ->
    UnaryIdx = maps:get(unary, Grammar),
    unary_loop(Cell0, St, Const, UnaryIdx, Arena0, Beam, Pruned0, UnaryApps0).

unary_loop(Cell, St, Const, UnaryIdx, Arena, Beam, Pruned, UnaryApps) ->
    %% intentamos expandir hasta punto fijo
    {Cell2, Arena2, Pruned2, UnaryApps2, Changed} =
        maps:fold(fun(RhsCat, Bk, {CAcc, AAcc, PAcc, UAcc, ChAcc}) ->
            Rules = maps:get(RhsCat, UnaryIdx, []),
            ItemsSnap = maps:get(items, Bk),
            lists:foldl(fun(R, {C1, A1, P1, U1, Ch1}) ->
                lists:foldl(fun(ChildIt, {C2, A2, P2, U2, Ch2}) ->
                    case apply_op(St, Const, R, ChildIt#item.feats, []) of
                        fail ->
                            {C2, A2, P2, U2, Ch2};
                        {ok, PF} ->
                            U3 = U2 + 1,
                            Score = ChildIt#item.score + R#rule.weight,
                            Node = #node{label=R#rule.lhs, feats=PF, score=Score, child=ChildIt#item.node},
                            {NodeId, A3} = arena_add(A2, Node),
                            It = #item{cat=R#rule.lhs, feats=PF, feats_h=feats_hash(PF), score=Score, node=NodeId},
                            {C3, P3} = cell_add_item(C2, It, Beam, P2),
                            %% changed si aumentó el bucket lhs (aprox: si no dedupe)
                            {C3, A3, P3, U3, true}
                    end
                end, {C1, A1, P1, U1, Ch1}, ItemsSnap)
            end, {CAcc, AAcc, PAcc, UAcc, ChAcc}, Rules)
        end, {Cell, Arena, Pruned, UnaryApps, false}, Cell),

    case Changed of
        true -> unary_loop(Cell2, St, Const, UnaryIdx, Arena2, Beam, Pruned2, UnaryApps2);
        false -> {Cell2, Arena2, Pruned2, UnaryApps2}
    end.

%% ============================================================
%% Parse
%% ============================================================

parse_all(Sents, St0, Const, Lexicon, Grammar, TopK, Beam, WantTrees, WantPrint) ->
    parse_all(Sents, St0, Const, Lexicon, Grammar, TopK, Beam, WantTrees, WantPrint,
              [], 0, 0.0, 0.0, 0.0).

parse_all([], _St, _Const, _Lex, _Gram, _TopK, _Beam, _Trees, _Print,
          RowsAcc, ParsedAcc, TotTok, TotOov, TotTime) ->
    {lists:reverse(RowsAcc), ParsedAcc, TotTok, TotOov, TotTime};
parse_all([S|T], St0, Const, Lexicon, Grammar, TopK, Beam, WantTrees, WantPrint,
          RowsAcc, ParsedAcc, TotTok, TotOov, TotTime) ->
    {Row, Parsed, TokN, OovN, TimeMs} =
        parse_sentence(S, St0, Const, Lexicon, Grammar, TopK, Beam, WantTrees, WantPrint),
    parse_all(T, St0, Const, Lexicon, Grammar, TopK, Beam, WantTrees, WantPrint,
              [Row|RowsAcc], ParsedAcc + Parsed, TotTok + TokN, TotOov + OovN, TotTime + TimeMs).

parse_sentence(Sent, St0, Const, Lexicon, Grammar, TopK, Beam, WantTrees, WantPrint) ->
    T0 = erlang:monotonic_time(microsecond),

    Tokens0 = tokenize(Sent),
    N = length(Tokens0),
    case N of
        0 ->
            Row = #{
                <<"sentence">> => Sent,
                <<"tokens">> => 0,
                <<"oovTokens">> => 0,
                <<"parsed">> => false,
                <<"nParsesReturned">> => 0,
                <<"bestScore">> => null,
                <<"timeMs">> => 0.0,
                <<"chartItemsTotal">> => 0,
                <<"chartItemsMaxCell">> => 0,
                <<"prunedByBeam">> => 0,
                <<"unaryApplications">> => 0,
                <<"ambiguousCells">> => 0,
                <<"sanitySHasVpFin">> => false,
                <<"sanitySinTakesVpNf">> => false,
                <<"sanityEncliticOnlyNf">> => false,
                <<"notes">> => [<<"empty">>],
                <<"bestTree">> => null
            },
            {Row, 0, 0.0, 0.0, 0.0};
        _ ->
            %% intern "t0..t{n-1}" por oración para idx
            {St1, TIds} = intern_tids(St0, N, #{}),
            Tokens = renumber_tokens(Tokens0, 0, []),

            Chart0 = chart_new(N),
            Arena0 = #arena{},

            %% lexical init
            {Chart1, Arena1, OovCount, Pruned1, UnaryApps1} =
                lex_init(0, N, Tokens, St1, Const, Lexicon, Grammar, Arena0, Chart0, Beam, 0, 0, TIds),

            %% CKY
            {Chart2, Arena2, Pruned2, UnaryApps2} =
                cky(2, N, St1, Const, Grammar, Chart1, Arena1, Beam, Pruned1, UnaryApps1),

            %% metrics
            {TotalItems, MaxCellItems, AmbCells} = chart_metrics(Chart2, N),

            %% best S
            Ssym = cid(Const, "S"),
            CellSN = chart_get(Chart2, 0, N, N),
            Best =
                case maps:get(Ssym, CellSN, undefined) of
                    undefined -> none;
                    Bk -> case maps:get(items, Bk) of [] -> none; [It|_] -> It end
                end,

            {Parsed, NParses, BestScore, Notes, TreeStr, San1, San2, San3} =
                case Best of
                    none ->
                        {false, 0, null, [<<"NO_PARSE">>], null, false, false, false};
                    #item{score=Sc, node=TreeId} ->
                        %% sanity
                        S1 = sanity_s_has_vpfin(Arena2, St1, Const, TreeId),
                        S2 = sanity_sin_takes_vpnf(Arena2, Const, TreeId),
                        S3 = sanity_enclitic_only_nf(Arena2, Const, TreeId),
                        Notes0 = [],
                        Notes1 = if S1 -> Notes0; true -> [<<"WARN: S sin VP_FIN visible">>|Notes0] end,
                        Notes2 = if S2 -> Notes1; true -> [<<"WARN: 'sin' sin VP_NF bajo Pinf">>|Notes1] end,
                        Notes3 = if S3 -> Notes2; true -> [<<"WARN: enclítico con verbo finito">>|Notes2] end,
                        BT = case WantTrees of true -> arena_pretty(Arena2, St1, TreeId); false -> null end,
                        %% topk
                        ItemsS = maps:get(items, maps:get(Ssym, CellSN)),
                        {true, min(TopK, length(ItemsS)), Sc, lists:reverse(Notes3), BT, S1, S2, S3}
                end,

            T1 = erlang:monotonic_time(microsecond),
            TimeMs = (T1 - T0) / 1000.0,

            Row = #{
                <<"sentence">> => Sent,
                <<"tokens">> => N,
                <<"oovTokens">> => OovCount,
                <<"parsed">> => Parsed,
                <<"nParsesReturned">> => NParses,
                <<"bestScore">> => BestScore,
                <<"timeMs">> => TimeMs,
                <<"chartItemsTotal">> => TotalItems,
                <<"chartItemsMaxCell">> => MaxCellItems,
                <<"prunedByBeam">> => Pruned2,
                <<"unaryApplications">> => UnaryApps2,
                <<"ambiguousCells">> => AmbCells,
                <<"sanitySHasVpFin">> => San1,
                <<"sanitySinTakesVpNf">> => San2,
                <<"sanityEncliticOnlyNf">> => San3,
                <<"notes">> => Notes,
                <<"bestTree">> => TreeStr
            },

            case WantPrint of
                true ->
                    io:format("==============================================================================~n"),
                    io:format("~s~n", [Sent]),
                    io:format("tokens=~p  oov=~p  parsed=~p  parses=~p  bestScore=~p  time_ms=~.1f~n",
                              [N, OovCount, bool01(Parsed), NParses, BestScore, TimeMs]),
                    io:format("chart_items=~p  max_cell=~p  pruned=~p  unary_apps=~p  amb_cells=~p~n",
                              [TotalItems, MaxCellItems, Pruned2, UnaryApps2, AmbCells]),
                    case Notes of [] -> ok; _ -> io:format("notes: ~s~n", [string:join([binary_to_list(Nn) || Nn <- Notes], "; ")]) end,
                    case WantTrees of
                        true when TreeStr =/= null -> io:format("~s", [TreeStr]);
                        _ -> ok
                    end;
                false -> ok
            end,

            {Row, bool01(Parsed), N, OovCount, TimeMs}
    end.

bool01(true) -> 1;
bool01(false) -> 0.

min(A,B) when A =< B -> A;
min(_,B) -> B.

intern_tids(St0, N, Acc) ->
    intern_tids(St0, 0, N, Acc).
intern_tids(St, I, N, Acc) when I >= N -> {St, Acc};
intern_tids(St0, I, N, Acc) ->
    S = "t" ++ integer_to_list(I),
    {Id, St1} = symtab_intern(St0, S),
    intern_tids(St1, I+1, N, maps:put(I, Id, Acc)).

renumber_tokens([], _I, Acc) -> lists:reverse(Acc);
renumber_tokens([Tok|T], I, Acc) ->
    renumber_tokens(T, I+1, [Tok#token{index=I}|Acc]).

chart_new(N) ->
    array:from_list(lists:duplicate(N*(N+1), #{})).

cidx(I,J,N) -> I*(N+1) + J + 1.

chart_get(Chart, I, J, N) ->
    array:get(cidx(I,J,N)-1, Chart).

chart_set(Chart, I, J, N, Cell) ->
    array:set(cidx(I,J,N)-1, Cell, Chart).

chart_metrics(Chart, N) ->
    chart_metrics(Chart, N, 0, 0, 0).
chart_metrics(_Chart, N, I, Tot, Max, Amb) when I >= N*(N+1) ->
    {Tot, Max, Amb};
chart_metrics(Chart, N, K, Tot0, Max0, Amb0) ->
    Cell = array:get(K, Chart),
    CellItems = lists:sum([ length(maps:get(items, Bk)) || {_Cat,Bk} <- maps:to_list(Cell) ]),
    Tot1 = Tot0 + CellItems,
    Max1 = if CellItems > Max0 -> CellItems; true -> Max0 end,
    Amb1 = if map_size(Cell) >= 2 -> Amb0 + 1; true -> Amb0 end,
    chart_metrics(Chart, N, K+1, Tot1, Max1, Amb1).

%% ============================================================
%% Lexical init + CKY
%% ============================================================

lex_init(I, N, _Tokens, _St, _Const, _Lex, _Gram, Arena, Chart, _Beam, Oov, Pruned, UnaryApps, _TIds) when I >= N ->
    {Chart, Arena, Oov, Pruned, UnaryApps};
lex_init(I, N, Tokens, St, Const, Lexicon, Grammar, Arena0, Chart0, Beam, Oov0, Pruned0, UnaryApps0, TIds) ->
    Tok = lists:nth(I+1, Tokens),
    Cell0 = chart_get(Chart0, I, I+1, N),

    Word = Tok#token.text,
    Raw  = Tok#token.raw,
    InLex = maps:get(Word, Lexicon, undefined),
    Oov1 = case InLex of undefined -> Oov0 + 1; _ -> Oov0 end,

    %% emitir entradas (lex o guess)
    {Arena1, Cell1, Pruned1} =
        case InLex of
            undefined ->
                {St2, Guess} = guess_lex(St, Const, Tok),
                emit_entries(Guess, Tok, St2, Const, Arena0, Cell0, Beam, Pruned0, TIds);
            Entries ->
                emit_entries(Entries, Tok, St, Const, Arena0, Cell0, Beam, Pruned0, TIds)
        end,

    %% unary closure
    {Cell2, Arena2, Pruned2, UnaryApps2} =
        unary_closure(Cell1, St, Const, Grammar, Arena1, Beam, Pruned1, UnaryApps0),

    Chart1 = chart_set(Chart0, I, I+1, N, Cell2),
    lex_init(I+1, N, Tokens, St, Const, Lexicon, Grammar, Arena2, Chart1, Beam, Oov1, Pruned2, UnaryApps2, TIds).

emit_entries([], _Tok, _St, _Const, Arena, Cell, _Beam, Pruned, _TIds) -> {Arena, Cell, Pruned};
emit_entries([E|T], Tok, St, Const, Arena0, Cell0, Beam, Pruned0, TIds) ->
    {Arena1, Cell1, Pruned1} = emit_one(E, Tok, St, Const, Arena0, Cell0, Beam, Pruned0, TIds),
    emit_entries(T, Tok, St, Const, Arena1, Cell1, Beam, Pruned1, TIds).

emit_one(Entry, Tok, St, Const, Arena0, Cell0, Beam, Pruned0, TIds) ->
    Pos = maps:get(pos, Entry),
    W   = maps:get(weight, Entry),
    Fe0 = maps:get(feats, Entry),

    %% default idx para N/PropN/Pron
    Npos = cid(Const, "N"),
    Ppos = cid(Const, "PropN"),
    Pron = cid(Const, "Pron"),
    IdxK = cid(Const, "idx"),
    Fe1 =
        case (Pos =:= Npos) orelse (Pos =:= Ppos) orelse (Pos =:= Pron) of
            true ->
                case feats_find(IdxK, Fe0) of
                    none ->
                        Tid = maps:get(Tok#token.index, TIds),
                        case feats_unify(St, Fe0, feats_sort([{IdxK,Tid}])) of
                            {ok, FeU} -> FeU;
                            fail -> Fe0
                        end;
                    _ -> Fe0
                end;
            false -> Fe0
        end,

    Leaf = #node{label=cid(Const,"TOK"), is_leaf=true, leaf_raw=Tok#token.raw, score=W},
    {LeafId, Arena1} = arena_add(Arena0, Leaf),

    Pre = #node{label=Pos, feats=Fe1, score=W, child=LeafId},
    {PreId, Arena2} = arena_add(Arena1, Pre),

    It = #item{cat=Pos, feats=Fe1, feats_h=feats_hash(Fe1), score=W, node=PreId},
    {Cell1, Pruned1} = cell_add_item(Cell0, It, Beam, Pruned0),

    {Arena2, Cell1, Pruned1}.

cky(Span, N, _St, _Const, _Grammar, Chart, Arena, _Beam, Pruned, UnaryApps) when Span > N ->
    {Chart, Arena, Pruned, UnaryApps};
cky(Span, N, St, Const, Grammar, Chart0, Arena0, Beam, Pruned0, UnaryApps0) ->
    {Chart1, Arena1, Pruned1, UnaryApps1} =
        cky_span(0, Span, N, St, Const, Grammar, Chart0, Arena0, Beam, Pruned0, UnaryApps0),
    cky(Span+1, N, St, Const, Grammar, Chart1, Arena1, Beam, Pruned1, UnaryApps1).

cky_span(I, _Span, N, _St, _Const, _Grammar, Chart, Arena, _Beam, Pruned, UnaryApps) when I >= N ->
    {Chart, Arena, Pruned, UnaryApps};
cky_span(I, Span, N, St, Const, Grammar, Chart0, Arena0, Beam, Pruned0, UnaryApps0) ->
    J = I + Span,
    if J =< N ->
        Cell0 = chart_get(Chart0, I, J, N),
        {Cell1, Arena1, Pruned1} = cky_splits(I, J, N, St, Const, Grammar, Chart0, Arena0, Beam, Pruned0),
        {Cell2, Arena2, Pruned2, UnaryApps2} = unary_closure(Cell1, St, Const, Grammar, Arena1, Beam, Pruned1, UnaryApps0),
        Chart1 = chart_set(Chart0, I, J, N, Cell2),
        cky_span(I+1, Span, N, St, Const, Grammar, Chart1, Arena2, Beam, Pruned2, UnaryApps2);
       true ->
        cky_span(I+1, Span, N, St, Const, Grammar, Chart0, Arena0, Beam, Pruned0, UnaryApps0)
    end.

cky_splits(I, J, N, St, Const, Grammar, Chart, Arena0, Beam, Pruned0) ->
    cky_split(I, J, I+1, N, St, Const, Grammar, Chart, Arena0, Beam, Pruned0, chart_get(Chart, I, J, N)).

cky_split(_I, _J, K, _N, _St, _Const, _Grammar, _Chart, Arena, _Beam, Pruned, CellAcc) when K =:= undefined ->
    {CellAcc, Arena, Pruned};
cky_split(I, J, K, N, St, Const, Grammar, Chart, Arena0, Beam, Pruned0, CellAcc0) ->
    if K >= J ->
        {CellAcc0, Arena0, Pruned0};
       true ->
        LCell = chart_get(Chart, I, K, N),
        RCell = chart_get(Chart, K, J, N),
        case (map_size(LCell) =:= 0) orelse (map_size(RCell) =:= 0) of
            true ->
                cky_split(I, J, K+1, N, St, Const, Grammar, Chart, Arena0, Beam, Pruned0, CellAcc0);
            false ->
                BinIdx = maps:get(binary, Grammar),
                {CellAcc1, Arena1, Pruned1} =
                    maps:fold(fun(CatL, BkL, {CAcc, AAcc, PAcc}) ->
                        maps:fold(fun(CatR, BkR, {CAcc2, AAcc2, PAcc2}) ->
                            Rules = maps:get({CatL,CatR}, BinIdx, []),
                            case Rules of
                                [] -> {CAcc2, AAcc2, PAcc2};
                                _ ->
                                    ItemsL = maps:get(items, BkL),
                                    ItemsR = maps:get(items, BkR),
                                    combine_rules(Rules, ItemsL, ItemsR, St, Const, AAcc2, CAcc2, Beam, PAcc2)
                            end
                        end, {CAcc, AAcc, PAcc}, RCell)
                    end, {CellAcc0, Arena0, Pruned0}, LCell),
                cky_split(I, J, K+1, N, St, Const, Grammar, Chart, Arena1, Beam, Pruned1, CellAcc1)
        end
    end.

combine_rules([], _L, _R, _St, _Const, Arena, Cell, _Beam, Pruned) ->
    {Cell, Arena, Pruned};
combine_rules([R|T], ItemsL, ItemsR, St, Const, Arena0, Cell0, Beam, Pruned0) ->
    {Cell1, Arena1, Pruned1} = combine_items(R, ItemsL, ItemsR, St, Const, Arena0, Cell0, Beam, Pruned0),
    combine_rules(T, ItemsL, ItemsR, St, Const, Arena1, Cell1, Beam, Pruned1).

combine_items(_R, [], _ItemsR, _St, _Const, Arena, Cell, _Beam, Pruned) ->
    {Cell, Arena, Pruned};
combine_items(R, [IL|TL], ItemsR, St, Const, Arena0, Cell0, Beam, Pruned0) ->
    {Cell1, Arena1, Pruned1} =
        lists:foldl(fun(IR, {CAcc, AAcc, PAcc}) ->
            case apply_op(St, Const, R, IL#item.feats, IR#item.feats) of
                fail -> {CAcc, AAcc, PAcc};
                {ok, PF} ->
                    Score = IL#item.score + IR#item.score + R#rule.weight,

                    %% post: PROPAGATE_IDX_TO_RIGHT
                    {RightNodeId, A1} =
                        case (R#rule.post band 1) =/= 0 of
                            true ->
                                IdxK = cid(Const, "idx"),
                                Qi   = cid(Const, "?i"),
                                case feats_find(IdxK, IL#item.feats) of
                                    {ok, IdxV} -> arena_clone_replace(AAcc, IR#item.node, Qi, IdxV);
                                    none -> {IR#item.node, AAcc}
                                end;
                            false ->
                                {IR#item.node, AAcc}
                        end,

                    Node = #node{label=R#rule.lhs, feats=PF, score=Score, left=IL#item.node, right=RightNodeId},
                    {NodeId, A2} = arena_add(A1, Node),
                    It = #item{cat=R#rule.lhs, feats=PF, feats_h=feats_hash(PF), score=Score, node=NodeId},
                    {C2, P2} = cell_add_item(CAcc, It, Beam, PAcc),
                    {C2, A2, P2}
            end
        end, {Cell0, Arena0, Pruned0}, ItemsR),
    combine_items(R, TL, ItemsR, St, Const, Arena1, Cell1, Beam, Pruned1).

%% ============================================================
%% JSON rows: convert Erlang strings/binaries coherently
%% ============================================================

%% Nota: json_encode ya soporta:
%% - mapas con claves binarias
%% - listas de mapas (arrays)
%% - strings como listas (se escapan)
%% En Row armamos claves binarias y valores:
%% - sentence: lista (string) -> OK
%% - notes: lista de binarios -> OK (encode como string)
%% - bestTree: binario o null -> OK
