program parser_v7;

{$mode delphiunicode}
{$H+}

uses
  SysUtils, Classes, Character,
  fpjson, jsonparser,
  Generics.Collections;

type
  TSymtab = class
  private
    FMap: TDictionary<string, Integer>;
    FVec: TList<string>;
  public
    constructor Create;
    destructor Destroy; override;
    function Intern(const s: string): Integer;
    function Str(id: Integer): string;
    function IsVar(id: Integer): Boolean;
  end;

  TFeat = record
    K: Integer;
    V: Integer;
  end;

  TFeatList = class(TList<TFeat>)
  public
    procedure SortNorm;
  end;

  TLexEntry = class
  public
    Pos: Integer;
    Weight: Double;
    Feats: TFeatList;
    constructor Create;
    destructor Destroy; override;
  end;

  TRule = class
  public
    Lhs: Integer;
    RhsLen: Integer;
    Rhs1: Integer;
    Rhs2: Integer;          // only if RhsLen=2
    Weight: Double;
    Op: string;
    ArgKey: Integer;        // -1 if none
    ArgVal: Integer;        // -1 if none
    ArgType: Integer;       // -1 if none
    PropIdxToRight: Boolean;
  end;

  TToken = record
    Raw: string;
    Text: string;
    Index: Integer;
  end;

  TNode = class
  public
    LabelId: Integer;
    IsLeaf: Boolean;
    LeafRaw: string;
    Feats: TFeatList;
    Score: Double;
    LeftId: Integer;   // -1 if none
    RightId: Integer;  // -1 if none
    ChildId: Integer;  // -1 if none (unary)
    constructor Create;
    destructor Destroy; override;
  end;

  TArena = class
  private
    FNodes: TObjectList<TNode>;
  public
    constructor Create;
    destructor Destroy; override;
    function AddNode(N: TNode): Integer;
    function GetNode(id: Integer): TNode;
    function CloneReplace(rootId, fromV, toV: Integer): Integer;
    function Pretty(st: TSymtab; nodeId: Integer): string;
  end;

  TItem = record
    Cat: Integer;
    Feats: TFeatList;
    FeatsH: QWord;
    Score: Double;
    NodeId: Integer;
  end;

  TBucket = class
  public
    Items: TList<TItem>;                 // sorted desc by Score
    Hashes: TDictionary<QWord, Byte>;    // set
    constructor Create;
    destructor Destroy; override;
    function HasHash(h: QWord): Boolean;
    function Insert(const it: TItem; beam: Integer): Integer;
  end;

  TCell = TDictionary<Integer, TBucket>; // cat -> bucket

  TChart = array of TCell;

  TConsts = record
    idx, qi, gap, obl, yes, fin, no_, gen, num, sg: Integer;
    TOK, S, VP_FIN, Pinf, VP_NF, VP, Cl: Integer;
    N, PropN, Pron, Adv, Vi, Vt: Integer;
  end;

  TGrammar = class
  public
    Unary: TDictionary<Integer, TObjectList<TRule>>;    // rhs1 -> rules
    Binary: TDictionary<string, TObjectList<TRule>>;    // "a,b" -> rules
    constructor Create;
    destructor Destroy; override;
  end;

const
  FNV_OFF: QWord = 1469598103934665603;
  FNV_PR : QWord = 1099511628211;

function PairKey(a, b: Integer): string; inline;
begin
  Result := IntToStr(a) + ',' + IntToStr(b);
end;

{ =========================
  Symtab
  ========================= }

constructor TSymtab.Create;
begin
  inherited Create;
  FMap := TDictionary<string, Integer>.Create;
  FVec := TList<string>.Create;
end;

destructor TSymtab.Destroy;
begin
  FMap.Free;
  FVec.Free;
  inherited Destroy;
end;

function TSymtab.Intern(const s: string): Integer;
begin
  if FMap.TryGetValue(s, Result) then Exit;
  Result := FVec.Count;
  FVec.Add(s);
  FMap.Add(s, Result);
end;

function TSymtab.Str(id: Integer): string;
begin
  if (id >= 0) and (id < FVec.Count) then Result := FVec[id] else Result := '?';
end;

function TSymtab.IsVar(id: Integer): Boolean;
var s: string;
begin
  s := Str(id);
  Result := (Length(s) > 0) and (s[1] = '?');
end;

{ =========================
  Feats
  ========================= }

procedure TFeatList.SortNorm;
begin
  Sort(TComparer<TFeat>.Construct(
    function(const a, b: TFeat): Integer
    begin
      if a.K <> b.K then Exit(a.K - b.K);
      Result := a.V - b.V;
    end
  ));
end;

function FeatsFind(fs: TFeatList; key: Integer; out value: Integer): Boolean;
var f: TFeat;
begin
  for f in fs do
    if f.K = key then begin value := f.V; Exit(True); end;
  Result := False;
end;

function FeatsHash64(fs: TFeatList): QWord;
var h: QWord; f: TFeat;
begin
  h := FNV_OFF;
  for f in fs do begin
    h := (h xor QWord(f.K)) * FNV_PR;
    h := (h xor QWord(f.V)) * FNV_PR;
  end;
  Result := h;
end;

function FeatsReplaceValue(fs: TFeatList; fromV, toV: Integer): TFeatList;
var outF: TFeatList; f: TFeat;
begin
  outF := TFeatList.Create;
  outF.Capacity := fs.Count;
  for f in fs do begin
    if f.V = fromV then outF.Add(TFeat.Create(f.K, toV)) else outF.Add(f);
  end;
  outF.SortNorm;
  Result := outF;
end;

function FeatsUnify(st: TSymtab; a, b: TFeatList): TFeatList;
var mp: TDictionary<Integer,Integer>; f: TFeat; va, vb: Integer; have: Boolean;
    outF: TFeatList; kv: TPair<Integer,Integer>;
    vaVar, vbVar: Boolean;
begin
  mp := TDictionary<Integer,Integer>.Create;
  try
    for f in a do mp.AddOrSetValue(f.K, f.V);
    for f in b do begin
      have := mp.TryGetValue(f.K, va);
      vb := f.V;
      if not have then begin
        mp.Add(f.K, vb);
        Continue;
      end;
      if va = vb then Continue;
      vaVar := st.IsVar(va);
      vbVar := st.IsVar(vb);
      if vaVar and (not vbVar) then mp[f.K] := vb
      else if (not vaVar) and vbVar then begin end
      else if vaVar and vbVar then begin end
      else begin
        Exit(nil);
      end;
    end;

    outF := TFeatList.Create;
    outF.Capacity := mp.Count;
    for kv in mp do outF.Add(TFeat.Create(kv.Key, kv.Value));
    outF.SortNorm;
    Result := outF;
  finally
    mp.Free;
  end;
end;

function FeatsRequire(st: TSymtab; fs: TFeatList; k, v: Integer): TFeatList;
var tmp: TFeatList;
begin
  tmp := TFeatList.Create;
  tmp.Add(TFeat.Create(k, v));
  tmp.SortNorm;
  Result := FeatsUnify(st, fs, tmp);
  tmp.Free;
end;

{ =========================
  LexEntry
  ========================= }

constructor TLexEntry.Create;
begin
  inherited Create;
  Feats := TFeatList.Create;
end;

destructor TLexEntry.Destroy;
begin
  Feats.Free;
  inherited Destroy;
end;

{ =========================
  Node / Arena
  ========================= }

constructor TNode.Create;
begin
  inherited Create;
  Feats := TFeatList.Create;
  LeftId := -1; RightId := -1; ChildId := -1;
end;

destructor TNode.Destroy;
begin
  Feats.Free;
  inherited Destroy;
end;

constructor TArena.Create;
begin
  inherited Create;
  FNodes := TObjectList<TNode>.Create(True);
end;

destructor TArena.Destroy;
begin
  FNodes.Free;
  inherited Destroy;
end;

function TArena.AddNode(N: TNode): Integer;
begin
  Result := FNodes.Count;
  FNodes.Add(N);
end;

function TArena.GetNode(id: Integer): TNode;
begin
  Result := FNodes[id];
end;

function TArena.CloneReplace(rootId, fromV, toV: Integer): Integer;
var n0, n2: TNode;
    c2, l2, r2: Integer;
    feats2: TFeatList;
begin
  n0 := GetNode(rootId);
  feats2 := FeatsReplaceValue(n0.Feats, fromV, toV);

  c2 := -1; l2 := -1; r2 := -1;
  if n0.ChildId >= 0 then c2 := CloneReplace(n0.ChildId, fromV, toV);
  if n0.LeftId  >= 0 then l2 := CloneReplace(n0.LeftId,  fromV, toV);
  if n0.RightId >= 0 then r2 := CloneReplace(n0.RightId, fromV, toV);

  n2 := TNode.Create;
  n2.LabelId := n0.LabelId;
  n2.IsLeaf := n0.IsLeaf;
  n2.LeafRaw := n0.LeafRaw;
  n2.Score := n0.Score;
  n2.LeftId := l2;
  n2.RightId := r2;
  n2.ChildId := c2;
  n2.Feats.Free;
  n2.Feats := feats2; // take ownership
  Result := AddNode(n2);
end;

function TArena.Pretty(st: TSymtab; nodeId: Integer): string;
var sb: TStringBuilder;

  procedure Rec(nid, indent: Integer);
  var n: TNode; i: Integer;
  begin
    n := GetNode(nid);
    sb.Append(StringOfChar(' ', indent*2));
    if n.IsLeaf then begin
      sb.AppendLine(n.LeafRaw);
      Exit;
    end;
    sb.Append(st.Str(n.LabelId));
    if (n.Feats <> nil) and (n.Feats.Count > 0) then begin
      sb.Append(' [');
      for i := 0 to n.Feats.Count-1 do begin
        if i > 0 then sb.Append(', ');
        sb.Append(st.Str(n.Feats[i].K)); sb.Append('=');
        sb.Append(st.Str(n.Feats[i].V));
      end;
      sb.Append(']');
    end;
    sb.Append('  (score=');
    sb.Append(FormatFloat('0.000', n.Score));
    sb.AppendLine(')');

    if n.ChildId >= 0 then
      Rec(n.ChildId, indent+1)
    else begin
      if n.LeftId >= 0 then Rec(n.LeftId, indent+1);
      if n.RightId >= 0 then Rec(n.RightId, indent+1);
    end;
  end;

begin
  sb := TStringBuilder.Create;
  try
    Rec(nodeId, 0);
    Result := sb.ToString;
  finally
    sb.Free;
  end;
end;

{ =========================
  Bucket
  ========================= }

constructor TBucket.Create;
begin
  inherited Create;
  Items := TList<TItem>.Create;
  Hashes := TDictionary<QWord, Byte>.Create;
end;

destructor TBucket.Destroy;
begin
  Items.Free;
  Hashes.Free;
  inherited Destroy;
end;

function TBucket.HasHash(h: QWord): Boolean;
var dummy: Byte;
begin
  Result := Hashes.TryGetValue(h, dummy);
end;

function TBucket.Insert(const it: TItem; beam: Integer): Integer;
var i, pruned: Integer;
begin
  Hashes.AddOrSetValue(it.FeatsH, 1);
  i := 0;
  while (i < Items.Count) and (Items[i].Score >= it.Score) do Inc(i);
  Items.Insert(i, it);
  if Items.Count > beam then begin
    pruned := Items.Count - beam;
    Items.DeleteRange(beam, pruned);
    Exit(pruned);
  end;
  Result := 0;
end;

{ =========================
  Grammar
  ========================= }

constructor TGrammar.Create;
begin
  inherited Create;
  Unary := TDictionary<Integer, TObjectList<TRule>>.Create;
  Binary := TDictionary<string, TObjectList<TRule>>.Create;
end;

destructor TGrammar.Destroy;
var kvU: TPair<Integer, TObjectList<TRule>>;
    kvB: TPair<string, TObjectList<TRule>>;
begin
  for kvU in Unary do kvU.Value.Free;
  for kvB in Binary do kvB.Value.Free;
  Unary.Free;
  Binary.Free;
  inherited Destroy;
end;

{ =========================
  Constants
  ========================= }

function EnsureConstants(st: TSymtab): TConsts;
begin
  Result.idx := st.Intern('idx');
  Result.qi  := st.Intern('?i');
  Result.gap := st.Intern('gap');
  Result.obl := st.Intern('obl');
  Result.yes := st.Intern('yes');
  Result.fin := st.Intern('fin');
  Result.no_ := st.Intern('no');
  Result.gen := st.Intern('gen');
  Result.num := st.Intern('num');
  Result.sg  := st.Intern('sg');

  Result.TOK := st.Intern('TOK');
  Result.S := st.Intern('S');
  Result.VP_FIN := st.Intern('VP_FIN');
  Result.Pinf := st.Intern('Pinf');
  Result.VP_NF := st.Intern('VP_NF');
  Result.VP := st.Intern('VP');
  Result.Cl := st.Intern('Cl');

  Result.N := st.Intern('N');
  Result.PropN := st.Intern('PropN');
  Result.Pron := st.Intern('Pron');
  Result.Adv := st.Intern('Adv');
  Result.Vi := st.Intern('Vi');
  Result.Vt := st.Intern('Vt');
end;

{ =========================
  Tokenization (UnicodeString)
  ========================= }

function IsLetterU(ch: WideChar): Boolean;
begin
  Result := TCharacter.IsLetter(ch);
end;

function IsWordConnector(ch: WideChar): Boolean;
begin
  Result := (ch = '-') or (ch = '''');
end;

function LowerU(const s: UnicodeString): UnicodeString;
begin
  Result := UnicodeLowerCase(s);
end;

function EndsWithU(const s, suffix: UnicodeString): Boolean;
var ls, lf: Integer;
begin
  ls := Length(s); lf := Length(suffix);
  Result := (ls >= lf) and (Copy(s, ls-lf+1, lf) = suffix);
end;

function EncliticSplit(const rawU, lowU: UnicodeString; idx: Integer): TArray<TToken>;
const
  Clitics: array[0..11] of UnicodeString =
    ('me','te','se','lo','la','los','las','le','les','nos','os','');
var best: UnicodeString; c: UnicodeString; i: Integer;
    lw, lc, baseLen: Integer;
    baseU, rawBaseU, rawClU: UnicodeString;
    looksVerb: Boolean;
begin
  SetLength(Result, 0);
  best := '';
  for i := 0 to 10 do begin
    c := Clitics[i];
    if (c <> '') and EndsWithU(lowU, c) then
      if (best = '') or (Length(c) > Length(best)) then best := c;
  end;
  if best = '' then Exit;

  lw := Length(lowU);
  lc := Length(best);
  baseLen := lw - lc;
  if baseLen <= 2 then Exit;

  baseU := Copy(lowU, 1, baseLen);
  looksVerb := EndsWithU(baseU,'ar') or EndsWithU(baseU,'er') or EndsWithU(baseU,'ir') or
               EndsWithU(baseU,'ando') or EndsWithU(baseU,'iendo');
  if not looksVerb then Exit;

  rawBaseU := Copy(rawU, 1, baseLen);
  rawClU := Copy(rawU, baseLen+1, lc);

  SetLength(Result, 2);
  Result[0].Raw := UTF8Encode(rawBaseU);
  Result[0].Text := UTF8Encode(LowerU(rawBaseU));
  Result[0].Index := idx;
  Result[1].Raw := UTF8Encode(rawClU);
  Result[1].Text := UTF8Encode(LowerU(rawClU));
  Result[1].Index := idx+1;
end;

function Tokenize(const sent: string): TList<TToken>;
var u: UnicodeString;
    i, n, idx: Integer;
    ch: WideChar;
    start: Integer;
    rawU, lowU: UnicodeString;
    split: TArray<TToken>;
    t: TToken;
begin
  Result := TList<TToken>.Create;
  u := UTF8Decode(sent);
  i := 1; n := Length(u); idx := 0;

  while i <= n do begin
    while (i <= n) and (not IsLetterU(u[i])) do Inc(i);
    if i > n then Break;
    start := i;
    Inc(i);
    while (i <= n) and (IsLetterU(u[i]) or IsWordConnector(u[i])) do Inc(i);
    rawU := Copy(u, start, i-start);
    lowU := LowerU(rawU);

    if lowU = 'al' then begin
      t.Raw := 'a'; t.Text := 'a'; t.Index := idx; Result.Add(t); Inc(idx);
      t.Raw := 'el'; t.Text := 'el'; t.Index := idx; Result.Add(t); Inc(idx);
      Continue;
    end;
    if lowU = 'del' then begin
      t.Raw := 'de'; t.Text := 'de'; t.Index := idx; Result.Add(t); Inc(idx);
      t.Raw := 'el'; t.Text := 'el'; t.Index := idx; Result.Add(t); Inc(idx);
      Continue;
    end;

    split := EncliticSplit(rawU, lowU, idx);
    if Length(split) = 2 then begin
      Result.Add(split[0]);
      Result.Add(split[1]);
      Inc(idx, 2);
    end else begin
      t.Raw := UTF8Encode(rawU);
      t.Text := UTF8Encode(lowU);
      t.Index := idx;
      Result.Add(t);
      Inc(idx);
    end;
  end;
end;

function SplitSentences(const text: string): TList<string>;
var parts: TStringList; i: Integer; s: string;
begin
  Result := TList<string>.Create;
  parts := TStringList.Create;
  try
    parts.Text := StringReplace(text, '.', #10, [rfReplaceAll]);
    for i := 0 to parts.Count-1 do begin
      s := Trim(parts[i]);
      if s <> '' then Result.Add(s);
    end;
  finally
    parts.Free;
  end;
end;

{ =========================
  Load JSON lexicon / grammar
  ========================= }

function LoadJSON(const path: string): TJSONData;
var parser: TJSONParser; fs: TFileStream;
begin
  fs := TFileStream.Create(path, fmOpenRead or fmShareDenyWrite);
  try
    parser := TJSONParser.Create(fs, [joUTF8]);
    try
      Result := parser.Parse;
    finally
      parser.Free;
    end;
  finally
    fs.Free;
  end;
end;

function LoadLexicon(st: TSymtab; const path: string): TDictionary<string, TObjectList<TLexEntry>>;
var root: TJSONObject; entries: TJSONObject;
    i, j: Integer; word: string;
    arr: TJSONArray; obj: TJSONObject;
    le: TLexEntry; featsObj: TJSONObject; fk: string;
begin
  Result := TDictionary<string, TObjectList<TLexEntry>>.Create;
  root := TJSONObject(LoadJSON(path));
  try
    entries := root.Objects['entries'];
    for i := 0 to entries.Count-1 do begin
      word := entries.Names[i];
      arr := entries.Arrays[word];
      var list := TObjectList<TLexEntry>.Create(True);
      for j := 0 to arr.Count-1 do begin
        obj := TJSONObject(arr.Items[j]);
        le := TLexEntry.Create;
        le.Pos := st.Intern(obj.Strings['pos']);
        le.Weight := obj.Floats['weight'];
        if obj.Find('feats') <> nil then begin
          featsObj := obj.Objects['feats'];
          for var k := 0 to featsObj.Count-1 do begin
            fk := featsObj.Names[k];
            le.Feats.Add(TFeat.Create(st.Intern(fk), st.Intern(featsObj.Strings[fk])));
          end;
        end;
        le.Feats.SortNorm;
        list.Add(le);
      end;
      Result.AddOrSetValue(word, list);
    end;
  finally
    root.Free;
  end;
end;

function LoadGrammar(st: TSymtab; const path: string): TGrammar;
var root: TJSONObject; rulesA: TJSONArray;
    i: Integer; r: TJSONObject; rhsA: TJSONArray;
    ru: TRule; lhs, rhs1, rhs2: Integer;
    args: TJSONObject; post: TJSONArray;
    key: string;
begin
  Result := TGrammar.Create;
  root := TJSONObject(LoadJSON(path));
  try
    rulesA := root.Arrays['rules'];
    for i := 0 to rulesA.Count-1 do begin
      r := TJSONObject(rulesA.Items[i]);
      ru := TRule.Create;
      lhs := st.Intern(r.Strings['lhs']);
      rhsA := r.Arrays['rhs'];
      ru.RhsLen := rhsA.Count;
      if (ru.RhsLen <> 1) and (ru.RhsLen <> 2) then raise Exception.Create('grammar.json: rhs len debe ser 1 o 2');
      rhs1 := st.Intern(rhsA.Strings[0]);
      rhs2 := -1;
      if ru.RhsLen = 2 then rhs2 := st.Intern(rhsA.Strings[1]);

      ru.Lhs := lhs;
      ru.Rhs1 := rhs1;
      ru.Rhs2 := rhs2;
      ru.Weight := r.Floats['weight'];
      if r.Find('op') <> nil then ru.Op := r.Strings['op'] else ru.Op := 'EMPTY';

      ru.ArgKey := -1; ru.ArgVal := -1; ru.ArgType := -1;
      if r.Find('args') <> nil then begin
        args := r.Objects['args'];
        if args.Find('key') <> nil then ru.ArgKey := st.Intern(args.Strings['key']);
        if args.Find('value') <> nil then ru.ArgVal := st.Intern(args.Strings['value']);
        if args.Find('type') <> nil then ru.ArgType := st.Intern(args.Strings['type']);
      end;

      ru.PropIdxToRight := False;
      if r.Find('post') <> nil then begin
        post := r.Arrays['post'];
        for var p := 0 to post.Count-1 do
          if post.Strings[p] = 'PROPAGATE_IDX_TO_RIGHT' then ru.PropIdxToRight := True;
      end;

      if ru.RhsLen = 1 then begin
        if not Result.Unary.ContainsKey(rhs1) then
          Result.Unary.Add(rhs1, TObjectList<TRule>.Create(True));
        Result.Unary[rhs1].Add(ru);
      end else begin
        key := PairKey(rhs1, rhs2);
        if not Result.Binary.ContainsKey(key) then
          Result.Binary.Add(key, TObjectList<TRule>.Create(True));
        Result.Binary[key].Add(ru);
      end;
    end;
  finally
    root.Free;
  end;
end;

{ =========================
  OOV guess
  ========================= }

function DetWord(const low: string): Boolean;
begin
  Result := (low='el') or (low='la') or (low='los') or (low='las');
end;

function GuessLex(st: TSymtab; c: TConsts; const tok: TToken): TObjectList<TLexEntry>;
var rawU, lowU: UnicodeString; le: TLexEntry;
    baseV: TFeatList; vg, vn: Integer;
begin
  Result := TObjectList<TLexEntry>.Create(True);
  rawU := UTF8Decode(tok.Raw);
  lowU := UTF8Decode(tok.Text);

  if (Length(rawU) > 0) and TCharacter.IsUpper(rawU[1]) and (not DetWord(tok.Text)) then begin
    le := TLexEntry.Create;
    le.Pos := c.PropN;
    le.Weight := 0.03;
    le.Feats.Add(TFeat.Create(c.num, c.sg));
    le.Feats.SortNorm;
    Result.Add(le);
  end;

  if EndsWithU(lowU, 'mente') then begin
    le := TLexEntry.Create;
    le.Pos := c.Adv;
    le.Weight := -0.03;
    le.Feats.SortNorm;
    Result.Add(le);
  end;

  baseV := TFeatList.Create;
  baseV.Add(TFeat.Create(c.fin, c.no_));
  baseV.Add(TFeat.Create(c.obl, c.no_));
  baseV.SortNorm;

  if EndsWithU(lowU,'ar') or EndsWithU(lowU,'er') or EndsWithU(lowU,'ir') then begin
    le := TLexEntry.Create; le.Pos := c.Vi; le.Weight := -0.12; le.Feats.Free; le.Feats := baseV; Result.Add(le);
    le := TLexEntry.Create; le.Pos := c.Vt; le.Weight := -0.14; le.Feats.Free; le.Feats := TFeatList.Create; le.Feats.AddRange(baseV); le.Feats.SortNorm; Result.Add(le);
  end else if EndsWithU(lowU,'ando') or EndsWithU(lowU,'iendo') then begin
    le := TLexEntry.Create; le.Pos := c.Vi; le.Weight := -0.14; le.Feats.Free; le.Feats := baseV; Result.Add(le);
    le := TLexEntry.Create; le.Pos := c.Vt; le.Weight := -0.16; le.Feats.Free; le.Feats := TFeatList.Create; le.Feats.AddRange(baseV); le.Feats.SortNorm; Result.Add(le);
  end else begin
    baseV.Free;
  end;

  if Result.Count = 0 then begin
    vg := st.Intern('?g'); vn := st.Intern('?n');
    le := TLexEntry.Create;
    le.Pos := c.N;
    le.Weight := -0.35;
    le.Feats.Add(TFeat.Create(c.gen, vg));
    le.Feats.Add(TFeat.Create(c.num, vn));
    le.Feats.SortNorm;
    Result.Add(le);
  end;
end;

{ =========================
  Ops
  ========================= }

function ApplyOp(st: TSymtab; c: TConsts; ru: TRule; lf, rf: TFeatList): TFeatList;
var tmp: TFeatList;
begin
  Result := nil;
  if ru.Op = 'EMPTY' then begin
    Result := TFeatList.Create;
    Exit;
  end;
  if ru.Op = 'LEFT' then Exit(lf);
  if ru.Op = 'RIGHT' then Exit(rf);
  if ru.Op = 'UNIFY' then Exit(FeatsUnify(st, lf, rf));
  if ru.Op = 'REQUIRE_LEFT' then begin
    if (ru.ArgKey >= 0) and (ru.ArgVal >= 0) then Exit(FeatsRequire(st, lf, ru.ArgKey, ru.ArgVal));
    Exit(nil);
  end;
  if ru.Op = 'REQUIRE_RIGHT' then begin
    if (ru.ArgKey >= 0) and (ru.ArgVal >= 0) then Exit(FeatsRequire(st, rf, ru.ArgKey, ru.ArgVal));
    Exit(nil);
  end;
  if ru.Op = 'MAKE_GAP' then begin
    if ru.ArgType < 0 then Exit(nil);
    tmp := TFeatList.Create;
    tmp.Add(TFeat.Create(c.idx, c.qi));
    tmp.Add(TFeat.Create(c.gap, ru.ArgType));
    tmp.SortNorm;
    Exit(tmp);
  end;
  if ru.Op = 'RELCLAUSE_OBL' then begin
    if FeatsRequire(st, rf, c.obl, c.yes) = nil then Exit(nil);
    tmp := TFeatList.Create;
    tmp.Add(TFeat.Create(c.gap, c.obl));
    tmp.SortNorm;
    Exit(FeatsUnify(st, lf, tmp));
  end;
end;

{ =========================
  Chart helpers
  ========================= }

function Cidx(i, j, n: Integer): Integer; inline;
begin
  Result := i*(n+1) + j;
end;

procedure ChartInit(var chart: TChart; n: Integer);
var size, k: Integer;
begin
  size := (n+1)*(n+1);
  SetLength(chart, size);
  for k := 0 to size-1 do chart[k] := TCell.Create;
end;

function ChartGet(var chart: TChart; i, j, n: Integer): TCell; inline;
begin
  Result := chart[Cidx(i,j,n)];
end;

{ =========================
  Unary closure
  ========================= }

procedure UnaryClosure(st: TSymtab; c: TConsts; g: TGrammar; cell: TCell; arena: TArena;
                       beam: Integer; var pruned, unaryApps: Integer);
var changed: Boolean; cats: TList<Integer>; rhsCat: Integer;
    bk: TBucket; rules: TObjectList<TRule>;
    itemsSnap: TList<TItem>;
    ru: TRule; child: TItem;
    pf: TFeatList; score: Double;
    node: TNode; nid: Integer;
    it: TItem;
begin
  repeat
    changed := False;
    cats := TList<Integer>.Create;
    try
      for rhsCat in cell.Keys do cats.Add(rhsCat);
      for rhsCat in cats do begin
        if not cell.TryGetValue(rhsCat, bk) then Continue;
        if not g.Unary.TryGetValue(rhsCat, rules) then Continue;
        itemsSnap := TList<TItem>.Create;
        try
          itemsSnap.AddRange(bk.Items);
          for ru in rules do begin
            for child in itemsSnap do begin
              pf := ApplyOp(st, c, ru, child.Feats, TFeatList.Create);
              if pf = nil then Continue;
              Inc(unaryApps);
              score := child.Score + ru.Weight;

              node := TNode.Create;
              node.LabelId := ru.Lhs;
              node.IsLeaf := False;
              node.Score := score;
              node.ChildId := child.NodeId;
              node.Feats.Free;
              node.Feats := pf;
              nid := arena.AddNode(node);

              it.Cat := ru.Lhs;
              it.Feats := pf;
              it.FeatsH := FeatsHash64(pf);
              it.Score := score;
              it.NodeId := nid;

              var bk2: TBucket;
              if not cell.TryGetValue(it.Cat, bk2) then begin
                bk2 := TBucket.Create;
                cell.Add(it.Cat, bk2);
              end;
              if not bk2.HasHash(it.FeatsH) then begin
                Inc(pruned, bk2.Insert(it, beam));
                changed := True;
              end;
            end;
          end;
        finally
          itemsSnap.Free;
        end;
      end;
    finally
      cats.Free;
    end;
  until not changed;
end;

{ =========================
  Sanity checks
  ========================= }

function HasDescLabel(arena: TArena; nodeId, label: Integer): Boolean;
var n: TNode;
begin
  n := arena.GetNode(nodeId);
  if (not n.IsLeaf) and (n.LabelId = label) then Exit(True);
  if (n.ChildId >= 0) and HasDescLabel(arena, n.ChildId, label) then Exit(True);
  if (n.LeftId >= 0) and HasDescLabel(arena, n.LeftId, label) then Exit(True);
  if (n.RightId >= 0) and HasDescLabel(arena, n.RightId, label) then Exit(True);
  Result := False;
end;

function SanitySHasVpFin(arena: TArena; c: TConsts; rootId: Integer): Boolean;
var stack: TStack<Integer>; id: Integer; n: TNode;
begin
  stack := TStack<Integer>.Create;
  try
    stack.Push(rootId);
    while stack.Count > 0 do begin
      id := stack.Pop;
      n := arena.GetNode(id);
      if (not n.IsLeaf) and (n.LabelId = c.S) then begin
        if (n.ChildId >= 0) and (arena.GetNode(n.ChildId).LabelId = c.VP_FIN) then Exit(True);
        if (n.LeftId  >= 0) and (arena.GetNode(n.LeftId ).LabelId = c.VP_FIN) then Exit(True);
        if (n.RightId >= 0) and (arena.GetNode(n.RightId).LabelId = c.VP_FIN) then Exit(True);
      end;
      if n.ChildId >= 0 then stack.Push(n.ChildId);
      if n.LeftId  >= 0 then stack.Push(n.LeftId);
      if n.RightId >= 0 then stack.Push(n.RightId);
    end;
    Result := False;
  finally
    stack.Free;
  end;
end;

function SanitySinTakesVpNf(arena: TArena; c: TConsts; rootId: Integer): Boolean;
var stack: TStack<Integer>; id: Integer; n: TNode;
begin
  stack := TStack<Integer>.Create;
  try
    stack.Push(rootId);
    while stack.Count > 0 do begin
      id := stack.Pop;
      n := arena.GetNode(id);
      if (not n.IsLeaf) and (n.LabelId = c.Pinf) then
        if not HasDescLabel(arena, id, c.VP_NF) then Exit(False);
      if n.ChildId >= 0 then stack.Push(n.ChildId);
      if n.LeftId  >= 0 then stack.Push(n.LeftId);
      if n.RightId >= 0 then stack.Push(n.RightId);
    end;
    Result := True;
  finally
    stack.Free;
  end;
end;

function SanityEncliticOnlyNf(arena: TArena; c: TConsts; rootId: Integer): Boolean;
var stack: TStack<Integer>; id: Integer; n, ln, rn: TNode;
begin
  stack := TStack<Integer>.Create;
  try
    stack.Push(rootId);
    while stack.Count > 0 do begin
      id := stack.Pop;
      n := arena.GetNode(id);
      if (not n.IsLeaf) and (n.LabelId = c.VP) and (n.LeftId >= 0) and (n.RightId >= 0) then begin
        ln := arena.GetNode(n.LeftId);
        rn := arena.GetNode(n.RightId);
        if (not rn.IsLeaf) and (rn.LabelId = c.Cl) and (not ln.IsLeaf) and ((ln.LabelId = c.Vt) or (ln.LabelId = c.Vi)) then
          Exit(False);
      end;
      if n.ChildId >= 0 then stack.Push(n.ChildId);
      if n.LeftId  >= 0 then stack.Push(n.LeftId);
      if n.RightId >= 0 then stack.Push(n.RightId);
    end;
    Result := True;
  finally
    stack.Free;
  end;
end;

{ =========================
  Parsing (CKY)
  ========================= }

procedure EmitEntries(st: TSymtab; c: TConsts; entries: TObjectList<TLexEntry>; const tok: TToken;
                      arena: TArena; cell: TCell; beam: Integer; tids: TDictionary<Integer,Integer>;
                      var pruned: Integer);
var e: TLexEntry; fs1, uni: TFeatList; tid: Integer; has: Boolean; dummy: Integer;
    leaf, pre: TNode; leafId, preId: Integer;
    it: TItem; bk: TBucket;
begin
  for e in entries do begin
    fs1 := e.Feats;

    if (e.Pos = c.N) or (e.Pos = c.PropN) or (e.Pos = c.Pron) then begin
      has := FeatsFind(fs1, c.idx, dummy);
      if not has then begin
        tid := tids[tok.Index];
        var tmp := TFeatList.Create;
        tmp.Add(TFeat.Create(c.idx, tid));
        tmp.SortNorm;
        uni := FeatsUnify(st, fs1, tmp);
        tmp.Free;
        if uni <> nil then fs1 := uni;
      end;
    end;

    leaf := TNode.Create;
    leaf.LabelId := c.TOK;
    leaf.IsLeaf := True;
    leaf.LeafRaw := tok.Raw;
    leaf.Score := e.Weight;
    leafId := arena.AddNode(leaf);

    pre := TNode.Create;
    pre.LabelId := e.Pos;
    pre.IsLeaf := False;
    pre.Score := e.Weight;
    pre.ChildId := leafId;
    pre.Feats.Free;
    pre.Feats := TFeatList.Create;
    pre.Feats.AddRange(fs1);
    pre.Feats.SortNorm;
    preId := arena.AddNode(pre);

    it.Cat := e.Pos;
    it.Feats := pre.Feats;
    it.FeatsH := FeatsHash64(pre.Feats);
    it.Score := e.Weight;
    it.NodeId := preId;

    if not cell.TryGetValue(it.Cat, bk) then begin
      bk := TBucket.Create;
      cell.Add(it.Cat, bk);
    end;
    if not bk.HasHash(it.FeatsH) then
      Inc(pruned, bk.Insert(it, beam));
  end;
end;

function ParseSentence(st: TSymtab; c: TConsts; lex: TDictionary<string, TObjectList<TLexEntry>>;
                       g: TGrammar; const sent: string;
                       beam, topk: Integer; wantTrees, wantPrint: Boolean): TJSONObject;
var t0, t1: QWord;
    toks: TList<TToken>; n: Integer;
    tids: TDictionary<Integer,Integer>;
    chart: TChart;
    arena: TArena;
    oov, prunedTotal, unaryTotal: Integer;
    i, j, k, span: Integer;
    cell, lcell, rcell: TCell;
    catL, catR: Integer;
    bkL, bkR, bk: TBucket;
    rules: TObjectList<TRule>;
    ru: TRule;
    il, ir: TItem;
    pf: TFeatList;
    score: Double;
    node: TNode;
    nid: Integer;
    rightNode: Integer;
    idxv: Integer; hasIdx: Boolean;
    totItems, maxCell, ambCells: Integer;
    parsed: Boolean;
    bestScore: TJSONData;
    nRet: Integer;
    notes: TJSONArray;
    bestTree: TJSONData;
    s1, s2, s3: Boolean;
begin
  t0 := GetTickCount64;
  toks := Tokenize(sent);
  try
    n := toks.Count;
    Result := TJSONObject.Create;
    Result.Add('sentence', sent);
    Result.Add('tokens', n);

    if n = 0 then begin
      Result.Add('oovTokens', 0);
      Result.Add('parsed', False);
      Result.Add('nParsesReturned', 0);
      Result.Add('bestScore', TJSONNull.Create);
      Result.Add('timeMs', 0.0);
      Result.Add('chartItemsTotal', 0);
      Result.Add('chartItemsMaxCell', 0);
      Result.Add('prunedByBeam', 0);
      Result.Add('unaryApplications', 0);
      Result.Add('ambiguousCells', 0);
      Result.Add('sanitySHasVpFin', False);
      Result.Add('sanitySinTakesVpNf', False);
      Result.Add('sanityEncliticOnlyNf', False);
      notes := TJSONArray.Create; notes.Add('empty'); Result.Add('notes', notes);
      Result.Add('bestTree', TJSONNull.Create);
      Exit;
    end;

    tids := TDictionary<Integer,Integer>.Create;
    try
      for i := 0 to n-1 do tids.Add(i, st.Intern('t'+IntToStr(i)));

      ChartInit(chart, n);
      arena := TArena.Create;
      try
        oov := 0; prunedTotal := 0; unaryTotal := 0;

        // Lex init
        for i := 0 to n-1 do begin
          cell := ChartGet(chart, i, i+1, n);

          var entries: TObjectList<TLexEntry>;
          if not lex.TryGetValue(toks[i].Text, entries) then begin
            Inc(oov);
            entries := GuessLex(st, c, toks[i]);
            try
              EmitEntries(st, c, entries, toks[i], arena, cell, beam, tids, prunedTotal);
            finally
              entries.Free;
            end;
          end else begin
            EmitEntries(st, c, entries, toks[i], arena, cell, beam, tids, prunedTotal);
          end;

          UnaryClosure(st, c, g, cell, arena, beam, prunedTotal, unaryTotal);
        end;

        // CKY
        for span := 2 to n do begin
          for i := 0 to n - span do begin
            j := i + span;
            cell := ChartGet(chart, i, j, n);

            for k := i+1 to j-1 do begin
              lcell := ChartGet(chart, i, k, n);
              rcell := ChartGet(chart, k, j, n);
              if (lcell.Count = 0) or (rcell.Count = 0) then Continue;

              for catL in lcell.Keys do begin
                bkL := lcell[catL];
                for catR in rcell.Keys do begin
                  bkR := rcell[catR];
                  if not g.Binary.TryGetValue(PairKey(catL, catR), rules) then Continue;

                  for ru in rules do begin
                    for il in bkL.Items do begin
                      for ir in bkR.Items do begin
                        pf := ApplyOp(st, c, ru, il.Feats, ir.Feats);
                        if pf = nil then Continue;
                        score := il.Score + ir.Score + ru.Weight;

                        rightNode := ir.NodeId;
                        if ru.PropIdxToRight then begin
                          hasIdx := FeatsFind(il.Feats, c.idx, idxv);
                          if hasIdx then rightNode := arena.CloneReplace(ir.NodeId, c.qi, idxv);
                        end;

                        node := TNode.Create;
                        node.LabelId := ru.Lhs;
                        node.IsLeaf := False;
                        node.Score := score;
                        node.LeftId := il.NodeId;
                        node.RightId := rightNode;
                        node.Feats.Free;
                        node.Feats := pf;
                        nid := arena.AddNode(node);

                        var it: TItem;
                        it.Cat := ru.Lhs;
                        it.Feats := pf;
                        it.FeatsH := FeatsHash64(pf);
                        it.Score := score;
                        it.NodeId := nid;

                        if not cell.TryGetValue(it.Cat, bk) then begin
                          bk := TBucket.Create;
                          cell.Add(it.Cat, bk);
                        end;
                        if not bk.HasHash(it.FeatsH) then
                          Inc(prunedTotal, bk.Insert(it, beam));
                      end;
                    end;
                  end;
                end;
              end;
            end;

            UnaryClosure(st, c, g, cell, arena, beam, prunedTotal, unaryTotal);
          end;
        end;

        // Metrics
        totItems := 0; maxCell := 0; ambCells := 0;
        for var idx := 0 to High(chart) do begin
          var cc := chart[idx];
          var count := 0;
          if cc.Count >= 2 then Inc(ambCells);
          for bk in cc.Values do Inc(count, bk.Items.Count);
          Inc(totItems, count);
          if count > maxCell then maxCell := count;
        end;

        // Best S at (0,n)
        cell := ChartGet(chart, 0, n, n);
        parsed := cell.ContainsKey(c.S) and (cell[c.S].Items.Count > 0);
        notes := TJSONArray.Create;

        bestScore := TJSONNull.Create;
        bestTree := TJSONNull.Create;
        nRet := 0;
        s1 := False; s2 := False; s3 := False;

        if parsed then begin
          var itemsS := cell[c.S].Items;
          nRet := topk; if itemsS.Count < nRet then nRet := itemsS.Count;
          bestScore.Free;
          bestScore := TJSONFloatNumber.Create(itemsS[0].Score);

          s1 := SanitySHasVpFin(arena, c, itemsS[0].NodeId);
          s2 := SanitySinTakesVpNf(arena, c, itemsS[0].NodeId);
          s3 := SanityEncliticOnlyNf(arena, c, itemsS[0].NodeId);

          if not s1 then notes.Add('WARN: S sin VP_FIN visible');
          if not s2 then notes.Add('WARN: ''sin'' sin VP_NF bajo Pinf');
          if not s3 then notes.Add('WARN: enclítico con verbo finito');

          if wantTrees then begin
            bestTree.Free;
            bestTree := TJSONString.Create(arena.Pretty(st, itemsS[0].NodeId));
          end;
        end else begin
          notes.Add('NO_PARSE');
        end;

        t1 := GetTickCount64;

        Result.Add('oovTokens', oov);
        Result.Add('parsed', parsed);
        Result.Add('nParsesReturned', nRet);
        Result.Add('bestScore', bestScore);
        Result.Add('timeMs', Double(t1 - t0));
        Result.Add('chartItemsTotal', totItems);
        Result.Add('chartItemsMaxCell', maxCell);
        Result.Add('prunedByBeam', prunedTotal);
        Result.Add('unaryApplications', unaryTotal);
        Result.Add('ambiguousCells', ambCells);
        Result.Add('sanitySHasVpFin', s1);
        Result.Add('sanitySinTakesVpNf', s2);
        Result.Add('sanityEncliticOnlyNf', s3);
        Result.Add('notes', notes);
        Result.Add('bestTree', bestTree);

        if wantPrint then begin
          Writeln('==============================================================================');
          Writeln(sent);
          Writeln(Format('tokens=%d  oov=%d  parsed=%d  parses=%d  bestScore=%s  time_ms=%.1f',
            [n, oov, Ord(parsed), nRet,
             IfThen(bestScore.JSONType=jtNull,'null',FormatFloat('0.000000', TJSONFloatNumber(bestScore).AsFloat)),
             Double(t1-t0)]));
          Writeln(Format('chart_items=%d  max_cell=%d  pruned=%d  unary_apps=%d  amb_cells=%d',
            [totItems, maxCell, prunedTotal, unaryTotal, ambCells]));
          if notes.Count > 0 then Writeln('notes: ', notes.AsJSON);
          if wantTrees and (bestTree.JSONType<>jtNull) then Writeln(TJSONString(bestTree).AsString);
        end;

      finally
        for var idx := 0 to High(chart) do chart[idx].Free;
        arena.Free;
      end;

    finally
      tids.Free;
    end;

  finally
    toks.Free;
  end;
end;

{ =========================
  CLI / Main
  ========================= }

function HasFlag(const args: TArray<string>; const name: string): Boolean;
begin
  for var s in args do if s = name then Exit(True);
  Result := False;
end;

function GetArgVal(const args: TArray<string>; const name: string; const def: string): string;
begin
  for var i := 0 to High(args)-1 do
    if args[i] = name then Exit(args[i+1]);
  Result := def;
end;

function CollectArgsAfterDashDash: TArray<string>;
var i, n: Integer; seen: Boolean;
begin
  n := ParamCount;
  seen := False;
  var tmp := TList<string>.Create;
  try
    for i := 1 to n do begin
      if ParamStr(i) = '--' then begin
        seen := True;
        Continue;
      end;
      if seen then tmp.Add(ParamStr(i));
    end;
    if not seen then begin
      tmp.Clear;
      for i := 1 to n do tmp.Add(ParamStr(i));
    end;
    Result := tmp.ToArray;
  finally
    tmp.Free;
  end;
end;

var
  args: TArray<string>;
  lexPath, gramPath, filePath, textArg, jsonOut: string;
  beam, topk: Integer;
  wantTrees, wantPrint: Boolean;
  st: TSymtab;
  c: TConsts;
  lex: TDictionary<string, TObjectList<TLexEntry>>;
  g: TGrammar;
  corpus: string;
  sents: TList<string>;
  rows: TJSONArray;
  parsedCount, totTok, totOov: Integer;
  totTime: Double;
begin
  args := CollectArgsAfterDashDash;

  if HasFlag(args, '--help') then begin
    Writeln('Uso: ./parser_v7 -- [--lex lexicon.json] [--grammar grammar.json] [--file corpus.txt | --text "…"] [--beam 16] [--topk 1] [--trees] [--print] [--json out.json]');
    Halt(0);
  end;

  lexPath := GetArgVal(args, '--lex', 'lexicon.json');
  gramPath := GetArgVal(args, '--grammar', 'grammar.json');
  filePath := GetArgVal(args, '--file', 'corpus.txt');
  textArg := GetArgVal(args, '--text', '');
  jsonOut := GetArgVal(args, '--json', '');

  beam := StrToIntDef(GetArgVal(args, '--beam', '16'), 16);
  topk := StrToIntDef(GetArgVal(args, '--topk', '1'), 1);
  wantTrees := HasFlag(args, '--trees');
  wantPrint := HasFlag(args, '--print');

  st := TSymtab.Create;
  try
    c := EnsureConstants(st);
    lex := LoadLexicon(st, lexPath);
    try
      g := LoadGrammar(st, gramPath);
      try
        if textArg <> '' then corpus := textArg
        else corpus := TFile.ReadAllText(filePath, TEncoding.UTF8);

        sents := SplitSentences(corpus);
        try
          rows := TJSONArray.Create;
          parsedCount := 0; totTok := 0; totOov := 0; totTime := 0.0;

          for var s in sents do begin
            var row := ParseSentence(st, c, lex, g, s, beam, topk, wantTrees, wantPrint);
            rows.Add(row);
            if row.Booleans['parsed'] then Inc(parsedCount);
            Inc(totTok, row.Integers['tokens']);
            Inc(totOov, row.Integers['oovTokens']);
            totTime += row.Floats['timeMs'];
          end;

          var sn := sents.Count;
          var coverage := IfThen(sn=0, 0.0, parsedCount / sn);
          var avgTok := IfThen(sn=0, 0.0, totTok / sn);
          var avgOov := IfThen(sn=0, 0.0, totOov / sn);
          var avgTime := IfThen(sn=0, 0.0, totTime / sn);

          if wantPrint then begin
            Writeln('==============================================================================');
            Writeln('SUMMARY');
            Writeln(Format('sentences=%d  coverage=%.3f  avg_tokens=%.2f  avg_oov=%.2f  avg_time_ms=%.1f  beam=%d  top_k=%d',
              [sn, coverage, avgTok, avgOov, avgTime, beam, topk]));
          end else begin
            Writeln(Format('SUMMARY: sentences=%d coverage=%.3f avg_time_ms=%.1f beam=%d top_k=%d',
              [sn, coverage, avgTime, beam, topk]));
          end;

          if jsonOut <> '' then begin
            var summary := TJSONObject.Create;
            summary.Add('sentences', sn);
            summary.Add('coverage', coverage);
            summary.Add('avgTokens', avgTok);
            summary.Add('avgOov', avgOov);
            summary.Add('totalTimeMs', totTime);
            summary.Add('avgTimeMs', avgTime);
            summary.Add('beam', beam);
            summary.Add('topK', topk);
            summary.Add('rows', rows);
            TFile.WriteAllText(jsonOut, summary.AsJSON, TEncoding.UTF8);
            Writeln('Wrote JSON: ', jsonOut);
            summary.Free; // also frees rows (owned by object)
          end else begin
            rows.Free;
          end;

        finally
          sents.Free;
        end;

      finally
        g.Free;
      end;
    finally
      for var kv in lex do kv.Value.Free;
      lex.Free;
    end;
  finally
    st.Free;
  end;
end.
