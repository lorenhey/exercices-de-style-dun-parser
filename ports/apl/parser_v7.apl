⎕IO←1
:Namespace ParserV7

⍝ =======================
⍝ Config
⍝ =======================
MAXN←64
BEAM←8
NEG←¯1E30
MAXJ←MAXN+1

⍝ =======================
⍝ Deck (toy) — reemplazá por tu deck real
⍝ Símbolos: 1 S, 2 NP, 3 VP, 4 DET, 5 N, 6 V, 7 NOUN
⍝ =======================
Id2Sym←'S' 'NP' 'VP' 'DET' 'N' 'V' 'NOUN'
START←1
FALLBACK←7

⍝ Reglas unarias: [lhs rhs logw]
Unary←⍪ 3 6 0            ⍝ VP -> V

⍝ Reglas binarias: [lhs rhs1 rhs2 logw]
Binary←⍪ 1 2 3 0         ⍝ S  -> NP VP
Binary←Binary⍪ 2 4 5 0   ⍝ NP -> DET N

⍝ Lexicón (lowercase): palabras y POS-id
LexW←'el' 'filósofo' 'filosofo' 'murió' 'murio'
LexP←4    5          5         6       6

⍝ =======================
⍝ Estado incremental
⍝ =======================
N←0
TokSurf←⍬
TokCat←⍬
TokSc←⍬
Chart←(MAXN MAXJ)⍴⊂⍬   ⍝ Chart[i;j] = lista de items

⍝ =======================
⍝ Helpers
⍝ =======================

Reset←{
  N←0 ⋄ TokSurf←⍬ ⋄ TokCat←⍬ ⋄ TokSc←⍬
  Chart←(MAXN MAXJ)⍴⊂⍬
  0
}

⍝ lookup lexical (word->catid)
LexLookup←{
  w←⎕C ⍵
  i←LexW⍳w
  (i>⍴LexW):FALLBACK
  LexP[i]
}

⍝ item fields: (cat score kind a b)
CatOf←{⍵[1]}
ScOf ←{⍵[2]}

⍝ Insert con dedupe por cat y beam
Insert←{
  ⍝ args: i j newItem
  i j item←⍵
  cell←Chart[i;j]
  newCat←CatOf item
  cats←CatOf¨cell
  k←cats⍳newCat
  :If k≤⍴cell
      :If (ScOf item)>(ScOf cell[k])
          cell[k]←item
          Chart[i;j]←cell
          1
      :Else
          0
      :EndIf
  :Else
      :If (⍴cell)<BEAM
          Chart[i;j]←cell,item
          1
      :Else
          scs←ScOf¨cell
          w←scs⍳⌊/scs   ⍝ peor
          :If (ScOf item)>(ScOf cell[w])
              cell[w]←item
              Chart[i;j]←cell
              1
          :Else
              0
          :EndIf
      :EndIf
  :EndIf
}

⍝ Unary closure (iter limitada)
UnaryClose←{
  ⍝ args: i j
  i j←⍵
  :For iter :In ⍳16
      changed←0
      cell←Chart[i;j]
      :For s :In ⍳⍴cell
          child←cell[s]
          childCat←child[1]
          childSc←child[2]
          childPtr←i j s
          ⍝ aplicar todas las unarias que matchean RHS
          rhs←Unary[;2]
          idx←⍸rhs=childCat
          :If 0<⍴idx
              :For t :In idx
                  lhs←Unary[t;1]
                  logw←Unary[t;3]
                  new←lhs (childSc+logw) 1 childPtr 0
                  changed+←Insert i j new
              :EndFor
          :EndIf
      :EndFor
      :If changed=0 :Leave :EndIf
  :EndFor
  0
}

⍝ Combine binario: prueba reglas sobre items de (i,k) y (k,j)
Combine←{
  ⍝ args: i k j
  i k j←⍵
  L←Chart[i;k]
  R←Chart[k;j]
  :If (0=⍴L)∨(0=⍴R) :Return 0 :EndIf

  :For sl :In ⍳⍴L
      li←L[sl]
      catL←li[1] ⋄ scL←li[2]
      ptrL←i k sl
      :For sr :In ⍳⍴R
          ri←R[sr]
          catR←ri[1] ⋄ scR←ri[2]
          ptrR←k j sr
          ⍝ filtrar reglas que matcheen RHS1/RHS2
          m1←Binary[;2]=catL
          m2←Binary[;3]=catR
          idx←⍸m1∧m2
          :If 0<⍴idx
              :For t :In idx
                  lhs←Binary[t;1]
                  logw←Binary[t;4]
                  new←lhs (scL+scR+logw) 2 ptrL ptrR
                  _←Insert i j new
              :EndFor
          :EndIf
      :EndFor
  :EndFor
  0
}

BestRootPtr←{
  :If N=0 :Return 0 :EndIf
  j←N+1
  cell←Chart[1;j]
  :If 0=⍴cell :Return 0 :EndIf
  idx←⍸(CatOf¨cell)=START
  :If 0=⍴idx :Return 0 :EndIf
  scs←(ScOf¨cell)[idx]
  k←idx[scs⍳⌈/scs]
  1 j k
}

Render←{
  ⍝ arg: ptr (i j s) o 0
  ptr←⍵
  :If ptr≡0 :Return '' :EndIf
  i j s←ptr
  item←Chart[i;j][s]
  cat score kind a b←item
  lab←Id2Sym[cat]
  :Select kind
  :Case 0
      '(' , lab , ' ' , TokSurf[a] , ')'
  :Case 1
      '(' , lab , ' ' , (Render a) , ')'
  :Case 2
      '(' , lab , ' ' , (Render a) , ' ' , (Render b) , ')'
  :EndSelect
}

⍝ =======================
⍝ Incremental real
⍝ =======================
Step←{
  ⍝ arg: token surface (string)
  tok←⍵
  :If N=MAXN :Return 0 :EndIf

  pos←LexLookup tok
  N+←1
  t←N
  TokSurf,←⊂tok
  TokCat,←pos
  TokSc,←0

  j←t+1

  ⍝ lexical cell (t, t+1)
  Chart[t;j]←⍬
  _←Insert t j (pos 0 0 t 0)
  _←UnaryClose t j

  ⍝ spans (i,j) con i=t-1..1
  :For i :In ⌽⍳(t-1)
      Chart[i;j]←⍬
      :For k :In (i+1)+⍳((j-1)-(i+1)+1)
          _←Combine i k j
      :EndFor
      _←UnaryClose i j
  :EndFor
  0
}

⍝ =======================
⍝ Tokenización mínima (si querés Step manual, no la uses)
⍝ =======================
Tokenize←{
  s←⎕C ⍵
  ⍝ reemplaza puntuación común por espacio
  bad←'. , ; : ? ! ( ) [ ] " '''
  :For c :In bad
      s←(c=' '⍴⍨⍴s)@(s=c)⊢s
  :EndFor
  t←(s≠' ')⊂s
  t
}

SplitMorph←{
  toks←⍵
  out←⍬
  :For w :In toks
      :If w≡'al'
          out,←⊂'a' ⋄ out,←⊂'el'
      :ElseIf w≡'del'
          out,←⊂'de' ⋄ out,←⊂'el'
      :Else
          out,←⊂w
      :EndIf
  :EndFor
  out
}

Parse←{
  Reset⍬
  toks←SplitMorph Tokenize ⍵
  :For w :In toks
      Step w
  :EndFor
  Render BestRootPtr⍬
}

:EndNamespace
