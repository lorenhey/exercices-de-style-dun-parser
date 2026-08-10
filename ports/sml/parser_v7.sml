(* ============================
 * Parser V7 — Standard ML (SML/NJ)
 * CKY incremental + beam + unary closure + backpointers
 * Deck: reglas/lexicon como listas SML.
 * ============================ *)

structure ParserV7 =
struct
  (* ---------- Config ---------- *)
  val MAXN  = 64
  val BEAM  = 8
  val NEG   = ~1.0E30
  val MAXJ  = MAXN + 1

  type sym = int          (* 0 = empty *)
  type score = real

  (* item fields: cat, sc, kind, a, b *)
  (* kind: 0 leaf, 1 unary, 2 binary *)
  type item = {cat:sym, sc:score, kind:int, a:int, b:int}

  (* ---------- Deck (toy) ---------- *)
  (* symbols: 1 S, 2 NP, 3 VP, 4 DET, 5 N, 6 V, 7 NOUN (fallback) *)
  val id2sym : string vector =
    Vector.fromList ["<?>","S","NP","VP","DET","N","V","NOUN"]  (* index 1.. *)

  val START : sym = 1  (* "S" id if you want: set to 1, and keep id2sym aligned *)
  val FALLBACK : sym = 7

  (* Rules *)
  type rule1 = {lhs:sym, rhs:sym, logw:score}               (* unary *)
  type rule2 = {lhs:sym, rhs1:sym, rhs2:sym, logw:score}    (* binary *)

  val unaryRules : rule1 list =
    [{lhs=3, rhs=6, logw=0.0}]   (* VP -> V *)

  val binaryRules : rule2 list =
    [{lhs=1, rhs1=2, rhs2=3, logw=0.0},   (* S  -> NP VP *)
     {lhs=2, rhs1=4, rhs2=5, logw=0.0}]   (* NP -> DET N *)

  (* Lexicon *)
  val lexicon : (string * sym) list =
    [("el",4), ("filósofo",5), ("filosofo",5), ("murió",6), ("murio",6)]

  (* ---------- State ---------- *)
  val n : int ref = ref 0
  val tokSurf : string array = Array.array(MAXN+1, "")
  val tokCat  : sym array    = Array.array(MAXN+1, 0)
  val tokSc   : score array  = Array.array(MAXN+1, 0.0)

  val size = MAXN * MAXJ * BEAM

  (* chart arrays *)
  val catA  : sym array   = Array.array(size+1, 0)
  val scA   : score array = Array.array(size+1, NEG)
  val kindA : int array   = Array.array(size+1, 0)
  val aA    : int array   = Array.array(size+1, 0)
  val bA    : int array   = Array.array(size+1, 0)

  fun idx (i:int, j:int, s:int) : int =
    1 + (((i-1)*MAXJ + (j-1))*BEAM + (s-1))

  fun pack (i:int, j:int, s:int) : int = ((i*1000)+j)*10 + s
  fun pi (ptr:int) : int = ptr div 10000
  fun pj (ptr:int) : int =
    let val i = ptr div 10000
    in (ptr div 10) - (i*1000) end
  fun ps (ptr:int) : int = ptr mod 10

  fun clearCell (i:int, j:int) =
    let fun loop s =
          if s>BEAM then ()
          else
            let val k = idx(i,j,s)
            in
              Array.update(catA, k, 0);
              Array.update(scA, k, NEG);
              Array.update(kindA, k, 0);
              Array.update(aA, k, 0);
              Array.update(bA, k, 0);
              loop (s+1)
            end
    in loop 1 end

  fun reset () =
    (n := 0;
     (* full clear *)
     let fun loop k =
           if k>size then ()
           else (Array.update(catA,k,0);
                 Array.update(scA,k,NEG);
                 Array.update(kindA,k,0);
                 Array.update(aA,k,0);
                 Array.update(bA,k,0);
                 loop (k+1))
     in loop 1 end)

  fun lower (s:string) =
    String.map Char.toLower s

  fun lexLookup (w:string) : sym =
    let val ww = lower w
        fun find [] = FALLBACK
          | find ((x,p)::xs) = if x=ww then p else find xs
    in find lexicon end

  (* Insert with dedupe by cat + beam *)
  fun insert (i:int, j:int, it:item) : bool =
    let
      val newCat = #cat it
      val newSc  = #sc it

      fun findSame s =
        if s>BEAM then 0
        else
          let val k = idx(i,j,s)
          in if Array.sub(catA,k)=newCat then s else findSame (s+1) end

      fun firstEmpty s =
        if s>BEAM then 0
        else
          let val k = idx(i,j,s)
          in if Array.sub(catA,k)=0 then s else firstEmpty (s+1) end

      fun writeSlot s =
        let val k = idx(i,j,s)
        in
          Array.update(catA,k,newCat);
          Array.update(scA,k,newSc);
          Array.update(kindA,k,#kind it);
          Array.update(aA,k,#a it);
          Array.update(bA,k,#b it)
        end

      val same = findSame 1
    in
      if same<>0 then
        let val k = idx(i,j,same)
        in if newSc > Array.sub(scA,k)
           then (writeSlot same; true)
           else false
        end
      else
        let val empty = firstEmpty 1
        in
          if empty<>0 then (writeSlot empty; true)
          else
            (* replace worst *)
            let
              fun worst s (bestS,bestSc) =
                if s>BEAM then bestS
                else
                  let val k = idx(i,j,s)
                      val sc = Array.sub(scA,k)
                  in if sc < bestSc then worst (s+1) (s,sc)
                     else worst (s+1) (bestS,bestSc)
                  end
              val s0 = 1
              val k0 = idx(i,j,1)
              val wS = worst 2 (1, Array.sub(scA,k0))
              val kw = idx(i,j,wS)
            in
              if newSc > Array.sub(scA,kw)
              then (writeSlot wS; true)
              else false
            end
        end
    end

  fun unaryClose (i:int, j:int) =
    let
      fun iterLoop 0 = ()
        | iterLoop t =
            let
              val changed = ref false

              fun scanSlot s =
                if s>BEAM then ()
                else
                  let val k = idx(i,j,s)
                      val childCat = Array.sub(catA,k)
                  in
                    if childCat<>0 then
                      let val childSc  = Array.sub(scA,k)
                          val childPtr = pack(i,j,s)
                          fun apply [] = ()
                            | apply (r::rs) =
                                if #rhs r = childCat then
                                  let val newIt = {cat=#lhs r,
                                                   sc=childSc + #logw r,
                                                   kind=1,
                                                   a=childPtr,
                                                   b=0}
                                  in
                                    if insert(i,j,newIt) then changed := true else ();
                                    apply rs
                                  end
                                else apply rs
                      in
                        apply unaryRules;
                        scanSlot (s+1)
                      end
                    else scanSlot (s+1)
                  end
            in
              scanSlot 1;
              if !changed then iterLoop (t-1) else ()
            end
    in iterLoop 16 end

  fun combine (i:int, ksplit:int, j:int) =
    let
      fun scanL sl =
        if sl>BEAM then ()
        else
          let val kl = idx(i,ksplit,sl)
              val catL = Array.sub(catA,kl)
          in
            if catL<>0 then
              let val scL = Array.sub(scA,kl)
                  val ptrL = pack(i,ksplit,sl)

                  fun scanR sr =
                    if sr>BEAM then ()
                    else
                      let val kr = idx(ksplit,j,sr)
                          val catR = Array.sub(catA,kr)
                      in
                        if catR<>0 then
                          let val scR = Array.sub(scA,kr)
                              val ptrR = pack(ksplit,j,sr)

                              fun apply [] = ()
                                | apply (r::rs) =
                                    if #rhs1 r = catL andalso #rhs2 r = catR then
                                      let val newIt = {cat=#lhs r,
                                                       sc=scL + scR + #logw r,
                                                       kind=2,
                                                       a=ptrL,
                                                       b=ptrR}
                                      in
                                        ignore (insert(i,j,newIt));
                                        apply rs
                                      end
                                    else apply rs
                          in
                            apply binaryRules;
                            scanR (sr+1)
                          end
                        else scanR (sr+1)
                      end
              in
                scanR 1;
                scanL (sl+1)
              end
            else scanL (sl+1)
          end
    in scanL 1 end

  (* ---------- Incremental Step ---------- *)
  fun step (tok:string) =
    if !n >= MAXN then ()
    else
      let
        val pos = lexLookup tok
        val t = (!n) + 1
        val j = t + 1
      in
        n := t;
        Array.update(tokSurf, t, tok);
        Array.update(tokCat,  t, pos);
        Array.update(tokSc,   t, 0.0);

        clearCell(t,j);
        ignore (insert(t,j,{cat=pos, sc=0.0, kind=0, a=t, b=0}));
        unaryClose(t,j);

        (* spans ending at j *)
        if t>=2 then
          let fun loopI i =
                if i<1 then ()
                else
                  (clearCell(i,j);
                   (* ksplit = i+1 .. j-1 *)
                   let fun loopK k =
                         if k>(j-1) then ()
                         else (combine(i,k,j); loopK (k+1))
                   in loopK (i+1) end;
                   unaryClose(i,j);
                   loopI (i-1))
          in loopI (t-1) end
        else ()
      end

  (* ---------- Best root & render ---------- *)
  fun bestRootPtr () : int =
    if !n=0 then 0
    else
      let val j = (!n) + 1
          fun loop s (bestS,bestSc) =
            if s>BEAM then bestS
            else
              let val k = idx(1,j,s)
                  val c = Array.sub(catA,k)
                  val sc = Array.sub(scA,k)
              in
                if c=START andalso sc>bestSc then loop (s+1) (s,sc)
                else loop (s+1) (bestS,bestSc)
              end
          val bestS = loop 1 (0,NEG)
      in if bestS=0 then 0 else pack(1,j,bestS) end

  fun symName (id:int) =
    if id>=0 andalso id < Vector.length id2sym
    then Vector.sub(id2sym,id)
    else "<?>"

  fun render (ptr:int) : string =
    if ptr=0 then ""
    else
      let
        val i = pi ptr
        val j = pj ptr
        val s = ps ptr
        val k = idx(i,j,s)
        val cat = Array.sub(catA,k)
        val kind = Array.sub(kindA,k)
        val a = Array.sub(aA,k)
        val b = Array.sub(bA,k)
        val lab = symName cat
      in
        if kind=0 then
          "(" ^ lab ^ " " ^ Array.sub(tokSurf,a) ^ ")"
        else if kind=1 then
          "(" ^ lab ^ " " ^ render a ^ ")"
        else
          "(" ^ lab ^ " " ^ render a ^ " " ^ render b ^ ")"
      end

  fun renderBest () = render (bestRootPtr ())

  (* ---------- Convenience parse (minimal tokenize) ---------- *)
  fun tokenize (s:string) : string list =
    let
      fun isWord c =
        Char.isAlphaNum c orelse c = #"_" orelse c = #"á" orelse c = #"é" orelse
        c = #"í" orelse c = #"ó" orelse c = #"ú" orelse c = #"ñ" orelse
        c = #"Á" orelse c = #"É" orelse c = #"Í" orelse c = #"Ó" orelse
        c = #"Ú" orelse c = #"Ñ"

      fun loop [] cur acc =
            if cur="" then List.rev acc else List.rev (cur::acc)
        | loop (c::cs) cur acc =
            if isWord c then loop cs (cur ^ str c) acc
            else if cur="" then loop cs "" acc
            else loop cs "" (cur::acc)
    in
      loop (String.explode s) "" []
    end

  fun splitMorph toks =
    let
      fun addTok w acc =
        let val lw = lower w
        in
          if lw="al" then "a"::"el"::acc
          else if lw="del" then "de"::"el"::acc
          else lw::acc
        end
      fun loop [] acc = List.rev acc
        | loop (x::xs) acc = loop xs (addTok x acc)
    in loop toks [] end

  fun parse (sentence:string) : string =
    (reset ();
     List.app step (splitMorph (tokenize sentence));
     renderBest ())

end
