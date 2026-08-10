with Resources; use Resources;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Characters.Handling; use Ada.Characters.Handling;

package body Parser_V7 is

   Neg_Inf : constant Long_Float := -1.0E30;

   -- Chart: (i,j,s) linealizado
   type Cell_Array_Int is array (Positive range <>) of Integer;
   type Cell_Array_Flt is array (Positive range <>) of Long_Float;

   Size : constant Positive := MaxN * MaxJ * Beam;

   Cat  : Cell_Array_Int(1 .. Size);
   Sc   : Cell_Array_Flt(1 .. Size);
   Kind : Cell_Array_Int(1 .. Size);  -- 0 leaf, 1 unary, 2 binary
   APtr : Cell_Array_Int(1 .. Size);
   BPtr : Cell_Array_Int(1 .. Size);

   N : Natural := 0;

   type Tok_Array is array (Positive range <>) of Unbounded_String;
   type TokCat_Array is array (Positive range <>) of Sym_Id;
   type TokSc_Array  is array (Positive range <>) of Long_Float;

   TokSurf : Tok_Array(1 .. MaxN);
   TokCat  : TokCat_Array(1 .. MaxN);
   TokSc   : TokSc_Array(1 .. MaxN);

   function Idx (I, J, S : Positive) return Positive is
   begin
      -- idx = 1 + (((i-1)*MaxJ + (j-1))*Beam + (s-1))
      return 1 + (((I - 1) * MaxJ + (J - 1)) * Beam + (S - 1));
   end Idx;

   function Pack (I, J, S : Positive) return Integer is
   begin
      return Integer(((I * 1000) + J) * 10 + S);
   end Pack;

   function PI (Ptr : Integer) return Positive is
   begin
      return Positive(Ptr / 10000);
   end PI;

   function PJ (Ptr : Integer) return Positive is
      I : constant Integer := Ptr / 10000;
   begin
      return Positive((Ptr / 10) - (I * 1000));
   end PJ;

   function PS (Ptr : Integer) return Positive is
   begin
      return Positive(Ptr mod 10);
   end PS;

   procedure Clear_Cell (I, J : Positive) is
      K : Positive;
   begin
      for S in 1 .. Beam loop
         K := Idx(I, J, S);
         Cat(K)  := 0;
         Sc(K)   := Neg_Inf;
         Kind(K) := 0;
         APtr(K) := 0;
         BPtr(K) := 0;
      end loop;
   end Clear_Cell;

   function Lex_Lookup (W : String) return Sym_Id is
      Lw : constant String := To_Lower(W);
   begin
      for I in Lexicon'Range loop
         if To_String(Lexicon(I).Word) = Lw then
            return Lexicon(I).Pos;
         end if;
      end loop;
      -- fallback: NOUN (asegurate de tenerlo en symbols)
      return 7;
   end Lex_Lookup;

   function Insert
     (I, J : Positive;
      NewCat : Sym_Id;
      NewSc  : Long_Float;
      NewKind : Integer;
      A, B : Integer) return Boolean
   is
      K : Positive;
   begin
      -- 1) dedupe por cat
      for S in 1 .. Beam loop
         K := Idx(I, J, S);
         if Sym_Id(Cat(K)) = NewCat then
            if NewSc > Sc(K) then
               Sc(K)   := NewSc;
               Kind(K) := NewKind;
               APtr(K) := A;
               BPtr(K) := B;
               return True;
            else
               return False;
            end if;
         end if;
      end loop;

      -- 2) empty slot
      for S in 1 .. Beam loop
         K := Idx(I, J, S);
         if Cat(K) = 0 then
            Cat(K)  := Integer(NewCat);
            Sc(K)   := NewSc;
            Kind(K) := NewKind;
            APtr(K) := A;
            BPtr(K) := B;
            return True;
         end if;
      end loop;

      -- 3) replace worst if better
      declare
         WorstS  : Positive := 1;
         WorstSc : Long_Float := Sc(Idx(I, J, 1));
      begin
         for S in 2 .. Beam loop
            K := Idx(I, J, S);
            if Sc(K) < WorstSc then
               WorstSc := Sc(K);
               WorstS  := S;
            end if;
         end loop;

         if NewSc > WorstSc then
            K := Idx(I, J, WorstS);
            Cat(K)  := Integer(NewCat);
            Sc(K)   := NewSc;
            Kind(K) := NewKind;
            APtr(K) := A;
            BPtr(K) := B;
            return True;
         end if;
      end;

      return False;
   end Insert;

   procedure Unary_Close (I, J : Positive) is
      Changed : Boolean;
   begin
      for Iter in 1 .. 16 loop
         Changed := False;

         for S in 1 .. Beam loop
            declare
               K : constant Positive := Idx(I, J, S);
               ChildCat : constant Sym_Id := Sym_Id(Cat(K));
            begin
               if ChildCat /= 0 then
                  declare
                     ChildSc  : constant Long_Float := Sc(K);
                     ChildPtr : constant Integer := Pack(I, J, S);
                  begin
                     for R in Rules'Range loop
                        if Rules(R).Len = 1 and then Rules(R).RHS1 = ChildCat then
                           if Insert(I, J,
                                     Rules(R).LHS,
                                     ChildSc + Rules(R).LogW,
                                     1,
                                     ChildPtr, 0) then
                              Changed := True;
                           end if;
                        end if;
                     end loop;
                  end;
               end if;
            end;
         end loop;

         exit when not Changed;
      end loop;
   end Unary_Close;

   procedure Combine (I, Ksplit, J : Positive) is
   begin
      for SL in 1 .. Beam loop
         declare
            KL : constant Positive := Idx(I, Ksplit, SL);
            CatL : constant Sym_Id := Sym_Id(Cat(KL));
         begin
            if CatL /= 0 then
               declare
                  ScL  : constant Long_Float := Sc(KL);
                  PtrL : constant Integer := Pack(I, Ksplit, SL);
               begin
                  for SR in 1 .. Beam loop
                     declare
                        KR : constant Positive := Idx(Ksplit, J, SR);
                        CatR : constant Sym_Id := Sym_Id(Cat(KR));
                     begin
                        if CatR /= 0 then
                           declare
                              ScR  : constant Long_Float := Sc(KR);
                              PtrR : constant Integer := Pack(Ksplit, J, SR);
                           begin
                              for R in Rules'Range loop
                                 if Rules(R).Len = 2
                                   and then Rules(R).RHS1 = CatL
                                   and then Rules(R).RHS2 = CatR
                                 then
                                    declare
                                       NewSc : constant Long_Float := ScL + ScR + Rules(R).LogW;
                                    begin
                                       -- kind=2 binary
                                       pragma Unreferenced (Insert);
                                       declare
                                          Dummy : constant Boolean :=
                                            Insert(I, J, Rules(R).LHS, NewSc, 2, PtrL, PtrR);
                                       begin
                                          null;
                                       end;
                                    end;
                                 end if;
                              end loop;
                           end;
                        end if;
                     end;
                  end loop;
               end;
            end if;
         end;
      end loop;
   end Combine;

   procedure Reset is
   begin
      N := 0;
      for K in 1 .. Size loop
         Cat(K)  := 0;
         Sc(K)   := Neg_Inf;
         Kind(K) := 0;
         APtr(K) := 0;
         BPtr(K) := 0;
      end loop;
   end Reset;

   procedure Step (Token_Surface : String; Token_Score : Long_Float := 0.0) is
      T : Positive;
      J : Positive;
      Pos : Sym_Id;
   begin
      if N = MaxN then
         return; -- truncar silenciosamente (estilo “deck”)
      end if;

      Pos := Lex_Lookup(Token_Surface);

      N := N + 1;
      T := N;
      TokSurf(T) := To_Unbounded_String(Token_Surface);
      TokCat(T)  := Pos;
      TokSc(T)   := Token_Score;

      J := T + 1;

      -- lexical cell (t, t+1)
      Clear_Cell(T, J);
      declare
         Dummy : constant Boolean := Insert(T, J, Pos, Token_Score, 0, Integer(T), 0);
      begin
         null;
      end;
      Unary_Close(T, J);

      -- spans ending at j
      if T >= 2 then
         for I in reverse 1 .. (T - 1) loop
            Clear_Cell(I, J);
            for Ksplit in (I + 1) .. (J - 1) loop
               Combine(I, Ksplit, J);
            end loop;
            Unary_Close(I, J);
         end loop;
      end if;
   end Step;

   function Best_Root_Ptr return Integer is
      J : Positive;
      BestSc : Long_Float := Neg_Inf;
      BestS  : Natural := 0;
   begin
      if N = 0 then
         return 0;
      end if;

      J := N + 1;
      for S in 1 .. Beam loop
         declare
            K : constant Positive := Idx(1, J, S);
         begin
            if Sym_Id(Cat(K)) = Start_Sym and then Sc(K) > BestSc then
               BestSc := Sc(K);
               BestS  := S;
            end if;
         end;
      end loop;

      if BestS = 0 then
         return 0;
      end if;

      return Pack(1, J, Positive(BestS));
   end Best_Root_Ptr;

   function Render_Node (Ptr : Integer) return Unbounded_String is
      I : Positive;
      J : Positive;
      S : Positive;
      K : Positive;
      C : Sym_Id;
      Knd : Integer;
      A  : Integer;
      B  : Integer;
      Lab : Unbounded_String;
   begin
      if Ptr = 0 then
         return To_Unbounded_String("");
      end if;

      I := PI(Ptr); J := PJ(Ptr); S := PS(Ptr);
      K := Idx(I, J, S);

      C   := Sym_Id(Cat(K));
      Knd := Kind(K);
      A   := APtr(K);
      B   := BPtr(K);

      if C >= 1 and then C <= NSyms then
         Lab := Id2Sym(Positive(C));
      else
         Lab := To_Unbounded_String("<?>");
      end if;

      if Knd = 0 then
         -- leaf: A = token index
         return To_Unbounded_String("(") & Lab & To_Unbounded_String(" ")
           & TokSurf(Positive(A)) & To_Unbounded_String(")");
      elsif Knd = 1 then
         return To_Unbounded_String("(") & Lab & To_Unbounded_String(" ")
           & Render_Node(A) & To_Unbounded_String(")");
      else
         return To_Unbounded_String("(") & Lab & To_Unbounded_String(" ")
           & Render_Node(A) & To_Unbounded_String(" ")
           & Render_Node(B) & To_Unbounded_String(")");
      end if;
   end Render_Node;

   function Render_Best return Unbounded_String is
      Root : constant Integer := Best_Root_Ptr;
   begin
      if Root = 0 then
         return To_Unbounded_String("");
      end if;
      return Render_Node(Root);
   end Render_Best;

   function Token_Count return Natural is
   begin
      return N;
   end Token_Count;

end Parser_V7;
