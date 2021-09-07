with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Resources is
   -- Ajustá estas constantes al “deck” real que generes.
   MaxN  : constant Positive := 64;
   Beam  : constant Positive := 8;
   MaxJ  : constant Positive := MaxN + 1;

   subtype Sym_Id is Natural;        -- 0 = vacío
   subtype Rule_Len is Positive range 1 .. 2;

   type Rule is record
      LHS  : Sym_Id;
      RHS1 : Sym_Id;
      RHS2 : Sym_Id;                -- 0 si Len=1
      Len  : Rule_Len;
      LogW : Long_Float;
   end record;

   -- ======= Deck de símbolos =======
   -- id2sym(1..NSyms): nombre para render (opcional pero útil)
   NSyms : constant Positive := 7;

   type Sym_Table is array (Positive range <>) of Unbounded_String;
   Id2Sym : constant Sym_Table(1 .. NSyms);

   -- start symbol
   Start_Sym : constant Sym_Id := 1;  -- "S"

   -- ======= Deck de reglas =======
   NRules : constant Positive := 3;
   type Rule_Table is array (Positive range <>) of Rule;
   Rules : constant Rule_Table(1 .. NRules);

   -- ======= Deck de lexicón =======
   type Lex_Entry is record
      Word : Unbounded_String;
      Pos  : Sym_Id;
   end record;

   NLex : constant Positive := 5;
   type Lex_Table is array (Positive range <>) of Lex_Entry;
   Lexicon : constant Lex_Table(1 .. NLex);

end Resources;
