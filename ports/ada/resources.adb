with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Resources is

   -- Símbolos:
   -- 1 S, 2 NP, 3 VP, 4 DET, 5 N, 6 V, 7 NOUN (fallback)
   Id2Sym : constant Sym_Table(1 .. NSyms) :=
     (1 => To_Unbounded_String("S"),
      2 => To_Unbounded_String("NP"),
      3 => To_Unbounded_String("VP"),
      4 => To_Unbounded_String("DET"),
      5 => To_Unbounded_String("N"),
      6 => To_Unbounded_String("V"),
      7 => To_Unbounded_String("NOUN"));

   Rules : constant Rule_Table(1 .. NRules) :=
     (1 => (LHS => 1, RHS1 => 2, RHS2 => 3, Len => 2, LogW => 0.0), -- S  -> NP VP
      2 => (LHS => 2, RHS1 => 4, RHS2 => 5, Len => 2, LogW => 0.0), -- NP -> DET N
      3 => (LHS => 3, RHS1 => 6, RHS2 => 0, Len => 1, LogW => 0.0)  -- VP -> V
     );

   Lexicon : constant Lex_Table(1 .. NLex) :=
     (1 => (Word => To_Unbounded_String("el"),       Pos => 4),
      2 => (Word => To_Unbounded_String("filósofo"), Pos => 5),
      3 => (Word => To_Unbounded_String("filosofo"), Pos => 5),
      4 => (Word => To_Unbounded_String("murió"),    Pos => 6),
      5 => (Word => To_Unbounded_String("murio"),    Pos => 6)
     );

end Resources;
