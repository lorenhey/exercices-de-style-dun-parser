library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package Resources_Pkg is
  -- Ajustes básicos
  constant WORD_MAX : natural := 16;

  subtype sym_id_t is natural range 0 to 255;  -- 0 = empty

  -- Scoring: entero (logw escalado si querés)
  subtype score_t is integer;

  -- ===== Símbolos (toy) =====
  -- 1 S, 2 NP, 3 VP, 4 DET, 5 N, 6 V, 7 NOUN (fallback)
  constant START_SYM    : sym_id_t := 1;
  constant FALLBACK_SYM : sym_id_t := 7;

  -- ===== Reglas =====
  type unary_rule_t is record
    lhs  : sym_id_t;
    rhs  : sym_id_t;
    logw : score_t;
  end record;

  type binary_rule_t is record
    lhs  : sym_id_t;
    rhs1 : sym_id_t;
    rhs2 : sym_id_t;
    logw : score_t;
  end record;

  constant N_UNARY  : natural := 1;
  constant N_BINARY : natural := 2;

  type unary_rules_t  is array (0 to N_UNARY-1)  of unary_rule_t;
  type binary_rules_t is array (0 to N_BINARY-1) of binary_rule_t;

  constant UNARY_RULES : unary_rules_t :=
    (0 => (lhs => 3, rhs => 6, logw => 0)); -- VP -> V

  constant BINARY_RULES : binary_rules_t :=
    (0 => (lhs => 1, rhs1 => 2, rhs2 => 3, logw => 0), -- S  -> NP VP
     1 => (lhs => 2, rhs1 => 4, rhs2 => 5, logw => 0)  -- NP -> DET N
    );

  -- ===== Lexicón (toy) =====
  type lex_entry_t is record
    w   : string(1 to WORD_MAX);
    len : natural range 0 to WORD_MAX;
    pos : sym_id_t;
  end record;

  constant N_LEX : natural := 5;
  type lexicon_t is array (0 to N_LEX-1) of lex_entry_t;

  constant LEXICON : lexicon_t :=
    (0 => (w => "el"       & (3 to WORD_MAX => ' '), len => 2, pos => 4),
     1 => (w => "filosofo" & (9 to WORD_MAX => ' '), len => 8, pos => 5),
     2 => (w => "murio"    & (6 to WORD_MAX => ' '), len => 5, pos => 6),
     3 => (w => "filósofo" & (9 to WORD_MAX => ' '), len => 8, pos => 5),
     4 => (w => "murió"    & (6 to WORD_MAX => ' '), len => 5, pos => 6)
    );

end package Resources_Pkg;
