library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.Resources_Pkg.all;

entity Parser_V7 is
  generic (
    MAXN : natural := 64;
    BEAM : natural := 8
  );
  port (
    clk  : in  std_logic;
    rst  : in  std_logic;

    -- Incremental input: un token por pulso
    token_valid : in  std_logic;
    token_str   : in  string(1 to WORD_MAX);
    token_len   : in  natural range 0 to WORD_MAX;

    -- Señales de estado
    done         : out std_logic;  -- 1 por 1 ciclo cuando termina un Step
    accept       : out std_logic;  -- 1 si hay parse S en span (1, N+1)
    best_root_ptr: out integer;    -- ptr packed ((i*1000)+j)*10+s

    token_count  : out natural range 0 to MAXN;

    -- Debug: leer cualquier celda (i,j,s) del chart desde afuera
    dbg_i    : in  natural range 1 to MAXN := 1;
    dbg_j    : in  natural range 1 to MAXN+1 := 2;
    dbg_s    : in  natural range 1 to BEAM := 1;
    dbg_cat  : out sym_id_t;
    dbg_sc   : out score_t;
    dbg_kind : out natural range 0 to 2;
    dbg_a    : out integer;
    dbg_b    : out integer
  );
end entity;

architecture Behavioral of Parser_V7 is
  constant MAXJ : natural := MAXN + 1;
  constant NEG  : score_t := -1073741824; -- ~ -2^30

  -- Un ítem del beam
  type item_t is record
    cat  : sym_id_t;
    sc   : score_t;
    kind : natural range 0 to 2;  -- 0 leaf, 1 unary, 2 binary
    a    : integer;
    b    : integer;
  end record;

  -- Chart: (i,j,s)
  type chart_t is array (1 to MAXN, 1 to MAXJ, 1 to BEAM) of item_t;

  -- Tokens almacenados
  type tok_surf_t is array (1 to MAXN) of string(1 to WORD_MAX);

  -- helpers pack/unpack de punteros
  function pack(i,j,s : natural) return integer is
  begin
    return integer(((i*1000)+j)*10 + s);
  end function;

  function eq_token(a : string(1 to WORD_MAX); alen : natural;
                    b : string(1 to WORD_MAX); blen : natural) return boolean is
  begin
    if alen /= blen then return false; end if;
    for k in 1 to alen loop
      if a(k) /= b(k) then return false; end if;
    end loop;
    return true;
  end function;

  function lex_lookup(w : string(1 to WORD_MAX); wlen : natural) return sym_id_t is
  begin
    for i in 0 to N_LEX-1 loop
      if eq_token(w, wlen, LEXICON(i).w, LEXICON(i).len) then
        return LEXICON(i).pos;
      end if;
    end loop;
    return FALLBACK_SYM;
  end function;

begin
  process(clk)
    -- estado persistente
    variable n_v      : natural range 0 to MAXN := 0;
    variable chart_v  : chart_t;
    variable surf_v   : tok_surf_t;

    -- procedimientos internos (actualizan chart_v)
    procedure clear_cell(i,j : natural) is
    begin
      for s in 1 to BEAM loop
        chart_v(i,j,s).cat  := 0;
        chart_v(i,j,s).sc   := NEG;
        chart_v(i,j,s).kind := 0;
        chart_v(i,j,s).a    := 0;
        chart_v(i,j,s).b    := 0;
      end loop;
    end procedure;

    procedure insert_cell(i,j : natural; newIt : item_t; changed : out boolean) is
      variable same_s  : natural := 0;
      variable empty_s : natural := 0;
      variable worst_s : natural := 1;
      variable worst_sc: score_t := chart_v(i,j,1).sc;
    begin
      changed := false;

      -- dedupe por cat
      for s in 1 to BEAM loop
        if chart_v(i,j,s).cat = newIt.cat then
          same_s := s;
        end if;
        if chart_v(i,j,s).cat = 0 and empty_s = 0 then
          empty_s := s;
        end if;
        if chart_v(i,j,s).sc < worst_sc then
          worst_sc := chart_v(i,j,s).sc;
          worst_s  := s;
        end if;
      end loop;

      if same_s /= 0 then
        if newIt.sc > chart_v(i,j,same_s).sc then
          chart_v(i,j,same_s) := newIt;
          changed := true;
        end if;
        return;
      end if;

      if empty_s /= 0 then
        chart_v(i,j,empty_s) := newIt;
        changed := true;
        return;
      end if;

      if newIt.sc > chart_v(i,j,worst_s).sc then
        chart_v(i,j,worst_s) := newIt;
        changed := true;
      end if;
    end procedure;

    procedure unary_close(i,j : natural) is
      variable changed_any : boolean;
      variable changed_one : boolean;
      variable childIt     : item_t;
      variable newIt       : item_t;
      variable childPtr    : integer;
    begin
      for iter in 1 to 16 loop
        changed_any := false;

        for s in 1 to BEAM loop
          childIt := chart_v(i,j,s);
          if childIt.cat /= 0 then
            childPtr := pack(i,j,s);

            for r in 0 to N_UNARY-1 loop
              if UNARY_RULES(r).rhs = childIt.cat then
                newIt.cat  := UNARY_RULES(r).lhs;
                newIt.sc   := childIt.sc + UNARY_RULES(r).logw;
                newIt.kind := 1;
                newIt.a    := childPtr;
                newIt.b    := 0;

                insert_cell(i,j,newIt,changed_one);
                if changed_one then changed_any := true; end if;
              end if;
            end loop;

          end if;
        end loop;

        exit when not changed_any;
      end loop;
    end procedure;

    procedure combine(i,k,j : natural) is
      variable L : item_t;
      variable R : item_t;
      variable newIt : item_t;
      variable dummy : boolean;
      variable ptrL, ptrR : integer;
    begin
      for sl in 1 to BEAM loop
        L := chart_v(i,k,sl);
        if L.cat /= 0 then
          ptrL := pack(i,k,sl);

          for sr in 1 to BEAM loop
            R := chart_v(k,j,sr);
            if R.cat /= 0 then
              ptrR := pack(k,j,sr);

              for rr in 0 to N_BINARY-1 loop
                if (BINARY_RULES(rr).rhs1 = L.cat) and (BINARY_RULES(rr).rhs2 = R.cat) then
                  newIt.cat  := BINARY_RULES(rr).lhs;
                  newIt.sc   := L.sc + R.sc + BINARY_RULES(rr).logw;
                  newIt.kind := 2;
                  newIt.a    := ptrL;
                  newIt.b    := ptrR;
                  insert_cell(i,j,newIt,dummy);
                end if;
              end loop;

            end if;
          end loop;

        end if;
      end loop;
    end procedure;

    -- util: calcular accept y best root
    procedure compute_root is
      variable j   : natural;
      variable bestS : natural := 0;
      variable bestSc: score_t := NEG;
    begin
      if n_v = 0 then
        accept <= '0';
        best_root_ptr <= 0;
        return;
      end if;

      j := n_v + 1;

      for s in 1 to BEAM loop
        if chart_v(1,j,s).cat = START_SYM then
          if chart_v(1,j,s).sc > bestSc then
            bestSc := chart_v(1,j,s).sc;
            bestS  := s;
          end if;
        end if;
      end loop;

      if bestS = 0 then
        accept <= '0';
        best_root_ptr <= 0;
      else
        accept <= '1';
        best_root_ptr <= pack(1,j,bestS);
      end if;
    end procedure;

    -- init chart (solo en reset)
    procedure clear_all is
    begin
      n_v := 0;
      for i in 1 to MAXN loop
        for j in 1 to MAXJ loop
          clear_cell(i,j);
        end loop;
      end loop;
    end procedure;

    -- temporales de step
    variable pos : sym_id_t;
    variable t   : natural;
    variable j   : natural;
    variable it  : item_t;
    variable changed_one : boolean;
  begin
    if rising_edge(clk) then
      done <= '0';

      if rst = '1' then
        clear_all;
        compute_root;
      else
        if token_valid = '1' then
          if n_v < MAXN then
            -- Step(token)
            pos := lex_lookup(token_str, token_len);

            n_v := n_v + 1;
            t   := n_v;
            j   := t + 1;
            surf_v(t) := token_str;

            -- lexical cell (t, t+1)
            clear_cell(t,j);
            it.cat  := pos;
            it.sc   := 0;
            it.kind := 0;
            it.a    := integer(t); -- token index
            it.b    := 0;

            insert_cell(t,j,it,changed_one);
            unary_close(t,j);

            -- spans ending at j
            if t >= 2 then
              for ii in reverse 1 to t-1 loop
                clear_cell(ii, j);
                for kk in ii+1 to j-1 loop
                  combine(ii,kk,j);
                end loop;
                unary_close(ii,j);
              end loop;
            end if;

            compute_root;
            done <= '1';
          else
            -- ignore if over MAXN
            compute_root;
            done <= '1';
          end if;
        end if;
      end if;

      token_count <= n_v;

      -- Debug read (combinado en el flanco; suficiente para testbench)
      dbg_cat  <= chart_v(dbg_i, dbg_j, dbg_s).cat;
      dbg_sc   <= chart_v(dbg_i, dbg_j, dbg_s).sc;
      dbg_kind <= chart_v(dbg_i, dbg_j, dbg_s).kind;
      dbg_a    <= chart_v(dbg_i, dbg_j, dbg_s).a;
      dbg_b    <= chart_v(dbg_i, dbg_j, dbg_s).b;
    end if;
  end process;

end architecture;
