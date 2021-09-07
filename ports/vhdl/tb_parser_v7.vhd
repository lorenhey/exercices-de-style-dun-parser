library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.Resources_Pkg.all;

entity tb_parser_v7 is
end;

architecture tb of tb_parser_v7 is
  constant MAXN : natural := 64;
  constant BEAM : natural := 8;

  signal clk, rst : std_logic := '0';
  signal token_valid : std_logic := '0';
  signal token_str   : string(1 to WORD_MAX) := (others => ' ');
  signal token_len   : natural range 0 to WORD_MAX := 0;

  signal done   : std_logic;
  signal accept : std_logic;
  signal best_root_ptr : integer;
  signal token_count : natural range 0 to MAXN;

  -- debug
  signal dbg_i : natural range 1 to MAXN := 1;
  signal dbg_j : natural range 1 to MAXN+1 := 2;
  signal dbg_s : natural range 1 to BEAM := 1;
  signal dbg_cat : sym_id_t;
  signal dbg_sc  : score_t;
  signal dbg_kind: natural range 0 to 2;
  signal dbg_a, dbg_b : integer;

  procedure push_token(w : in string) is
    variable L : natural := w'length;
    variable tmp : string(1 to WORD_MAX) := (others => ' ');
  begin
    if L > WORD_MAX then L := WORD_MAX; end if;
    for i in 1 to L loop
      tmp(i) := w(i);
    end loop;
    token_str <= tmp;
    token_len <= L;
    token_valid <= '1';
    wait until rising_edge(clk);
    token_valid <= '0';
    wait until rising_edge(clk); -- deja que done pulse
  end procedure;

begin
  dut: entity work.Parser_V7
    generic map (MAXN => MAXN, BEAM => BEAM)
    port map (
      clk => clk, rst => rst,
      token_valid => token_valid,
      token_str => token_str,
      token_len => token_len,
      done => done,
      accept => accept,
      best_root_ptr => best_root_ptr,
      token_count => token_count,
      dbg_i => dbg_i, dbg_j => dbg_j, dbg_s => dbg_s,
      dbg_cat => dbg_cat, dbg_sc => dbg_sc, dbg_kind => dbg_kind,
      dbg_a => dbg_a, dbg_b => dbg_b
    );

  -- clock
  clk <= not clk after 5 ns;

  stim: process
  begin
    rst <= '1';
    wait until rising_edge(clk);
    rst <= '0';
    wait until rising_edge(clk);

    -- "El filosofo murio"
    push_token("el");
    push_token("filosofo");
    push_token("murio");

    report "N=" & integer'image(token_count)
      & " ACCEPT=" & std_logic'image(accept)
      & " ROOT_PTR=" & integer'image(best_root_ptr);

    wait for 50 ns;
    report "DONE." severity note;
    wait;
  end process;

end architecture;
