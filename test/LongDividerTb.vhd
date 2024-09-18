-- Copyright Till Straumann, 2024. Licensed under the EUPL-1.2 or later.
-- You may obtain a copy of the license at
--   https://joinup.ec.europa.eu/collection/eupl/eupl-text-eupl-12
-- This notice must not be removed.

library ieee;
use     ieee.std_logic_1164.all;
use     ieee.numeric_std.all;

entity LongDividerTb is
end entity LongDividerTb;

architecture sim of LongDividerTb is
   constant Q_SIZE_C : natural            := 6;
   constant D_SIZE_C : natural            := 6;
   signal dividend   : unsigned(Q_SIZE_C - 1 downto 0) := to_unsigned(13, Q_SIZE_C);
   signal quotient   : unsigned(Q_SIZE_C - 1 downto 0) := to_unsigned(13, Q_SIZE_C);
   signal divisor    : unsigned(D_SIZE_C - 1 downto 0) := to_unsigned( 3, D_SIZE_C);
   signal remainder  : unsigned(D_SIZE_C - 1 downto 0) := to_unsigned( 3, D_SIZE_C);
   signal clk      : std_logic            := '0';
   signal rst      : std_logic            := '0';
   signal vld      : std_logic            := '0';
   signal rdy      : std_logic            := '0';
   signal don      : std_logic            := '0';
   signal run      : boolean              := true;

   procedure tick(constant n : natural := 1) is
   begin
      for i in 1 to n loop
         wait until rising_edge(clk);
      end loop;
   end procedure tick;
begin

   P_CLK : process is
   begin
      wait for 5 ns; clk <= not clk;
      if ( not run ) then wait; end if;
   end process P_CLK;

   P_DRV : process is
      variable n : natural := 0;
   begin
      tick;
      tick;
      for dvdn in 0 to 2**Q_SIZE_C - 1 loop
         for dvsr in 1 to 2**D_SIZE_C - 1 loop
            vld <= '1';
            dividend <= to_unsigned(dvdn, dividend'length);
            divisor  <= to_unsigned(dvsr, divisor'length);
            while ( (vld and rdy) = '0' ) loop
               tick;
            end loop;
            vld <= '0';
            tick;
            while ( don = '0' ) loop
               tick;
            end loop;
            assert to_integer(quotient) = dvdn/dvsr report "quotient mismatch" severity failure;
            assert to_integer(remainder) = dvdn mod dvsr report "remainder mismatch" severity failure;
            n := n + 1;
         end loop;
      end loop;
      report "TEST PASSED: " & integer'image(n);
      run <= false; 
      wait;
   end process P_DRV;

   DUT : entity work.LongDivider
      generic map (
         Q_SIZE_G  => Q_SIZE_C,
         D_SIZE_G  => D_SIZE_C
      )
      port map (
         clk       => clk,
         rst       => rst,
         vld       => vld,
         rdy       => rdy,
         dividend  => dividend,
         divisor   => divisor,
         quotient  => quotient,
         remainder => remainder,
         don       => don
      );
end architecture sim;
