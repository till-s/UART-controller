-- Copyright Till Straumann, 2024. Licensed under the EUPL-1.2 or later.
-- You may obtain a copy of the license at
--   https://joinup.ec.europa.eu/collection/eupl/eupl-text-eupl-12
-- This notice must not be removed.

library ieee;
use     ieee.std_logic_1164.all;
use     ieee.numeric_std.all;
use     work.UartPkg.all;

-- Bit-rate generator with fractional divider.
entity RateGenerator is
   generic (
      -- bit-width of integer part
      DIV_WIDTH_G : natural;
      -- bit-width of fractional part (numerator = denominator)
      DEN_WIDTH_G : natural;
      -- phase offset 0.5 period enable; when this is 'true'
      -- the first 'rateEnable' pulse (after 'rst' is released)
      -- is generated at approximately 1/2 of the integer divisor
      -- (useful when sampling bits between transitions). Otherwise
      -- the first pulse is generated at round(div + num/den) clock
      -- cycles.
      INI_PHOFF_G : boolean := false
   );
   port (
      -- clock
      clk         : in  std_logic;
      rst         : in  std_logic := '0';
      -- integer part of divisor
      div         : in  unsigned(DIV_WIDTH_G - 1 downto 0);
      -- numerator of fractional part
      num         : in  unsigned(DEN_WIDTH_G - 1 downto 0);
      -- denominator of fractional part
      den         : in  unsigned(DEN_WIDTH_G - 1 downto 0);
      -- divided output; asserted for 1 clock cycle.
      rateEnable  : out std_logic
   );
end entity RateGenerator;

architecture rtl of RateGenerator is
   -- delta keeps track of the deviation of the actual phase from
   -- desired phase. It is offset by -den/2, i.e., delta=0 corresponds
   -- to a phase in the middle between two clk cycles. 
   -- The integer division is set to either 'div' or 'div + 1', depending
   -- on the accumulated phase error.
   --  a) when dividing by 'div':
   --     actual:  phase(n+1) = phase(n) + div
   --     desired: phase(n+1) = phase(n) + div + num/den
   --     desired - actual = num/den
   --  a) when dividing by 'div + 1':
   --     actual:  phase(n+1) = phase(n) + div + 1
   --     desired: phase(n+1) = phase(n) + div + num/den
   --     desired - actual = (num - den)/den
   --  => after 'n' steps (a) the accumuated error is n*num/den; once that
   --     reaches 0.5 we flip to (b) at which point the error decreases
   --     with each step (num < den => num-den < 0) and we go back to (a)
   --     as soon as the error falls below 0.5.
   --     We keep 'delta' not as a fraction, of course but multiplied
   --     by 'den'; and to make math easier we offset 'delta' by -0.5 den:
   --         -> when delta >=0 => divide by (div + 1), delta += (num - den)
   --         -> when delta < 0 => divide by (div), delta += num
   signal delta      : signed(DEN_WIDTH_G downto 0) := (others => '1');
   -- counter/divisor; this is -1 based, i.e., counts from 'div-2' downto '-1'
   signal count      : signed(DIV_WIDTH_G downto 0) := (others => '1');
   signal rateEnLoc  : std_logic;
begin

   -- count < 0 and delta < 0
   rateEnLoc <= count(count'left) and delta(delta'left);

   P_DIV : process ( clk ) is
   begin
      if ( rising_edge( clk ) ) then
         if ( (rst or rateEnLoc) = '1' ) then
            if ( INI_PHOFF_G and (rst = '1') ) then
               -- initial count is 1/2 div to offset by 1/2 output clock
               -- (approximately). Count is -1 based but avoid initializing
               -- the count to -1 (if div == 2) which would lead to the first
               -- rate pulse in INIT state.
               count <= '0' & signed(shift_right(div,1)) - 1;
            else
               -- count is -1 based => subtract 2.
               count <= '0' & signed(div) - 2;
            end if;
         else
            count <= count - 1;
         end if;
         if ( rst = '1' ) then
            -- first division is by 'div' (not 'div+1'); pre-compute delta:
            --   delta := num - den/2;
            -- the '-den/2' accounts for the offset so we can use the sign
            -- to decide whether to divide by 'div' or 'div+1'.
            delta <= ('0' & signed(num)) - ( '0' & signed(shift_right(den,1)) );
         elsif ( count < 0 ) then
            -- count has dropped
            if ( delta >= 0 ) then
               -- if 'delta' > 0 then we must delay the output
               -- by one extra cycle and compute 'delta += num - den'.
               -- We do the second term here;
               -- note that 'rateEnLoc' also looks at the sign bit
               -- of 'delta' and hence is *not* asserted during this
               -- cycle (this adds the extra cycle delay to the output)
               -- 'count' will decrement to -2 (does no harm) and the
               -- subtraction of 'den' will render 'delta' negative during
               -- the next cycle which will (during the next cycle)
               --  => assert rateEnLoc
               --  => end up adding 'num' (else branch of this statement)
               delta <= delta - ('0' & signed(den));
            else
               delta <= delta + ('0' & signed(num));
            end if;
         end if;
      end if;
   end process P_DIV;

   rateEnable <= rateEnLoc;

end architecture rtl;
