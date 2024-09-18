-- Copyright Till Straumann, 2024. Licensed under the EUPL-1.2 or later.
-- You may obtain a copy of the license at
--   https://joinup.ec.europa.eu/collection/eupl/eupl-text-eupl-12
-- This notice must not be removed.

-- trivial divider

library ieee;
use     ieee.std_logic_1164.all;
use     ieee.numeric_std.all;

entity LongDivider is
   generic (
      -- size of dividend/quotient
      Q_SIZE_G    : natural;
      -- size of divisor/remainder
      D_SIZE_G    : natural
   );
   port (
      clk         : in  std_logic;
      rst         : in  std_logic;
      vld         : in  std_logic;
      -- consume input
      rdy         : out std_logic;
      -- result ready; asserted for 1 cycle
      don         : out std_logic;
      dividend    : in  unsigned(Q_SIZE_G - 1 downto 0);
      divisor     : in  unsigned(D_SIZE_G - 1 downto 0);
      quotient    : out unsigned(Q_SIZE_G - 1 downto 0);
      remainder   : out unsigned(D_SIZE_G - 1 downto 0)
   );
end entity LongDivider;

architecture rtl of LongDivider is

   function nBits(constant x: in natural) return natural is
      variable lg : natural;
   begin
      lg := 1;
      while (x >= 2**lg) loop lg := lg + 1; end loop;
      return lg;
   end function nBits;

   type StateType is (IDLE, RUN);

   type RegType is record
      state       : StateType;
      a           : unsigned(Q_SIZE_G - 1 downto 0);
      b           : unsigned(D_SIZE_G - 1 downto 0);
      r           : unsigned(D_SIZE_G - 1 downto 0);
      cnt         : signed(nBits(Q_SIZE_G - 2) downto 0);
      don         : std_logic;
   end record RegType;

   constant REG_INIT_C : RegType := (
      state       => IDLE,
      a           => (others => '0'),
      b           => (others => '0'),
      r           => (others => '0'),
      cnt         => (others => '1'),
      don         => '0'
   );

   signal r       : RegType := REG_INIT_C;
   signal rin     : RegType;
   
begin

   P_COMB : process (r, dividend, divisor, vld) is
      variable v    : RegType;
      variable diff : signed(D_SIZE_G downto 0);
   begin

      v     := r;
      rdy   <= '0';
      v.r   := r.r(r.r'left - 1 downto 0) & r.a(r.a'left);
      v.a   := r.a(r.a'left - 1 downto 0) & '0';
      -- must keep the msbit or r;  r.r might be < r.b but
      -- still have it's msbit set. Must preserve when shifting;
      -- the difference is guaranteed to be < r.b:
      --    max. is   b-1 => after shift 2*(b-1) + 1 (shifted in from a)
      --     => 2*(b-1) + 1 - b = b-1 < b.
      diff  := signed(r.r(r.r'left) & v.r) - signed('0' & r.b);
      v.cnt := r.cnt - 1;
      v.don := '0';

      case (r.state) is
         when IDLE =>
            rdy <= '1';
            if ( vld = '1' ) then
               v.a     := dividend;
               v.b     := divisor;
               v.r     := (others => '0');
               v.cnt   := to_signed( Q_SIZE_G - 2, v.cnt'length );
               v.state := RUN;
            end if;

         when RUN =>
            if ( diff >= 0 ) then
               v.r    := unsigned(diff(v.r'range));
               v.a(0) := '1';
            end if;
            if ( r.cnt < 0 ) then
               v.don   := '1';
               v.state := IDLE;
            end if;

      end case;

      rin <= v;
   end process P_COMB;

   P_SEQ : process ( clk ) is
   begin
      if ( rising_edge( clk ) ) then
         if ( rst = '1' ) then
            r <= REG_INIT_C;
         else
            r <= rin;
         end if;
      end if;
   end process P_SEQ;

   don       <= r.don;
   quotient  <= r.a;
   remainder <= r.r;

end architecture rtl;
