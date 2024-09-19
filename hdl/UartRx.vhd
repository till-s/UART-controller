-- Copyright Till Straumann, 2024. Licensed under the EUPL-1.2 or later.
-- You may obtain a copy of the license at
--   https://joinup.ec.europa.eu/collection/eupl/eupl-text-eupl-12
-- This notice must not be removed.

library ieee;
use     ieee.std_logic_1164.all;
use     ieee.numeric_std.all;
use     work.UartPkg.all;

-- Simple UART receiver sampling at bit-mid points.
entity UartRx is
   generic (
      -- max. data width
      DATA_WIDTH_G   : natural := 8;
      -- Debounce (number of clk cycles) period when
      -- detecting the leading edge of the start bit
      -- (and the end of a break condition).
      -- Note: DEBOUNCE_G must be < min. rate-period / 2
      DEBOUNCE_G     : natural := 0
   );
   port (
      clk            : in  std_logic;
      rst            : in  std_logic;
      -- clock-enable at the desired bit-rate
      rateEnable     : in  std_logic;
      data           : out std_logic_vector(DATA_WIDTH_G - 1 downto 0);
      -- data are consumed when (dataVld and dataRdy) = '1'
      dataRdy        : in  std_logic := '1';
      dataVld        : out std_logic;
      -- asserted while a break condition is present
      break          : out std_logic;
      -- asserted when a parity error is detected (dataVld is not asserted)
      -- cleared when 'clearErrors' is asserted or at the next start bit.
      parityErr      : out std_logic;
      -- asserted when a framing error is detected (dataVld is not asserted)
      -- cleared when 'clearErrors' is asserted or at the next start bit.
      -- A break condition will also result in a framing error.
      framingErr     : out std_logic;
      -- asserted when an overrun is detected, i.e. when new data are
      -- shifted in while dataRdy is asserted. Old data are discarded
      -- and the new data are latched (which eventually results in
      -- both, 'dataVld' as well as 'overrunErr' being asserted). The
      -- 'overrunErr' simply means that previous data were lost; the
      -- data marked as 'dataVld' are still good.
      -- Cleared when 'clearErrors' is asserted or at the next start bit
      -- (i.e., at the start of the frame following the one which caused
      -- the overrun).
      overrunErr     : out std_logic;
      -- sync signal to reset the rate-generator
      waitSync       : out std_logic;
      -- expected data format (parity, stop bits etc.)
      parity         : in  UartParityType;
      stopBits       : in  UartStopBitType;
      numBits        : in  natural range 1 to DATA_WIDTH_G;
      -- clear errors; may be permanently asserted which causes all
      -- error flags to be asserted for a single cycle.
      clearErrors    : in  std_logic;
      -- serial data input
      serialRx       : in  std_logic
   );
end entity UartRx;

architecture rtl of UartRx is

   function max(constant a,b: in integer) return integer is
   begin
      if ( a > b ) then return a; else return b; end if;
   end function max;

   constant MAX_CNT_C : natural := max( DEBOUNCE_G, DATA_WIDTH_G );

   -- -1 based count + extra bit for sign
   subtype CountType is signed(uartNumBits(MAX_CNT_C) downto 0);

   function toCount(constant x : in integer) return CountType is
   begin
      return CountType(to_signed(x - 2, CountType'length));
   end function toCount;

   type StateType is ( PREP, IDLE, START_BIT, SHIFT_DATA, PARITY_BIT, STOP1_BIT, STOP2_BIT, WAIT_EOF );

   type RegType is record
      state     : StateType;
      shftReg   : std_logic_vector(DATA_WIDTH_G - 1 downto 0);
      count     : CountType;
      sync      : std_logic;
      break     : std_logic;
      maybeBrk  : std_logic;
      breakCnt  : CountType;
      parity    : std_logic;
      parityErr : std_logic;
      framingErr: std_logic;
      overrunErr: std_logic;
      vld       : std_logic;
   end record RegType;

   constant REG_INIT_C : RegType := (
      state     => PREP,
      shftReg   => (others => '0'),
      count     => toCount( DEBOUNCE_G ),
      sync      => '1',
      break     => '0',
      maybeBrk  => '0',
      breakCnt  => (others => '1'),
      parity    => '0',
      parityErr => '0',
      framingErr=> '0',
      overrunErr=> '0',
      vld       => '0'
   );

   signal r     : RegType := REG_INIT_C;
   signal rin   : RegType;

begin

   P_COMB : process ( r, rateEnable, dataRdy, parity, stopBits, numBits, serialRx, clearErrors ) is
      variable v : RegType;
   begin
      v := r;

      if ( ( r.vld and dataRdy ) = '1' ) then
         v.vld := '0';
      end if;

      if ( clearErrors = '1' ) then
         v.overrunErr := '0';
         v.framingErr := '0';
         v.parityErr  := '0';
      end if;

      if ( rateEnable = '1' ) then
         v.parity   := r.parity xor serialRx;
         v.maybeBrk := r.maybeBrk and not serialRx;
         v.count    := r.count - 1;
         v.breakCnt := r.breakCnt - 1;

         case ( r.state ) is
            when PREP       => -- should never get here
            when IDLE       =>
               v.state      := PREP;
               v.framingErr := '1';
            when START_BIT  =>
               v.maybeBrk   := '1';
               v.parity     := '0';
               v.count      := toCount( numBits );
               v.state      := SHIFT_DATA;
               v.parityErr  := '0';
               v.framingErr := '0';
               v.overrunErr := '0';
               -- unacked data? withdraw vld and flag overrun
               if ( (r.vld and not dataRdy) = '1' ) then
                  v.vld := '0';
                  v.overrunErr := '1';
               end if;
               if ( serialRx = '1' ) then
                  v.framingErr := '1';
                  v.state      := PREP;
               end if;
            when SHIFT_DATA =>
               v.shftReg              := '0' & r.shftReg(r.shftReg'left downto 1);
               v.shftReg(numBits - 1) := serialRx;
               if ( r.count < 0 ) then
                 if ( parity /= NONE ) then
                    v.state := PARITY_BIT;
                 else
                    v.state := STOP1_BIT;
                 end if;
               end if;
            when PARITY_BIT =>
               case (parity) is
                  when MARK  => v.parityErr := not serialRx;
                  when SPACE => v.parityErr := serialRx;
                  when others =>
                     if ( (parity = ODD) /= ( r.parity /= serialRx ) ) then
                        v.parityErr := '1';
                     end if;
               end case;
               v.state      := STOP1_BIT;

            when STOP1_BIT | STOP2_BIT  =>
               if ( serialRx = '0' ) then
                  v.framingErr := '1';
                  v.state      := WAIT_EOF;
                  v.break      := r.maybeBrk;
                  -- approximate count to signal break;
                  v.breakCnt   := to_signed( numBits, v.breakCnt'length );
                  v.count      := toCount( DEBOUNCE_G );
               else
                  if ( stopBits = TWO and r.state /= STOP2_BIT ) then
                     -- treat 1.5 as one
                     v.state := STOP2_BIT;
                  else
                     v.state := PREP;
                     v.vld   := not r.parityErr;
                  end if;
               end if;
              
            when WAIT_EOF   =>
               if ( r.breakCnt < 0 ) then
                  v.break := '1'; -- may already have been signalled
               end if;
         end case;
      end if;

      if ( r.state = PREP ) then
         v.sync  := '1';
         v.break := '0';
         v.count := toCount( DEBOUNCE_G );
         v.state := IDLE;
      elsif ( r.state = IDLE ) then
         if ( serialRx = '0' ) then
            v.sync  := '0';
            v.count := r.count - 1;
            if ( r.count < 0 ) then
               v.state := START_BIT;
            end if;
         else
            v.count := toCount( DEBOUNCE_G );
         end if;
      elsif ( r.state = WAIT_EOF ) then
         if ( serialRx = '0' ) then
            v.count := toCount( DEBOUNCE_G );
         else
            v.count := r.count - 1;
            -- require serialRx to be '1' in addition to the
            -- debounce count having expired (may be 0, i.e, always
            -- expired!)
            if ( r.count < 0 ) then
               v.state := PREP;
            end if;
         end if;
      end if;

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

   data           <= r.shftReg;
   dataVld        <= r.vld;
   break          <= r.break;
   parityErr      <= r.parityErr;
   framingErr     <= r.framingErr;
   overrunErr     <= r.overrunErr;
   -- in PREP state the rate-div RST must already be asserted
   -- so that in IDLE state no rate pulse can happen => use v.sync!
   waitSync       <= rin.sync and serialRx;
end architecture rtl;
