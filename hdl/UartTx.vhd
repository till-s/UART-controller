package UartPkg is
   type UartParityType  is (NONE, EVEN, ODD, MARK, SPACE);
   type UartStopBitType is (ONE, ONEP5, TWO);

   function uartNumBits(constant x : in natural) return natural;
end package UartPkg;

package body UartPkg is
   function uartNumBits(constant x : in natural) return natural is
      variable ld  : natural;
      variable cmp : natural;
   begin
      ld  := 1;
      cmp := 2;
      while ( x >= cmp ) loop 
         cmp := cmp * 2;
         ld  := ld + 1;
      end loop;
      return ld;
   end function uartNumBits;
end package body UartPkg;

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

library ieee;
use     ieee.std_logic_1164.all;
use     ieee.numeric_std.all;
use     work.UartPkg.all;

entity UartTx is
   generic (
      -- max. data width
      DATA_WIDTH_G   : natural := 8
   );
   port (
      clk            : in  std_logic;
      rst            : in  std_logic;
      -- clock-enable at the desired bit-rate
      rateEnable     : in  std_logic;
      data           : in  std_logic_vector(DATA_WIDTH_G - 1 downto 0);
      -- data are consumed during cycle with (dataVld and dataRdy) = '1'
      dataVld        : in  std_logic;
      dataRdy        : out std_logic;
      -- send break
      break          : in  std_logic := '0';
      parity         : in  UartParityType;
      stopBits       : in  UartStopBitType;
      numBits        : in  natural range 1 to DATA_WIDTH_G;
      -- serial data output
      serialTx       : out std_logic
   );
end entity UartTx;

architecture rtl of UartTx is

   function tailLength(constant p : in UartParityType; constant s : in UartStopBitType) return  natural is
      variable v : natural;
   begin
      -- treat 1.5 stop bits as two
      if ( s = ONE ) then
         v := 1;
      else
         v := 2;
      end if;
      if ( p /= NONE ) then
         v := v + 1;
      end if;
      return v;
   end function tailLength;

   type StateType is (IDLE, SHIFT_DATA, SHIFT_TAIL, SEND_BREAK);

   type RegType is record
      state     : StateType;
      rdy       : std_logic;
      shftReg   : std_logic_vector(DATA_WIDTH_G - 1 downto -1);
      parity    : std_logic;
      -- max. count is DATA_WIDTH_G + 1 (including start bit);
      -- however, the count is -1 based, so subtract 2
      -- => DATA_WIDTH_G + 1 - 2
      count     : signed(uartNumBits(DATA_WIDTH_G - 1) downto 0);
   end record RegType;

   constant REG_INIT_C : RegType := (
      state     => IDLE,
      rdy       => '1',
      shftReg   => (others => '1'),
      parity    => '0',
      count     => (others => '1')
   );

   signal r     : RegType := REG_INIT_C;
   signal rin   : RegType;

begin

   P_COMB : process( r, rateEnable, data, dataVld, parity, stopBits, numBits, break ) is
      variable v : RegType;
   begin
      v       := r;

      if ( rateEnable = '1' ) then
         v.shftReg := '1' & r.shftReg(r.shftReg'left downto r.shftReg'right + 1);
         v.count   := r.count -1;

         case (r.state) is
            when SHIFT_DATA =>
               -- use *next* data bit
               v.parity := r.parity xor r.shftReg(r.shftReg'right + 1);
               if ( r.count < 0 ) then
                  v.count := to_signed( tailLength( parity, stopBits ) - 2, v.count'length);
                  v.state := SHIFT_TAIL;
                  if ( parity = SPACE ) then
                     v.shftReg(v.shftReg'right) := '0';
                  elsif ( parity = EVEN ) then
                     v.shftReg(v.shftReg'right) := r.parity;
                  elsif ( parity = ODD  ) then
                     v.shftReg(v.shftReg'right) := not r.parity;
                  end if;
               end if;
            when SHIFT_TAIL =>
               if ( r.count < 0 ) then
                  v.state := IDLE;
                  v.rdy   := '1';
               end if;
            when others =>
         end case;
      end if;

      if ( r.state = IDLE ) then
         if ( break = '1' ) then
            v.state := SEND_BREAK;
            v.rdy   := '0';
         elsif ( dataVld = '1' ) then
            v.shftReg := data & '0'; -- start bit
            v.shftReg(r.shftReg'left downto numBits) := (others => '1');
            v.rdy     := '0';
            v.state   := SHIFT_DATA;
            -- must include start bit 
            v.count   := to_signed(numBits - 1, v.count'length);
            v.parity  := '0';
         end if;
      elsif ( r.state = SEND_BREAK ) then
         v.shftReg(v.shftReg'right) := '0';
         if ( break = '0' ) then
            v.shftReg(v.shftReg'right) := '1';
            v.rdy   := '1';
            v.state := IDLE;
         end if;
      end if;

      rin     <= v;
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

   dataRdy  <= r.rdy and not break;
   serialTx <= r.shftReg(r.shftReg'right);
end architecture rtl;

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
   subtype CountType is signed(uartNumBits(MAX_CNT_C - 2) downto 0);

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
      count     => to_signed( DEBOUNCE_G - 2, CountType'length ),
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
               v.count      := to_signed(numBits - 2, v.count'length);
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
                  v.breakCnt   := to_signed(numBits, v.breakCnt'length);
                  v.count      := to_signed(DEBOUNCE_G - 2, v.count'length);
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
         v.count := to_signed(DEBOUNCE_G - 2, v.count'length);
         v.state := IDLE;
      elsif ( r.state = IDLE ) then
         if ( serialRx = '0' ) then
            v.sync  := '0';
            v.count := r.count - 1;
            if ( r.count < 0 ) then
               v.state := START_BIT;
            end if;
         else
            v.count := to_signed(DEBOUNCE_G - 2, v.count'length);
         end if;
      elsif ( r.state = WAIT_EOF ) then
         if ( serialRx = '0' ) then
            v.count := to_signed(DEBOUNCE_G - 2, v.count'length);
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

library ieee;
use     ieee.std_logic_1164.all;
use     ieee.numeric_std.all;
use     work.UartPkg.all;

entity UartTxTb is
end entity UartTxTb;

architecture sim of UartTxTb is
   constant PER_C      : natural := 4 - 2;
   signal parityRx     : UartParityType := ODD;
   signal parityTx     : UartParityType := EVEN;
   signal stopBits     : UartStopBitType := TWO;
   signal dataTx       : std_logic_vector(7 downto 0) := x"a5";
   signal dataRx       : std_logic_vector(7 downto 0);
   signal clk          : std_logic := '0';
   signal div          : signed(3 downto 0) := to_signed(PER_C, 4);
   signal rst          : std_logic := '0';
   signal vldTx        : std_logic := '0';
   signal rdyTx        : std_logic := '0';
   signal rdyRx        : std_logic := '0';
   signal vldRx        : std_logic := '0';
   signal breakTx      : std_logic := '0';
   signal breakRx      : std_logic;
   signal parityErr    : std_logic;
   signal framingErr   : std_logic;
   signal overrunErr   : std_logic;
   signal serialTx     : std_logic := '0';
   signal rateEnableTx : std_logic := '0';
   signal rateEnableRx : std_logic := '0';
   signal rateSyncRx   : std_logic := '0';
   signal numBits      : natural   := 6;
   signal run          : boolean   := true;
   signal count        : natural   := 0;
   signal clearErrRx   : std_logic := '0';

   procedure tick(constant n : in natural := 1) is
   begin
      for i in 1 to n loop
         wait until rising_edge(clk);
      end loop;
   end procedure tick;

   procedure send(
      signal   d : inout std_logic_vector(7 downto 0);
      signal   v : inout std_logic; 
      constant x : in    std_logic_vector(7 downto 0)
   ) is
   begin
      d <= x;
      v <= '1';
      while ( (v and rdyTx) = '0' ) loop
         tick;
      end loop;
      v <= '0';
   end procedure send;

   procedure recv(
      variable y : out   std_logic_vector(7 downto 0);
      signal   r : inout std_logic
   ) is
   begin
      r <= '1';
      while ( (r and vldRx) = '0' ) loop
         tick;
      end loop;
      y := dataRx;
      r <= '0';
   end procedure recv;

   procedure clerr(signal s : out std_logic) is
   begin
      s <= '1';
      tick;
      s <= '0';
      tick;
   end procedure clerr;

   procedure assertNoErrs is
   begin
      assert parityErr  = '0' report "unexpected parity error"  severity failure;
      assert framingErr = '0' report "unexpected framing error" severity failure;
      assert overrunErr = '0' report "unexpected overrun error" severity failure;
   end procedure assertNoErrs;
 
begin

   P_CLK : process is
   begin
      wait for 10 ns;
      clk <= not clk;
      if ( not run ) then
         wait;
      end if;
   end process P_CLK;

   P_DRV : process is
      variable rdData : std_logic_vector(7 downto 0);
      variable wrData : std_logic_vector(7 downto 0);
   begin
      tick;
      tick;
      for p in UartParityType'low to UartParityType'high loop
         parityTx <= p;
         parityRx <= p;
         report UartParityType'image(p);
         tick;
         for i in 0 to 63 loop
            wrData := std_logic_vector(to_unsigned(i, 8));
            send( dataTx, vldTx, wrData );
            recv( rdData, rdyRx );
            assert to_integer(unsigned(rdData(numBits - 1 downto 0))) = i report "readback data mismatch" severity failure;
            assert breakRx   = '0' report "unexpected break" severity failure;
            assertNoErrs;
         end loop;
      end loop;
      tick;
      parityTx <= EVEN;
      parityRx <= ODD;
      tick;
      send(dataTx, vldTx, x"a5");
      for i in 1 to 100 loop
         rdyRx <= '1';
         tick;
         assert vldRx = '0' report "unexpected valid" severity failure;
      end loop;
      rdyRx <= '0';
      assert parityErr = '1' report "parity error expected" severity failure;
      parityRx <= EVEN;
      tick;
      send(dataTx, vldTx, x"a3");
      tick;
      send(dataTx, vldTx, x"a2");
      tick(100);
      assert parityErr = '0' report "unexpected parity error" severity failure;
      assert overrunErr = '1' report "overrun error expected" severity failure;
      clerr( clearErrRx );
      assertNoErrs;

      breakTx <= '1';
      tick(100);
      assert breakRx    = '1' report "break expected" severity failure;
      assert framingErr = '1' report "framing error expected" severity failure;
      breakTx <= '0';
      clerr( clearErrRx );
      tick(100);
      assertNoErrs;
      assert breakRx = '0' report "unexpected break" severity failure;
      wrData := x"31";
      send(dataTx, vldTx, wrData);
      recv( rdData, rdyRx );
      assert rdData(numBits - 1 downto 0) = wrData(numBits - 1 downto 0) report "data mismatch after break" severity failure;
      
      
      report "TEST PASSED";
      tick(100);
      run <= false;
      wait;
   end process P_DRV;

--   rateEnable <= div(div'left);

   U_DIV_TX : entity work.RateGenerator
      generic map (
         DIV_WIDTH_G    => 4,
         DEN_WIDTH_G    => 4
      )
      port map (
         clk            => clk,
         rst            => rdyTx,
         div            => to_unsigned(3,4),
         num            => to_unsigned(1,4),
         den            => to_unsigned(3,4),
         rateEnable     => rateEnableTx
      );

   U_DUT_TX : entity work.UartTx
      port map (
         clk            => clk,
         rst            => rst,
         rateEnable     => rateEnableTx,
         data           => dataTx,
         break          => breakTx,
         dataVld        => vldTx,
         dataRdy        => rdyTx,
         parity         => parityTx,
         stopBits       => stopBits,
         numBits        => numBits,
         serialTx       => serialTx
      );

   U_DIV_RX : entity work.RateGenerator
      generic map (
         DIV_WIDTH_G    => 4,
         DEN_WIDTH_G    => 4,
         INI_PHOFF_G    => true
      )
      port map (
         clk            => clk,
         rst            => rateSyncRx,
         div            => to_unsigned(3,4),
         num            => to_unsigned(1,4),
         den            => to_unsigned(3,4),
         rateEnable     => rateEnableRx
      );

   U_DUT_RX : entity work.UartRx
      generic map (
         DEBOUNCE_G     => 1
      )
      port map (
         clk            => clk,
         rst            => '0',
         rateEnable     => rateEnableRx,
         data           => dataRx,
         dataRdy        => rdyRx,
         dataVld        => vldRx,
         break          => breakRx,
         parityErr      => parityErr,
         framingErr     => framingErr,
         overrunErr     => overrunErr,
         waitSync       => rateSyncRx,
         parity         => parityRx,
         stopBits       => stopBits,
         numBits        => numBits,
         clearErrors    => clearErrRx,
         serialRx       => serialTx
      );

end architecture sim;
