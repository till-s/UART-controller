-- Copyright Till Straumann, 2024. Licensed under the EUPL-1.2 or later.
-- You may obtain a copy of the license at
--   https://joinup.ec.europa.eu/collection/eupl/eupl-text-eupl-12
-- This notice must not be removed.

library ieee;
use     ieee.std_logic_1164.all;
use     ieee.numeric_std.all;
use     work.UartPkg.all;

entity UartTb is
end entity UartTb;

architecture sim of UartTb is
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
