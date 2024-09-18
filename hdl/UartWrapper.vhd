-- Copyright Till Straumann, 2024. Licensed under the EUPL-1.2 or later.
-- You may obtain a copy of the license at
--   https://joinup.ec.europa.eu/collection/eupl/eupl-text-eupl-12
-- This notice must not be removed.

-- Wrapper aggregating the UartRx, UartTx, rate-generator and
-- a divider to compute the fractional rate divisor.

library ieee;
use     ieee.std_logic_1164.all;
use     ieee.numeric_std.all;
use     work.UartPkg.all;

entity UartWrapper is
   generic (
      -- main clock frequency
      CLOCK_FREQ_G   : natural;
      -- max # of bits of rate
      LD_RATE_G      : natural := 22;
      -- max. data width
      DATA_WIDTH_G   : natural := 8;
      -- Debounce (number of clk cycles) period when
      -- detecting the leading edge of the start bit
      -- (and the end of a break condition).
      -- Note: DEBOUNCE_G must be < min. rate-period / 2
      DEBOUNCE_G     : natural := 0;
      -- may selectively disable instantiation of Rx or Tx
      USE_TX_G       : boolean := true;
      USE_RX_G       : boolean := true
   );
   port (
      clk            : in  std_logic;
      rst            : in  std_logic := '0';

      rxData         : out std_logic_vector(DATA_WIDTH_G - 1 downto 0);
      -- data are consumed when (rxDataVld and rxDataRdy) = '1'
      rxDataRdy      : in  std_logic := '1';
      rxDataVld      : out std_logic := '0';
      -- asserted while a break condition is present
      rxBreak        : out std_logic := '0';
      -- asserted when a parity error is detected (dataVld is not asserted)
      -- cleared when 'clearErrors' is asserted or at the next start bit.
      rxParityErr    : out std_logic := '0';
      -- asserted when a framing error is detected (dataVld is not asserted)
      -- cleared when 'clearErrors' is asserted or at the next start bit.
      -- A break condition will also result in a framing error.
      rxFramingErr   : out std_logic := '0';
      -- asserted when an overrun is detected, i.e. when new data are
      -- shifted in while dataRdy is asserted. Old data are discarded
      -- and the new data are latched (which eventually results in
      -- both, 'dataVld' as well as 'overrunErr' being asserted). The
      -- 'overrunErr' simply means that previous data were lost; the
      -- data marked as 'dataVld' are still good.
      -- Cleared when 'clearErrors' is asserted or at the next start bit
      -- (i.e., at the start of the frame following the one which caused
      -- the overrun).
      rxOverrunErr   : out std_logic := '0';

      txData         : in  std_logic_vector(DATA_WIDTH_G - 1 downto 0) := (others => '0');
      -- data are consumed during cycle with (txDataVld and txDataRdy) = '1'
      txDataVld      : in  std_logic := '0';
      txDataRdy      : out std_logic := '0';
      -- send break
      txBreak        : in  std_logic := '0';
      -- expected data format (parity, stop bits etc.)
      cfgParity      : in  UartParityType;
      cfgStopBits    : in  UartStopBitType;
      cfgNumBits     : in  natural range 1 to DATA_WIDTH_G;
      cfgBitRateHz   : in  unsigned(LD_RATE_G - 1 downto 0) := to_unsigned(9600, LD_RATE_G);
      -- clear errors; may be permanently asserted which causes all
      -- error flags to be asserted for a single cycle.
      clearErrors    : in  std_logic := '1';

      -- serial data input; NOTE: no internal synchronizer present
      rxSerial       : in  std_logic := '1';
      -- serial data output
      txSerial       : out std_logic := '1'
   );
end entity UartWrapper;

architecture rtl of UartWrapper is
   constant LD_CLOCK_FREQ_C : natural := uartNumBits(CLOCK_FREQ_G);
   constant INIT_DIV_C      : natural := CLOCK_FREQ_G / 9600;

   signal   divDon          : std_logic;
   signal   rateDivisor     : unsigned(LD_CLOCK_FREQ_C - 1 downto 0) := to_unsigned(INIT_DIV_C, LD_CLOCK_FREQ_C);
   signal   rateFractNum    : unsigned(LD_RATE_G - 1 downto 0)       := to_unsigned(0,          LD_RATE_G      );
   signal   rateDivisorIn   : unsigned(LD_CLOCK_FREQ_C - 1 downto 0);
   signal   rateFractNumIn  : unsigned(LD_RATE_G - 1 downto 0);

   signal   rxRateEnable    : std_logic;
   signal   rxRateSync      : std_logic;
   signal   txRateEnable    : std_logic;
   signal   txDataRdyLoc    : std_logic := '0';
begin

   U_DIV : entity work.LongDivider
      generic map (
         Q_SIZE_G           => LD_CLOCK_FREQ_C,
         D_SIZE_G           => LD_RATE_G
      )
      port map (
         clk                => clk,
         rst                => rst,
         -- run continuously
         vld                => '1',
         rdy                => open,
         don                => divDon,
         dividend           => to_unsigned(CLOCK_FREQ_G, LD_CLOCK_FREQ_C),
         divisor            => cfgBitRateHz,
         quotient           => rateDivisorIn,
         remainder          => rateFractNumIn
      );

   P_REG_DIV : process ( clk ) is
   begin
      if ( rising_edge(clk) ) then
         if ( rst = '1' ) then
            rateDivisor  <= to_unsigned(INIT_DIV_C, rateDivisor'length );
            rateFractNum <= to_unsigned(0,          rateFractNum'length);
         elsif ( divDon = '1' ) then
            rateDivisor  <= rateDivisorIn;
            rateFractNum <= rateFractNumIn;
         end if;
      end if;
   end process P_REG_DIV;

   G_RX : if ( USE_RX_G ) generate

      U_RX_RATE : entity work.RateGenerator
         generic map (
            DIV_WIDTH_G        => LD_CLOCK_FREQ_C,
            DEN_WIDTH_G        => LD_RATE_G,
            -- RX rate has a phase offset of 1/2 bit-time
            INI_PHOFF_G        => true
         )
         port map (
            clk                => clk,
            rst                => rxRateSync,
            div                => rateDivisor,
            num                => rateFractNum,
            den                => cfgBitRateHz,
            rateEnable         => rxRateEnable
         );

      U_RX : entity work.UartRx
         generic map (
            DATA_WIDTH_G       => DATA_WIDTH_G,
            DEBOUNCE_G         => DEBOUNCE_G
         )
         port map (
            clk                => clk,
            rst                => rst,

            rateEnable         => rxRateEnable,
            waitSync           => rxRateSync,

            data               => rxData,
            dataRdy            => rxDataRdy,
            dataVld            => rxDataVld,

            break              => rxBreak,
            parityErr          => rxParityErr,
            framingErr         => rxFramingErr,
            overrunErr         => rxOverrunErr,

            parity             => cfgParity,
            stopBits           => cfgStopBits,
            numBits            => cfgNumBits,

            clearErrors        => clearErrors,

            serialRx           => rxSerial
         );

   end generate G_RX;

   G_TX : if ( USE_TX_G ) generate

      U_TX_RATE : entity work.RateGenerator
         generic map (
            DIV_WIDTH_G        => LD_CLOCK_FREQ_C,
            DEN_WIDTH_G        => LD_RATE_G,
            INI_PHOFF_G        => false
         )
         port map (
            clk                => clk,
            rst                => txDataRdyLoc,
            div                => rateDivisor,
            num                => rateFractNum,
            den                => cfgBitRateHz,
            rateEnable         => txRateEnable
         );

      U_TX : entity work.UartTx
         generic map (
            DATA_WIDTH_G       => DATA_WIDTH_G
         )
         port map (
            clk                => clk,
            rst                => rst,

            rateEnable         => txRateEnable,

            data               => txData,
            dataVld            => txDataVld,
            dataRdy            => txDataRdyLoc,

            break              => txBreak,
            parity             => cfgParity,
            stopBits           => cfgStopBits,
            numBits            => cfgNumBits,

            serialTx           => txSerial
         );

   end generate G_TX;

   txDataRdy <= txDataRdyLoc;
   
end architecture rtl;
