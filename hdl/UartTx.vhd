-- Copyright Till Straumann, 2024. Licensed under the EUPL-1.2 or later.
-- You may obtain a copy of the license at
--   https://joinup.ec.europa.eu/collection/eupl/eupl-text-eupl-12
-- This notice must not be removed.

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
      shftReg   : std_logic_vector(DATA_WIDTH_G downto 0);
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
               v.shftReg  := (others => '1');
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
