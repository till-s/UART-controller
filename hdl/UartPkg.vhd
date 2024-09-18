-- Copyright Till Straumann, 2024. Licensed under the EUPL-1.2 or later.
-- You may obtain a copy of the license at
--   https://joinup.ec.europa.eu/collection/eupl/eupl-text-eupl-12
-- This notice must not be removed.

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
