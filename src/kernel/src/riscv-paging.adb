-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

package body RISCV.Paging is
   function Are_Address_Bits_39_To_63_Equal_To_Bit_38
     (Number : Unsigned_64) return Boolean
   is
      Bit_38 : constant Boolean := (Number and 16#4000000000#) /= 0;
      Mask   : constant Unsigned_64 := 16#FFFFFF8000000000#;
   begin
      if Bit_38 then
         return (Number and Mask) = Mask;
      end if;

      return (Number and Mask) = 0;
   end Are_Address_Bits_39_To_63_Equal_To_Bit_38;

   function Is_Table_Free (Page_Table : Page_Table_T) return Boolean is
   begin
      for Table_Entry of Page_Table loop
         if Table_Entry.V then
            return False;
         end if;
      end loop;

      return True;
   end Is_Table_Free;

   function Is_Valid_SV39_Virtual_Address
     (Virtual_Address : Address) return Boolean is
   begin
      return
        Are_Address_Bits_39_To_63_Equal_To_Bit_38
          (Unsigned_64 (To_Integer (Virtual_Address)));
   end Is_Valid_SV39_Virtual_Address;

   ----------------------------------------------------------------------------
   --  To_Page_Aligned_Address
   --
   --  Implementation Notes:
   --   - Converts the address to a 64 bit unsigned integer in order to
   --     properly truncate the value to the 4kb aligned 44-bit value.
   ----------------------------------------------------------------------------
   function To_Page_Aligned_Address
     (Addr : System.Address) return Page_Aligned_Address_T is
   begin
      return
        Page_Aligned_Address_T
          (Shift_Right (Unsigned_64 (To_Integer (Addr)), 12));
   exception
      when Constraint_Error =>
         return 0;
   end To_Page_Aligned_Address;

end RISCV.Paging;
