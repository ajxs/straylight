-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  Authors:
--     Anthony <ajxs [at] panoptic.online>
-------------------------------------------------------------------------------

package body Straylight.Common is
   function Convert_BEU32_To_LEU32 (BEU32 : Unsigned_32) return Unsigned_32 is
      Integer_Bytes : array (1 .. 4) of Unsigned_8
      with Import, Address => BEU32'Address;
   begin
      return
        Shift_Left (Unsigned_32 (Integer_Bytes (1)), 24)
        + Shift_Left (Unsigned_32 (Integer_Bytes (2)), 16)
        + Shift_Left (Unsigned_32 (Integer_Bytes (3)), 8)
        + Unsigned_32 (Integer_Bytes (4));
   end Convert_BEU32_To_LEU32;

   function Convert_BEU64_To_LEU64 (BEU64 : Unsigned_64) return Unsigned_64 is
      Integer_Bytes : array (1 .. 8) of Unsigned_8
      with Import, Address => BEU64'Address;
   begin
      return
        Shift_Left (Unsigned_64 (Integer_Bytes (1)), 56)
        + Shift_Left (Unsigned_64 (Integer_Bytes (2)), 48)
        + Shift_Left (Unsigned_64 (Integer_Bytes (3)), 40)
        + Shift_Left (Unsigned_64 (Integer_Bytes (4)), 32)
        + Shift_Left (Unsigned_64 (Integer_Bytes (5)), 24)
        + Shift_Left (Unsigned_64 (Integer_Bytes (6)), 16)
        + Shift_Left (Unsigned_64 (Integer_Bytes (7)), 8)
        + Unsigned_64 (Integer_Bytes (8));
   end Convert_BEU64_To_LEU64;

   function Get_Address_Word_High (Addr : Address) return Unsigned_32 is
   begin
      return Unsigned_32 (Shift_Right (Unsigned_64 (To_Integer (Addr)), 32));
   exception
      when Constraint_Error =>
         return 0;
   end Get_Address_Word_High;

   function Get_Address_Word_Low (Addr : Address) return Unsigned_32 is
   begin
      return Unsigned_32 (Unsigned_64 (To_Integer (Addr) and 16#FFFF_FFFF#));
   exception
      when Constraint_Error =>
         return 0;
   end Get_Address_Word_Low;
end Straylight.Common;
