-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Interfaces; use Interfaces;

package Utilities is
   pragma Preelaborate;

   function Convert_BEU32_To_LEU32 (BEU32 : Unsigned_32) return Unsigned_32
   is (Shift_Left (BEU32 and 16#0000_00FF#, 24)
       or Shift_Left (BEU32 and 16#0000_FF00#, 8)
       or Shift_Right (BEU32 and 16#00FF_0000#, 8)
       or Shift_Right (BEU32 and 16#FF00_0000#, 24))
   with Pure_Function;

   function Convert_BEU64_To_LEU64 (BEU64 : Unsigned_64) return Unsigned_64
   is (Shift_Left (BEU64 and 16#0000_0000_0000_00FF#, 56)
       or Shift_Left (BEU64 and 16#0000_0000_0000_FF00#, 40)
       or Shift_Left (BEU64 and 16#0000_0000_00FF_0000#, 24)
       or Shift_Left (BEU64 and 16#0000_0000_FF00_0000#, 8)
       or Shift_Right (BEU64 and 16#0000_00FF_0000_0000#, 8)
       or Shift_Right (BEU64 and 16#0000_FF00_0000_0000#, 24)
       or Shift_Right (BEU64 and 16#00FF_0000_0000_0000#, 40)
       or Shift_Right (BEU64 and 16#FF00_0000_0000_0000#, 56))
   with Pure_Function;

   function Convert_Wide_Char_To_ASCII
     (Wide_Char : Wide_Character) return Character
   with Pure_Function;

   function ASCII_To_Wide_Char (ASCII_Char : Character) return Wide_Character
   with Pure_Function;

   type UTF8_Converted_Char_Buffer_T is array (1 .. 4) of Character;

   procedure Encode_UCS2_Wide_Char_As_UTF8_Buffer
     (Wide_Char                 : Wide_Character;
      Output_Buffer             : out UTF8_Converted_Char_Buffer_T;
      Output_Buffer_Byte_Length : out Natural);

   procedure Set_Fixed_Length_String (Input : String; Dest_Str : out String);

end Utilities;
