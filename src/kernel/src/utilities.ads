-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Interfaces; use Interfaces;

package Utilities is
   pragma Preelaborate;

   function Convert_BEU32_To_LEU32 (BEU32 : Unsigned_32) return Unsigned_32
   with
     Pure_Function,
     Import,
     Convention    => Assembler,
     External_Name => "utilities_convert_beu32_to_leu32";

   function Convert_BEU64_To_LEU64 (BEU64 : Unsigned_64) return Unsigned_64
   with
     Pure_Function,
     Import,
     Convention    => Assembler,
     External_Name => "utilities_convert_beu64_to_leu64";

   function Convert_Wide_Char_To_ASCII
     (Wide_Char : Wide_Character) return Character
   with Pure_Function;

   type UTF8_Converted_Char_Buffer_T is array (1 .. 4) of Character;

   procedure Encode_UCS2_Wide_Char_As_UTF8_Buffer
     (Wide_Char                 : Wide_Character;
      Output_Buffer             : out UTF8_Converted_Char_Buffer_T;
      Output_Buffer_Byte_Length : out Natural);

end Utilities;
