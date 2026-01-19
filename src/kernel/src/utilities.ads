-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

package Utilities is
   pragma Preelaborate;

   function Convert_Wide_Char_To_ASCII
     (Wide_Char : Wide_Character) return Character
   with Pure_Function;

   type UTF8_Converted_Char_Bufer_T is array (1 .. 4) of Character;

   procedure Encode_UCS2_Wide_Char_As_UTF8_Buffer
     (Wide_Char                 : Wide_Character;
      Output_Buffer             : out UTF8_Converted_Char_Bufer_T;
      Output_Buffer_Byte_Length : out Natural);

end Utilities;
