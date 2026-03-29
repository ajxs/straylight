-------------------------------------------------------------------------------
--  Copyright (c) 2026, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with AUnit.Assertions; use AUnit.Assertions;

package body Utilities.Test_Cases is
   overriding
   procedure Run_Test (T : in out Test_Convert_Wide_Char_To_ASCII) is
      pragma Unreferenced (T);
   begin
      Assert
        (Convert_Wide_Char_To_ASCII ('A') = 'A',
         "ASCII 'A' round-trips correctly");
      Assert
        (Convert_Wide_Char_To_ASCII ('Z') = 'Z',
         "ASCII 'Z' round-trips correctly");
      Assert
        (Convert_Wide_Char_To_ASCII (' ') = ' ',
         "Space round-trips correctly");

      Assert
        (Convert_Wide_Char_To_ASCII (Wide_Character'Val (0))
         = Character'Val (0),
         "U+0000 (NUL) maps correctly");
      Assert
        (Convert_Wide_Char_To_ASCII (Wide_Character'Val (16#7F#))
         = Character'Val (16#7F#),
         "U+007F (DEL) maps correctly");
      Assert
        (Convert_Wide_Char_To_ASCII (Wide_Character'Val (16#FF#))
         = Character'Val (16#FF#),
         "U+00FF maps correctly");

      --  Non-ASCII wide characters should map to space
      Assert
        (Convert_Wide_Char_To_ASCII (Wide_Character'Val (16#100#)) = ' ',
         "U+0100 maps to space");
      Assert
        (Convert_Wide_Char_To_ASCII (Wide_Character'Val (16#FF01#)) = ' ',
         "U+FF01 maps to space");
   end Run_Test;

   overriding
   procedure Run_Test (T : in out Test_Encode_UCS2_Wide_Char_As_UTF8_Buffer) is
      pragma Unreferenced (T);
      Buffer : UTF8_Converted_Char_Buffer_T;
      Length : Natural;
   begin
      --  1-byte encoding: U+0000..U+007F
      Encode_UCS2_Wide_Char_As_UTF8_Buffer ('A', Buffer, Length);
      Assert (Length = 1, "U+0041 (A) encodes as 1 byte");
      Assert (Buffer (1) = 'A', "U+0041 byte value is correct");

      Encode_UCS2_Wide_Char_As_UTF8_Buffer
        (Wide_Character'Val (0), Buffer, Length);
      Assert (Length = 1, "U+0000 encodes as 1 byte");
      Assert (Character'Pos (Buffer (1)) = 0, "U+0000 byte is 0x00");

      Encode_UCS2_Wide_Char_As_UTF8_Buffer
        (Wide_Character'Val (16#7F#), Buffer, Length);
      Assert (Length = 1, "U+007F encodes as 1 byte");
      Assert (Character'Pos (Buffer (1)) = 16#7F#, "U+007F byte is 0x7F");

      --  2-byte encoding: U+0080..U+07FF
      --  U+0080: first 2-byte code point, expected 0xC2 0x80
      Encode_UCS2_Wide_Char_As_UTF8_Buffer
        (Wide_Character'Val (16#80#), Buffer, Length);
      Assert (Length = 2, "U+0080 encodes as 2 bytes");
      Assert (Character'Pos (Buffer (1)) = 16#C2#, "U+0080 byte 1 = 0xC2");
      Assert (Character'Pos (Buffer (2)) = 16#80#, "U+0080 byte 2 = 0x80");

      --  U+00E9 LATIN SMALL LETTER E WITH ACUTE (é), expected 0xC3 0xA9
      Encode_UCS2_Wide_Char_As_UTF8_Buffer
        (Wide_Character'Val (16#E9#), Buffer, Length);
      Assert (Length = 2, "U+00E9 (é) encodes as 2 bytes");
      Assert (Character'Pos (Buffer (1)) = 16#C3#, "U+00E9 byte 1 = 0xC3");
      Assert (Character'Pos (Buffer (2)) = 16#A9#, "U+00E9 byte 2 = 0xA9");

      --  U+07FF: last 2-byte code point, expected 0xDF 0xBF
      Encode_UCS2_Wide_Char_As_UTF8_Buffer
        (Wide_Character'Val (16#7FF#), Buffer, Length);
      Assert (Length = 2, "U+07FF encodes as 2 bytes");
      Assert (Character'Pos (Buffer (1)) = 16#DF#, "U+07FF byte 1 = 0xDF");
      Assert (Character'Pos (Buffer (2)) = 16#BF#, "U+07FF byte 2 = 0xBF");

      --  3-byte encoding: U+0800..U+FFFF (excluding surrogate range)
      --  U+0800: first 3-byte code point, expected 0xE0 0xA0 0x80
      Encode_UCS2_Wide_Char_As_UTF8_Buffer
        (Wide_Character'Val (16#0800#), Buffer, Length);
      Assert (Length = 3, "U+0800 encodes as 3 bytes");
      Assert (Character'Pos (Buffer (1)) = 16#E0#, "U+0800 byte 1 = 0xE0");
      Assert (Character'Pos (Buffer (2)) = 16#A0#, "U+0800 byte 2 = 0xA0");
      Assert (Character'Pos (Buffer (3)) = 16#80#, "U+0800 byte 3 = 0x80");

      --  U+4E2D CJK UNIFIED IDEOGRAPH (中), expected 0xE4 0xB8 0xAD
      Encode_UCS2_Wide_Char_As_UTF8_Buffer
        (Wide_Character'Val (16#4E2D#), Buffer, Length);
      Assert (Length = 3, "U+4E2D encodes as 3 bytes");
      Assert (Character'Pos (Buffer (1)) = 16#E4#, "U+4E2D byte 1 = 0xE4");
      Assert (Character'Pos (Buffer (2)) = 16#B8#, "U+4E2D byte 2 = 0xB8");
      Assert (Character'Pos (Buffer (3)) = 16#AD#, "U+4E2D byte 3 = 0xAD");

      --  Surrogate range: should produce replacement character U+FFFD.
      Encode_UCS2_Wide_Char_As_UTF8_Buffer
        (Wide_Character'Val (16#D800#), Buffer, Length);
      Assert
        (Length = 3, "Surrogate U+D800 produces replacement char (3 bytes)");
      Assert
        (Character'Pos (Buffer (1)) = 16#EF#,
         "Surrogate U+D800 replacement byte 1 = 0xEF");
      Assert
        (Character'Pos (Buffer (2)) = 16#BF#,
         "Surrogate U+D800 replacement byte 2 = 0xBF");
      Assert
        (Character'Pos (Buffer (3)) = 16#BD#,
         "Surrogate U+D800 replacement byte 3 = 0xBD");

      Encode_UCS2_Wide_Char_As_UTF8_Buffer
        (Wide_Character'Val (16#DFFF#), Buffer, Length);
      Assert
        (Length = 3, "Surrogate U+DFFF produces replacement char (3 bytes)");
      Assert
        (Character'Pos (Buffer (1)) = 16#EF#,
         "Surrogate U+DFFF replacement byte 1 = 0xEF");
   end Run_Test;

   overriding
   procedure Run_Test (T : in out Test_Convert_BEU32_To_LEU32) is
      pragma Unreferenced (T);
   begin
      Assert
        (Convert_BEU32_To_LEU32 (16#1234_5678#) = 16#7856_3412#,
         "BEU32 0x12345678 converts to LEU32 0x78563412");
      Assert
        (Convert_BEU32_To_LEU32 (16#0000_0000#) = 16#0000_0000#,
         "BEU32 0x00000000 converts to LEU32 0x00000000");
      Assert
        (Convert_BEU32_To_LEU32 (16#FFFF_FFFF#) = 16#FFFF_FFFF#,
         "BEU32 0xFFFFFFFF converts to LEU32 0xFFFFFFFF");
   end Run_Test;

   overriding
   procedure Run_Test (T : in out Test_Convert_BEU64_To_LEU64) is
      pragma Unreferenced (T);
   begin
      Assert
        (Convert_BEU64_To_LEU64 (16#0123_4567_89AB_CDEF#)
         = 16#EFCD_AB89_6745_2301#,
         "BEU64 0x0123456789ABCDEF converts to LEU64 0xEFCDAB8967452301");
      Assert
        (Convert_BEU64_To_LEU64 (16#0000_0000_0000_0000#)
         = 16#0000_0000_0000_0000#,
         "BEU64 0x0000000000000000 converts to LEU64 0x0000000000000000");
      Assert
        (Convert_BEU64_To_LEU64 (16#FFFF_FFFF_FFFF_FFFF#)
         = 16#FFFF_FFFF_FFFF_FFFF#,
         "BEU64 0xFFFFFFFFFFFFFFFF converts to LEU64 0xFFFFFFFFFFFFFFFF");
   end Run_Test;

end Utilities.Test_Cases;
