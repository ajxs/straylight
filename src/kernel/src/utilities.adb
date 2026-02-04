-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Interfaces; use Interfaces;

with Logging; use Logging;

package body Utilities is
   function Convert_Wide_Char_To_ASCII
     (Wide_Char : Wide_Character) return Character is
   begin
      if Wide_Character'Pos (Wide_Char) <= Character'Pos (Character'Last) then
         return Character'Val (Wide_Character'Pos (Wide_Char));
      end if;

      return ' ';
   exception
      when Constraint_Error =>
         return ' ';
   end Convert_Wide_Char_To_ASCII;

   procedure Encode_UCS2_Wide_Char_As_UTF8_Buffer
     (Wide_Char                 : Wide_Character;
      Output_Buffer             : out UTF8_Converted_Char_Buffer_T;
      Output_Buffer_Byte_Length : out Natural)
   is
      Code : constant Natural := Wide_Character'Pos (Wide_Char);

      UTF8_One_Byte_Max        : constant := 16#7F#;
      UTF8_Two_Byte_Max        : constant := 16#7FF#;
      UTF8_Two_Byte_Prefix     : constant := 16#C0#;
      UTF8_Three_Byte_Prefix   : constant := 16#E0#;
      UTF8_Continuation_Prefix : constant := 16#80#;
      UTF8_Continuation_Mask   : constant := 16#3F#;
      Surrogate_Range_Start    : constant := 16#D800#;
      Surrogate_Range_End      : constant := 16#DFFF#;
   begin
      Output_Buffer := [others => Character'Val (0)];

      --  Reject surrogate range (invalid Unicode scalar values)
      if Code in Surrogate_Range_Start .. Surrogate_Range_End then
         raise Constraint_Error;
      end if;

      if Code <= UTF8_One_Byte_Max then
         --  1-byte UTF-8
         Output_Buffer (Output_Buffer'First) := Character'Val (Code);
         Output_Buffer_Byte_Length := 1;
      elsif Code <= UTF8_Two_Byte_Max then
         --  2-byte UTF-8
         Output_Buffer (Output_Buffer'First) :=
           Character'Val
             (UTF8_Two_Byte_Prefix or Shift_Right (Unsigned_16 (Code), 6));

         Output_Buffer (Output_Buffer'First + 1) :=
           Character'Val
             (UTF8_Continuation_Prefix
              or (Unsigned_16 (Code) and UTF8_Continuation_Mask));

         Output_Buffer_Byte_Length := 2;
      else
         --  3-byte UTF-8
         Output_Buffer (Output_Buffer'First) :=
           Character'Val
             (UTF8_Three_Byte_Prefix or Shift_Right (Unsigned_16 (Code), 12));

         Output_Buffer (Output_Buffer'First + 1) :=
           Character'Val
             (UTF8_Continuation_Prefix
              or
                ((Shift_Right (Unsigned_16 (Code), 6))
                 and UTF8_Continuation_Mask));

         Output_Buffer (Output_Buffer'First + 2) :=
           Character'Val
             (UTF8_Continuation_Prefix
              or (Unsigned_16 (Code) and UTF8_Continuation_Mask));

         Output_Buffer_Byte_Length := 3;
      end if;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Encode_UCS2_Wide_Char_As_UTF8_Buffer");

         --  On error, output replacement character U+FFFD
         --  Refer to: https://en.wikipedia.org/wiki/Specials_(Unicode_block)
         Output_Buffer (Output_Buffer'First) := Character'Val (16#EF#);
         Output_Buffer (Output_Buffer'First + 1) := Character'Val (16#BF#);
         Output_Buffer (Output_Buffer'First + 2) := Character'Val (16#BD#);
         Output_Buffer_Byte_Length := 3;
   end Encode_UCS2_Wide_Char_As_UTF8_Buffer;

end Utilities;
