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

   function Convert_Wide_Wide_Char_To_Wide_Char
     (Wide_Wide_Char : Wide_Wide_Character) return Wide_Character is
   begin
      Code : constant Natural := Wide_Wide_Character'Pos (Wide_Wide_Char);
      if Code <= Wide_Character'Pos (Wide_Character'Last) then
         return Wide_Character'Val (Code);
      else
         return 'X';
      end if;
   exception
      when Constraint_Error =>
         return 'X';
   end Convert_Wide_Wide_Char_To_Wide_Char;

   procedure Encode_UCS2_Wide_Char_As_UTF8_Buffer
     (Wide_Char                 : Wide_Character;
      Output_Buffer             : out UTF8_Encoded_UCS2_Char_Buffer_T;
      Output_Buffer_Byte_Length : out Natural)
   is
      Code : constant Natural := Wide_Character'Pos (Wide_Char);
   begin
      --  Reject surrogate range (invalid Unicode scalar values)
      if Code in 16#D800# .. 16#DFFF# then
         raise Constraint_Error;
      end if;

      if Code <= 16#7F# then
         --  1-byte UTF-8
         Output_Buffer (Output_Buffer'First) := Character'Val (Code);
         Output_Buffer_Byte_Length := 1;

      elsif Code <= 16#7FF# then
         --  2-byte UTF-8
         Output_Buffer (Output_Buffer'First) :=
           Character'Val (16#C0# or (Unsigned_16 (Code) / 16#40#));
         Output_Buffer (Output_Buffer'First + 1) :=
           Character'Val (16#80# or (Unsigned_16 (Code) and 16#3F#));
         Output_Buffer_Byte_Length := 2;

      else
         --  3-byte UTF-8
         Output_Buffer (Output_Buffer'First) :=
           Character'Val (16#E0# or (Unsigned_16 (Code) / 16#1000#));
         Output_Buffer (Output_Buffer'First + 1) :=
           Character'Val
             (16#80# or ((Unsigned_16 (Code) / 16#40#) and 16#3F#));
         Output_Buffer (Output_Buffer'First + 2) :=
           Character'Val (16#80# or (Unsigned_16 (Code) and 16#3F#));
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
