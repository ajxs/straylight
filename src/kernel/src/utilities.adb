-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

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

end Utilities;
