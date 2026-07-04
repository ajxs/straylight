-------------------------------------------------------------------------------
--  Copyright (c) 2026, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

package body Logging is

   procedure Log_Error (Message : String; Tags : Log_Tags := Empty_Tag_List) is
      pragma Unreferenced (Message, Tags);
   begin
      null;
   end Log_Error;

   procedure Log_Debug (Message : String; Tags : Log_Tags := Empty_Tag_List) is
      pragma Unreferenced (Message, Tags);
   begin
      null;
   end Log_Debug;

   procedure Log_Debug_Wide
     (Message : Wide_String; Tags : Log_Tags := Empty_Tag_List)
   is
      pragma Unreferenced (Message, Tags);
   begin
      null;
   end Log_Debug_Wide;
end Logging;
