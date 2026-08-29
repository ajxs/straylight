-------------------------------------------------------------------------------
--  Copyright (c) 2026, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

package body Logging is

   procedure Log_Error (Message : String) is
      pragma Unreferenced (Message);
   begin
      null;
   end Log_Error;

   procedure Log_Debug (Message : String) is
      pragma Unreferenced (Message);
   begin
      null;
   end Log_Debug;

   procedure Log_Debug_Wide (Message : Wide_String) is
      pragma Unreferenced (Message);
   begin
      null;
   end Log_Debug_Wide;

   procedure Log_Constraint_Error
     (File : String := GNAT.Source_Info.File;
      Line : Positive := GNAT.Source_Info.Line) is
   begin
      pragma Unreferenced (File, Line);
   end Log_Constraint_Error;
end Logging;
