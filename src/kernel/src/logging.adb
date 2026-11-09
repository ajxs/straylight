-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Utilities;             use Utilities;
with Logging.Debug_Console; use Logging.Debug_Console;

package body Logging is
   procedure Log_Message
     (Message : String; Level : Log_Level_T; File : String; Line : Positive) is
   begin
      if Active_Logging_Transports (Log_Transport_Debug_Console) then
         Log_To_Debug_Console (Message, Level, File, Line);
      end if;
   end Log_Message;

   procedure Log_Message_Wide
     (Wide_Message : Wide_String;
      Level        : Log_Level_T;
      File         : String;
      Line         : Positive) is
   begin
      declare
         Message : String (1 .. Wide_Message'Length);
      begin
         for I in Wide_Message'Range loop
            Message (I) := Convert_Wide_Char_To_ASCII (Wide_Message (I));
         end loop;

         if Active_Logging_Transports (Log_Transport_Debug_Console) then
            Log_To_Debug_Console (Message, Level, File, Line);
         end if;
      end;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint error in Log_Message_Wide");
   end Log_Message_Wide;

   procedure Log_Debug
     (Message : String;
      File    : String := GNAT.Source_Info.File;
      Line    : Positive := GNAT.Source_Info.Line) is
   begin
      Log_Message (Message, Log_Level_Debug, File, Line);
   end Log_Debug;

   procedure Log_Debug_Wide
     (Message : Wide_String;
      File    : String := GNAT.Source_Info.File;
      Line    : Positive := GNAT.Source_Info.Line) is
   begin
      Log_Message_Wide (Message, Log_Level_Debug, File, Line);
   end Log_Debug_Wide;

   procedure Log_Error
     (Message : String;
      File    : String := GNAT.Source_Info.File;
      Line    : Positive := GNAT.Source_Info.Line) is
   begin
      Log_Message (Message, Log_Level_Error, File, Line);
   end Log_Error;

   procedure Log_Constraint_Error
     (File : String := GNAT.Source_Info.File;
      Line : Positive := GNAT.Source_Info.Line) is
   begin
      Log_Message ("Constraint_Error", Log_Level_Error, File, Line);
   end Log_Constraint_Error;

end Logging;
