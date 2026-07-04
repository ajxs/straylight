-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Utilities;             use Utilities;
with Logging.Debug_Console; use Logging.Debug_Console;

package body Logging is
   function Should_Log_To_Debug_Console
     (Tags : Log_Tags; Level : Log_Level_T) return Boolean is
   begin
      if Level = Log_Level_Error then
         return True;
      end if;

      if Level > System_Logging_Level then
         return False;
      end if;

      for Tag of Tags loop
         if Active_Logging_Tags (Tag) then
            return True;
         end if;
      end loop;

      return False;
   end Should_Log_To_Debug_Console;

   procedure Log_Message
     (Message : String; Tags : Log_Tags; Level : Log_Level_T) is
   begin
      if Should_Log_To_Debug_Console (Tags, Level) then
         if Active_Logging_Transports (Log_Transport_Debug_Console) then
            Log_To_Debug_Console (Message, Level);
         end if;
      end if;
   end Log_Message;

   procedure Log_Message_Wide
     (Wide_Message : Wide_String; Tags : Log_Tags; Level : Log_Level_T) is
   begin
      if not Should_Log_To_Debug_Console (Tags, Level) then
         return;
      end if;

      declare
         Message : String (1 .. Wide_Message'Length);
      begin
         for I in Wide_Message'Range loop
            Message (I) := Convert_Wide_Char_To_ASCII (Wide_Message (I));
         end loop;

         if Active_Logging_Transports (Log_Transport_Debug_Console) then
            Log_To_Debug_Console (Message, Level);
         end if;
      end;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint error in Log_Message_Wide");
   end Log_Message_Wide;

   procedure Log_Debug (Message : String; Tags : Log_Tags := Empty_Tag_List) is
   begin
      Log_Message (Message, Tags, Log_Level_Debug);
   end Log_Debug;

   procedure Log_Debug_Wide
     (Message : Wide_String; Tags : Log_Tags := Empty_Tag_List) is
   begin
      Log_Message_Wide (Message, Tags, Log_Level_Debug);
   end Log_Debug_Wide;

   procedure Log_Error (Message : String; Tags : Log_Tags := Empty_Tag_List) is
   begin
      Log_Message (Message, Tags, Log_Level_Error);
   end Log_Error;

end Logging;
