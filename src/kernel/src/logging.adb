-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Utilities;             use Utilities;
with Logging.Debug_Console; use Logging.Debug_Console;

package body Logging is
   function Are_Given_Tags_Active (Tags : Log_Tags) return Boolean is
   begin
      for Tag of Tags loop
         if Active_Logging_Tags (Tag) then
            return True;
         end if;
      end loop;

      return False;
   end Are_Given_Tags_Active;

   function Should_Log_To_Debug_Console
     (Tags : Log_Tags; Level : Log_Level_T) return Boolean
   is (Level = Log_Level_Error
       or else
         (Level <= System_Logging_Level
          and then Are_Given_Tags_Active (Tags)));

   procedure Log_Message
     (Message : String; Tags : Log_Tags; Level : Log_Level_T) is
   begin
      if Active_Logging_Transports (Log_Transport_Debug_Console)
        and then Should_Log_To_Debug_Console (Tags, Level)
      then
         Log_To_Debug_Console (Message, Level);
      end if;
   end Log_Message;

   procedure Log_Message_Wide
     (Wide_Message : Wide_String; Tags : Log_Tags; Level : Log_Level_T) is
   begin
      declare
         Message : String (1 .. Wide_Message'Length);
      begin
         for I in Wide_Message'Range loop
            Message (I) := Convert_Wide_Char_To_ASCII (Wide_Message (I));
         end loop;

         if Active_Logging_Transports (Log_Transport_Debug_Console)
           and then Should_Log_To_Debug_Console (Tags, Level)
         then
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
