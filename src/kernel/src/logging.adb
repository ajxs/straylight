-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Interfaces; use Interfaces;
with System;     use System;

with Addresses;  use Addresses;
with RISCV;
with RISCV.SBI;
with Utilities;  use Utilities;
with Hart_State; use Hart_State;

package body Logging is
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

   procedure Log_To_Debug_Console (Message : String; Level : Log_Level_T) is
      System_Time : constant Unsigned_64 := RISCV.Get_System_Time;
   begin
      case Level is
         when Log_Level_Error =>
            Print_String_To_SBI_Console
              ("Error: " & System_Time'Image & ": " & Message & ASCII.LF);

         when Log_Level_Info  =>
            Print_String_To_SBI_Console
              ("Info: " & System_Time'Image & ": " & Message & ASCII.LF);

         when Log_Level_Debug =>
            Print_String_To_SBI_Console
              ("Debug: " & System_Time'Image & ": " & Message & ASCII.LF);
      end case;
   exception
      when Constraint_Error =>
         Panic ("Constraint_Error: Log_To_Debug_Console");
   end Log_To_Debug_Console;

   procedure Print_String_To_SBI_Console (Message : String) is
      Physical_Address : Address := Null_Address;

      SBI_Result   : RISCV.SBI.SBI_Result_T := (Error => 0, Value => 0);
      Print_Index  : Integer := 1;
      Print_Length : Integer := 0;
   begin
      --  Get the physical address of the SBI logging buffer.
      --  This works because the SBI buffer is statically allocated in the
      --  package, so it has a fixed address in the lower-half address space.
      Physical_Address :=
        Convert_Higher_Half_Address_To_Lower (SBI_Logging_Buffer'Address);

      --  Copy the message into the SBI logging buffer in blocks, printing with
      --  each iteration, until the whole message has been printed.
      while Print_Index < Message'Length loop
         if Print_Index + SBI_Log_Buffer_Length - 1 > Message'Length then
            Print_Length := Message'Length - Print_Index + 1;
         else
            Print_Length := SBI_Log_Buffer_Length;
         end if;

         SBI_Logging_Buffer (1 .. Print_Length) :=
           Message (Print_Index .. Print_Index + Print_Length - 1);

         SBI_Result :=
           RISCV.SBI.Debug_Console_Print
             (Unsigned_64 (Print_Length),
              Unsigned_64 (Get_Address_Word_Low (Physical_Address)),
              Unsigned_64 (Get_Address_Word_High (Physical_Address)));

         if SBI_Result.Error /= 0 then
            return;
         end if;

         Print_Index := Print_Index + SBI_Log_Buffer_Length;
      end loop;
   exception
      when Constraint_Error =>
         null;
   end Print_String_To_SBI_Console;

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

end Logging;
