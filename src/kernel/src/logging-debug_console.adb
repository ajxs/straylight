-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Interfaces; use Interfaces;
with System;     use System;

with Hart_State; use Hart_State;
with Locks;      use Locks;
with Memory;     use Memory;
with RISCV;
with RISCV.SBI;

package body Logging.Debug_Console is
   ----------------------------------------------------------------------------
   --  Printing a string to the SBI Debug Console requires knowing the
   --  physical address of the string to be printed. Most strings will be
   --  printed from the stack, which technically doesn't have a fixed physical
   --  address. To facilitate this, a static buffer is used to store the string
   --  before it's sent to the SBI Debug Console.
   --  The buffer is a fixed size, and the string is copied into the buffer
   --  in blocks of this size.
   ----------------------------------------------------------------------------
   SBI_Log_Buffer_Length : constant := 128;

   ----------------------------------------------------------------------------
   --  Statically-allocated buffer used to transfer strings to the SBI Debug
   --  Console.
   --  This needs to have a static address, so that we can easily derive the
   --  physical address to pass to the SBI call.
   ----------------------------------------------------------------------------
   SBI_Logging_Buffer : String (1 .. SBI_Log_Buffer_Length);

   SBI_Logging_Buffer_Spinlock : Spinlock_T :=
     (Locked        => 0,
      Time_Acquired => 0,
      Hart_Id       => No_Hart_Id,
      Lock_Id       => Lock_Id_SBI_Log_Buffer);

   procedure Print_String_To_SBI_Console (Message : String) is
      Print_Index : Integer := 1;
   begin
      --  Get the physical address of the SBI logging buffer.
      --  This works because the SBI buffer is statically allocated in the
      --  package, so it has a fixed address in the lower-half address space.
      Physical_Address : constant Address :=
        Convert_Higher_Half_Address_To_Lower (SBI_Logging_Buffer'Address);

      --  Copy the message into the SBI logging buffer in blocks, printing with
      --  each iteration, until the whole message has been printed.
      while Print_Index < Message'Length loop
         Print_Length : constant Integer :=
           (if Print_Index + SBI_Log_Buffer_Length - 1 > Message'Length
            then Message'Length - Print_Index + 1
            else SBI_Log_Buffer_Length);

         SBI_Logging_Buffer (1 .. Print_Length) :=
           Message (Print_Index .. Print_Index + Print_Length - 1);

         SBI_Result : constant RISCV.SBI.SBI_Result_T :=
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

   procedure Log_To_Debug_Console (Message : String; Level : Log_Level_T) is
      System_Time : constant Unsigned_64 := RISCV.Get_System_Time;
   begin
      Acquire_Spinlock (SBI_Logging_Buffer_Spinlock);
      Print_String_To_SBI_Console
        ((if Level = Log_Level_Error
          then "Error: "
          elsif Level = Log_Level_Info
          then "Info: "
          else "Debug: ")
         & System_Time'Image
         & ": "
         & Message
         & ASCII.LF);
      Release_Spinlock (SBI_Logging_Buffer_Spinlock);
   exception
      when Constraint_Error =>
         Panic ("Constraint_Error: Log_To_Debug_Console");
   end Log_To_Debug_Console;

end Logging.Debug_Console;
