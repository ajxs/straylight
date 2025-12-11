-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Interfaces; use Interfaces;

with Filesystems;      use Filesystems;
with Function_Results; use Function_Results;
with Logging;          use Logging;
with Processes;        use Processes;

package System_Calls is
   pragma Preelaborate;

   --  @TODO: Replace with main function result type?
   subtype Syscall_Result_T is Unsigned_64;

   Syscall_Result_Success : constant Syscall_Result_T := 0;
   --  Failure is given an arbitrary value to increase the Hamming distance
   --  between it and other values.
   Syscall_Result_Failure : constant Syscall_Result_T := 9998_0000;

   Syscall_Exit_Process    : constant := 5446_0000;
   Syscall_Yield_Process   : constant := 5446_0001;
   Syscall_Log_Debug       : constant := 5446_0002;
   Syscall_Log_Error       : constant := 5446_0003;
   Syscall_Allocate_Memory : constant := 5446_0004;
   Syscall_Free_Memory     : constant := 5446_0005;
   Syscall_Print_To_Serial : constant := 5446_0006;

   Syscall_Open_File : constant := 5446_0107;
   Syscall_Read_File : constant := 5446_0108;
   Syscall_Seek_File : constant := 5446_0109;

   Syscall_Update_Framebuffer : constant := 5446_0209;

   procedure Handle_User_Mode_Syscall
     (Process : in out Process_Control_Block_T; Result : out Function_Result);

private
   Logging_Tags : constant Log_Tags :=
     [Log_Tag_Processes, Log_Tag_System_Calls];

   procedure Handle_Logging_Syscall
     (Process   : in out Process_Control_Block_T;
      Log_Level : Log_Level_T;
      Result    : out Function_Result);

   procedure Handle_Process_Exit_Syscall
     (Process : in out Process_Control_Block_T)
   with No_Return;

   procedure Handle_Process_Yield_Syscall
     (Process : in out Process_Control_Block_T; Result : out Function_Result);

   procedure Handle_Allocate_Memory_Syscall
     (Process : in out Process_Control_Block_T; Result : out Function_Result);

   procedure Handle_Print_To_Serial_Syscall
     (Process : in out Process_Control_Block_T; Result : out Function_Result);

   procedure Handle_Open_File_Syscall
     (Process : in out Process_Control_Block_T; Result : out Function_Result);

   procedure Handle_Read_File_Syscall
     (Process : in out Process_Control_Block_T; Result : out Function_Result);

   procedure Handle_Seek_File_Syscall
     (Process : in out Process_Control_Block_T; Result : out Function_Result);

   procedure Handle_Update_Framebuffer_Syscall
     (Process : in out Process_Control_Block_T; Result : out Function_Result);

   function Unsigned_64_To_File_Open_Mode
     (Mode : Unsigned_64) return File_Open_Mode_T;

end System_Calls;
