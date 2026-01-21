-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Ada.Unchecked_Conversion;
with Interfaces; use Interfaces;

with Filesystems;      use Filesystems;
with Function_Results; use Function_Results;
with Logging;          use Logging;
with Processes;        use Processes;

package System_Calls is
   pragma Preelaborate;

   Syscall_Exit_Process    : constant := 5446_0000;
   Syscall_Yield_Process   : constant := 5446_0001;
   Syscall_Log_Debug       : constant := 5446_0002;
   Syscall_Log_Error       : constant := 5446_0003;
   Syscall_Allocate_Memory : constant := 5446_0004;
   Syscall_Free_Memory     : constant := 5446_0005;
   Syscall_Print_To_Serial : constant := 5446_0006;

   Syscall_Open_File  : constant := 5446_0107;
   Syscall_Read_File  : constant := 5446_0108;
   Syscall_Seek_File  : constant := 5446_0109;
   Syscall_Close_File : constant := 5446_0110;

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

   procedure Handle_Free_Memory_Syscall
     (Process : in out Process_Control_Block_T; Result : out Function_Result);

   procedure Handle_Print_To_Serial_Syscall
     (Process : in out Process_Control_Block_T; Result : out Function_Result);

   procedure Handle_Open_File_Syscall
     (Process : in out Process_Control_Block_T; Result : out Function_Result);

   procedure Handle_Read_File_Syscall
     (Process : in out Process_Control_Block_T; Result : out Function_Result);

   procedure Handle_Seek_File_Syscall
     (Process : in out Process_Control_Block_T; Result : out Function_Result);

   procedure Handle_Close_File_Syscall
     (Process : in out Process_Control_Block_T; Result : out Function_Result);

   procedure Handle_Update_Framebuffer_Syscall
     (Process : in out Process_Control_Block_T; Result : out Function_Result);

   function Unsigned_64_To_File_Open_Mode
     (Mode : Unsigned_64) return File_Open_Mode_T;

   Syscall_Result_Success : constant := 0;

   type Syscall_Error_Result_T is new Integer with Convention => C, Size => 64;

   Syscall_Error_Invalid_Argument      : constant Syscall_Error_Result_T := -1;
   Syscall_Error_Invalid_Address       : constant Syscall_Error_Result_T := -2;
   Syscall_Error_No_Memory             : constant Syscall_Error_Result_T := -3;
   Syscall_Error_File_Not_Found        : constant Syscall_Error_Result_T := -4;
   Syscall_Error_File_Handle_Not_Found : constant Syscall_Error_Result_T := -5;
   Syscall_Error_Other                 : constant Syscall_Error_Result_T :=
     -99;

   function Syscall_Error_Result_To_Unsigned_64 is new
     Ada.Unchecked_Conversion
       (Source => Syscall_Error_Result_T,
        Target => Unsigned_64);

end System_Calls;
