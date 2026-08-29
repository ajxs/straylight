-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Interfaces; use Interfaces;

with Function_Results; use Function_Results;
with Processes;        use Processes;

package System_Calls
  with Preelaborate
is
   Syscall_Exit_Process  : constant := 5446_0000;
   Syscall_Yield_Process : constant := 5446_0001;

   Syscall_Open_File     : constant := 5446_0107;
   Syscall_Read_File     : constant := 5446_0108;
   Syscall_Seek_File     : constant := 5446_0109;
   Syscall_Write_File    : constant := 5446_0110;
   Syscall_Close_File    : constant := 5446_0111;
   Syscall_Truncate_File : constant := 5446_0112;

   Syscall_Grow_Process_Heap : constant := 5446_0300;

   Syscall_Update_Framebuffer : constant := 5446_0209;

   procedure Handle_User_Mode_Syscall
     (Process : in out Process_Control_Block_T; Result : out Function_Result);

private
   procedure Handle_Process_Exit_Syscall
   with No_Return;

   procedure Handle_Process_Yield_Syscall;

   procedure Handle_Update_Framebuffer_Syscall
     (Process        : in out Process_Control_Block_T;
      Syscall_Result : out Unsigned_64;
      Result         : out Function_Result);

   Syscall_Result_Success : constant := 0;

end System_Calls;
