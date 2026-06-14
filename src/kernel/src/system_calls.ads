-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Ada.Unchecked_Conversion;
with Interfaces; use Interfaces;

with Function_Results; use Function_Results;
with Logging;          use Logging;
with Processes;        use Processes;

package System_Calls
  with Preelaborate
is

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
   Syscall_Write_File : constant := 5446_0110;
   Syscall_Close_File : constant := 5446_0111;

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
   with No_Return;

   procedure Handle_Process_Yield_Syscall;

   procedure Handle_Allocate_Memory_Syscall
     (Process : in out Process_Control_Block_T; Result : out Function_Result);

   procedure Handle_Free_Memory_Syscall
     (Process : in out Process_Control_Block_T; Result : out Function_Result);

   procedure Handle_Print_To_Serial_Syscall
     (Process : in out Process_Control_Block_T; Result : out Function_Result);

   procedure Handle_Update_Framebuffer_Syscall
     (Process : in out Process_Control_Block_T; Result : out Function_Result);

   Syscall_Result_Success : constant := 0;

   type Syscall_Error_Result_T is new Integer with Convention => C, Size => 64;

   --  Operation not permitted.
   EPERM        : constant Syscall_Error_Result_T := 1;
   --  No such file or directory.
   ENOENT       : constant Syscall_Error_Result_T := 2;
   --  No such process.
   ESRCH        : constant Syscall_Error_Result_T := 3;
   --  Interrupted system call.
   EINTR        : constant Syscall_Error_Result_T := 4;
   --  I/O error.
   EIO          : constant Syscall_Error_Result_T := 5;
   --  No such device or address.
   ENXIO        : constant Syscall_Error_Result_T := 6;
   --  Argument list too long.
   E2BIG        : constant Syscall_Error_Result_T := 7;
   --  Exec format error.
   ENOEXEC      : constant Syscall_Error_Result_T := 8;
   --  Bad file number.
   EBADF        : constant Syscall_Error_Result_T := 9;
   --  No child processes.
   ECHILD       : constant Syscall_Error_Result_T := 10;
   --  Try again.
   EAGAIN       : constant Syscall_Error_Result_T := 11;
   --  Out of memory.
   ENOMEM       : constant Syscall_Error_Result_T := 12;
   --  Permission denied.
   EACCES       : constant Syscall_Error_Result_T := 13;
   --  Bad address.
   EFAULT       : constant Syscall_Error_Result_T := 14;
   --  Block device required.
   ENOTBLK      : constant Syscall_Error_Result_T := 15;
   --  Device or resource busy.
   EBUSY        : constant Syscall_Error_Result_T := 16;
   --  File exists.
   EEXIST       : constant Syscall_Error_Result_T := 17;
   --  Cross-device link.
   EXDEV        : constant Syscall_Error_Result_T := 18;
   --  No such device.
   ENODEV       : constant Syscall_Error_Result_T := 19;
   --  Not a directory.
   ENOTDIR      : constant Syscall_Error_Result_T := 20;
   --  Is a directory.
   EISDIR       : constant Syscall_Error_Result_T := 21;
   --  Invalid argument.
   EINVAL       : constant Syscall_Error_Result_T := 22;
   --  File table overflow.
   ENFILE       : constant Syscall_Error_Result_T := 23;
   --  Too many open files.
   EMFILE       : constant Syscall_Error_Result_T := 24;
   --  Not a typewriter.
   ENOTTY       : constant Syscall_Error_Result_T := 25;
   --  Text file busy.
   ETXTBSY      : constant Syscall_Error_Result_T := 26;
   --  File too large.
   EFBIG        : constant Syscall_Error_Result_T := 27;
   --  No space left on device.
   ENOSPC       : constant Syscall_Error_Result_T := 28;
   --  Illegal seek.
   ESPIPE       : constant Syscall_Error_Result_T := 29;
   --  Read-only file system.
   EROFS        : constant Syscall_Error_Result_T := 30;
   --  Too many links.
   EMLINK       : constant Syscall_Error_Result_T := 31;
   --  Broken pipe.
   EPIPE        : constant Syscall_Error_Result_T := 32;
   --  Math argument out of domain of func.
   EDOM         : constant Syscall_Error_Result_T := 33;
   --  Math result not representable.
   ERANGE       : constant Syscall_Error_Result_T := 34;
   --  File name too long.
   ENAMETOOLONG : constant Syscall_Error_Result_T := 35;

   function Syscall_Error_Result_To_Unsigned_64 is new
     Ada.Unchecked_Conversion
       (Source => Syscall_Error_Result_T,
        Target => Unsigned_64);

end System_Calls;
