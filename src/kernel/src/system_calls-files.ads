-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Ada.Unchecked_Conversion;

with Filesystems; use Filesystems;

private package System_Calls.Files
  with Preelaborate
is
   procedure Handle_Close_File_Syscall
     (Process        : in out Process_Control_Block_T;
      Syscall_Result : out Unsigned_64;
      Result         : out Function_Result);

   procedure Handle_Open_File_Syscall
     (Process        : in out Process_Control_Block_T;
      Syscall_Result : out Unsigned_64;
      Result         : out Function_Result);

   procedure Handle_Read_File_Syscall
     (Process        : in out Process_Control_Block_T;
      Syscall_Result : out Unsigned_64;
      Result         : out Function_Result);

   procedure Handle_Seek_File_Syscall
     (Process        : in out Process_Control_Block_T;
      Syscall_Result : out Unsigned_64;
      Result         : out Function_Result);

   procedure Handle_Write_File_Syscall
     (Process        : in out Process_Control_Block_T;
      Syscall_Result : out Unsigned_64;
      Result         : out Function_Result);

   procedure Handle_Truncate_File_Syscall
     (Process        : in out Process_Control_Block_T;
      Syscall_Result : out Unsigned_64;
      Result         : out Function_Result);

   ----------------------------------------------------------------------------
   --  Copies the process's working directory into the buffer address in a1.
   --  The a2 register holds the length of the buffer to copy into.
   --  No null-terminating character is added.
   --  Returns -ERANGE if the specified length in a2 is too small to hold the
   --  working directory path.
   --  Returns -EFAULT if a null  buffer address is passed, or a buffer range
   --  outside userspace.
   ----------------------------------------------------------------------------
   procedure Handle_Get_Working_Directory_Syscall
     (Process        : in out Process_Control_Block_T;
      Syscall_Result : out Unsigned_64;
      Result         : out Function_Result);

   ----------------------------------------------------------------------------
   --  Changes the process's working directory to the path at the address in
   --  a1. The a2 register holds the path's length.
   --  If a relative path is specified, it's resolved against the process'
   --  current working directory.
   --  Returns -ENOENT if the path doesn't exist.
   --  Returns -ENOTDIR if it isn't a directory or mount point.
   --  Returns -EFAULT if the path's address range is outside userspace.
   ----------------------------------------------------------------------------
   procedure Handle_Change_Working_Directory_Syscall
     (Process        : in out Process_Control_Block_T;
      Syscall_Result : out Unsigned_64;
      Result         : out Function_Result);

private
   Maximum_Path_String_Argument_Length : constant := 256;

   function Unsigned_64_To_File_Open_Flags is new
     Ada.Unchecked_Conversion
       (Source => Unsigned_64,
        Target => File_Open_Flags_T);

end System_Calls.Files;
