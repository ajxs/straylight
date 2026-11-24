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

   ----------------------------------------------------------------------------
   --  Reads the next entry of the directory open with the file handle in a1,
   --  into the Directory_Entry_T record in userspace at the address in a2.
   --  Returns 1 if an entry was read, and 0 at the end of the directory.
   --  Returns -EBADF if the file handle isn't open, and -ENOTDIR if it isn't
   --  a directory or mount point.
   --  Returns -ENOTSUP for a directory whose filesystem can't list entries.
   --  Returns -EFAULT for a null record address, or a record outside
   --  userspace.
   ----------------------------------------------------------------------------
   procedure Handle_Read_Directory_Entry_Syscall
     (Process        : in out Process_Control_Block_T;
      Syscall_Result : out Unsigned_64;
      Result         : out Function_Result);

private
   Maximum_Path_String_Argument_Length : constant := 256;

   --  The type of a directory entry, as seen by userspace.
   --  A device mount point is reported as a directory.
   --  The values are ASCII letters, as used by 'find -type', so that they're
   --  readable in a memory dump.
   type Directory_Entry_Type_T is
     (Directory_Entry_Type_Unknown,
      Directory_Entry_Type_Device,
      Directory_Entry_Type_Directory,
      Directory_Entry_Type_Regular_File)
   with Size => 8;
   for Directory_Entry_Type_T use
     (Directory_Entry_Type_Unknown      => Character'Pos ('?'),
      Directory_Entry_Type_Device       => Character'Pos ('c'),
      Directory_Entry_Type_Directory    => Character'Pos ('d'),
      Directory_Entry_Type_Regular_File => Character'Pos ('f'));

   --  The size of the name field in a directory entry record.
   --  If this doesn't match the kernel's node name limit, a compile-time error
   --  will be raised.
   Directory_Entry_Name_Length : constant := 256;

   pragma
     Compile_Time_Error
       (Filesystem_Node_Name_Max_Byte_Length > Directory_Entry_Name_Length,
        "A node name doesn't fit in a directory entry record");

   --  The record that the read directory entry system call fills in.
   --  The name isn't terminated. libc mirrors this record in
   --  straylight_syscall.h.
   type Directory_Entry_T is record
      Entry_Type  : Directory_Entry_Type_T;
      Name_Length : Unsigned_16;
      Name        : String (1 .. Directory_Entry_Name_Length);
   end record
   with Convention => C;

   function Unsigned_64_To_File_Open_Flags is new
     Ada.Unchecked_Conversion
       (Source => Unsigned_64,
        Target => File_Open_Flags_T);

end System_Calls.Files;
