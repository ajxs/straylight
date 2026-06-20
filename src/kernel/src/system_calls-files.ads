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
     (Process : in out Process_Control_Block_T; Result : out Function_Result);

   procedure Handle_Open_File_Syscall
     (Process : in out Process_Control_Block_T; Result : out Function_Result);

   procedure Handle_Read_File_Syscall
     (Process : in out Process_Control_Block_T; Result : out Function_Result);

   procedure Handle_Seek_File_Syscall
     (Process : in out Process_Control_Block_T; Result : out Function_Result);

   procedure Handle_Write_File_Syscall
     (Process : in out Process_Control_Block_T; Result : out Function_Result);

   procedure Handle_Truncate_File_Syscall
     (Process : in out Process_Control_Block_T; Result : out Function_Result);

private
   function Unsigned_64_To_File_Open_Flags is new
     Ada.Unchecked_Conversion
       (Source => Unsigned_64,
        Target => File_Open_Flags_T);

end System_Calls.Files;
