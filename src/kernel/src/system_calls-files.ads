-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Ada.Unchecked_Conversion;

with Filesystems; use Filesystems;

private package System_Calls.Files is
   pragma Preelaborate;

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

   function Unsigned_64_To_File_Open_Mode is new
     Ada.Unchecked_Conversion
       (Source => Unsigned_64,
        Target => File_Open_Mode_T);

end System_Calls.Files;
