-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

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

end System_Calls.Files;
