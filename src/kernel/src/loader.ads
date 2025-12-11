-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Interfaces; use Interfaces;

with ELF;              use ELF;
with Function_Results; use Function_Results;
with Logging;          use Logging;
with Memory.Virtual;   use Memory.Virtual;
with Processes;        use Processes;

package Loader is
   pragma Preelaborate;

   procedure Load_New_Process_From_Filesystem
     (Loading_Process : in out Process_Control_Block_T;
      Path            : Wide_String;
      Result          : out Function_Result);

private
   Logging_Tags : constant Log_Tags := [Log_Tag_Loader];

   function Validate_Executable_Is_Loadable
     (ELF_Header : Elf64_File_Header_T) return Boolean;

   procedure Print_ELF64_Program_Header_Info
     (Program_Header : ELF.Elf64_Program_Header_T);

   procedure Print_ELF_Header_Program_Header_Info
     (ELF_Header : ELF.Elf64_File_Header_T);

   procedure Print_ELF_Header (ELF_Header : ELF.Elf64_File_Header_T);

   function Parse_ELF_Program_Header_Flags_Into_Memory_Region_Flags
     (Flags : Unsigned_32) return Memory_Region_Flags_T
   is ((Flags and PF_R) /= 0,
       (Flags and PF_W) /= 0,
       (Flags and PF_X) /= 0,
       False);

end Loader;
