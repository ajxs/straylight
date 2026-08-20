-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Interfaces; use Interfaces;

with ELF;              use ELF;
with Filesystems;      use Filesystems;
with Function_Results; use Function_Results;
with Memory.Virtual;   use Memory.Virtual;
with Processes;        use Processes;

package Loader
  with Preelaborate
is
   procedure Load_New_Process_From_Filesystem
     (Loading_Process : in out Process_Control_Block_T;
      Path            : Filesystem_Path_T;
      Result          : out Function_Result);

private
   function Validate_Executable_Is_Loadable
     (ELF_Header : Elf64_File_Header_T) return Boolean
   is (ELF.Validate_Elf_Header_Magic_Number (ELF_Header.e_ident.Magic_Number)
       and then ELF_Header.e_ident.File_Class = ELFCLASS64);

   function Parse_ELF_Program_Header_Flags_Into_Memory_Region_Flags
     (Flags : Unsigned_32) return Memory_Region_Flags_T
   is ((Flags and PF_R) /= 0,
       (Flags and PF_W) /= 0,
       (Flags and PF_X) /= 0,
       False);

end Loader;
