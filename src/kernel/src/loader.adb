-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with System;                  use System;
with System.Storage_Elements; use System.Storage_Elements;

with Addresses;       use Addresses;
with Memory;          use Memory;
with Memory.Physical; use Memory.Physical;
with Hart_State;      use Hart_State;

package body Loader is
   procedure Load_New_Process_From_Filesystem
     (Loading_Process : in out Process_Control_Block_T;
      Path            : Filesystem_Path_T;
      Result          : out Function_Result)
   is
      Executable_File : Process_File_Handle_Access := null;

      New_Process : Process_Control_Block_Access := null;

      ELF_Header     : ELF.Elf64_File_Header_T;
      Program_Header : ELF.Elf64_Program_Header_T;

      Region_Flags : Memory_Region_Flags_T := (False, False, False, True);

      Allocated_Physical_Address      : Physical_Address_T :=
        Null_Physical_Address;
      Current_Segment_Virtual_Address : Virtual_Address_T := Null_Address;

      Program_Header_Read_Offset : Unsigned_64 := 0;
      Bytes_To_Read              : Natural := 0;
      Bytes_Read                 : Natural := 0;
   begin
      Log_Debug
        ("Loading new process from filesystem: '" & Path & "'", Logging_Tags);

      Open_File
        (Loading_Process, Path, File_Open_Mode_Read, Executable_File, Result);
      if Is_Error (Result) then
         return;
      end if;

      if Result = File_Not_Found then
         Log_Error ("Executable file not found.", Logging_Tags);
         return;
      end if;

      Create_New_Process (New_Process, Result);
      if Is_Error (Result) then
         return;
      end if;

      --  Read the ELF header.
      Bytes_To_Read := 64;

      Read_File
        (Loading_Process,
         Executable_File,
         ELF_Header'Address,
         Bytes_To_Read,
         Bytes_Read,
         Result);
      if Is_Error (Result) then
         return;
      elsif Bytes_Read /= Bytes_To_Read then
         Log_Error
           ("Invalid amount of data read: " & Bytes_Read'Image, Logging_Tags);
         Result := Unhandled_Exception;
         return;
      end if;

      if not Validate_Executable_Is_Loadable (ELF_Header) then
         Log_Error ("Invalid ELF header", Logging_Tags);
         Result := Unhandled_Exception;
         return;
      end if;

      Print_ELF_Header (ELF_Header);
      Print_ELF_Header_Program_Header_Info (ELF_Header);

      Program_Header_Read_Offset := ELF_Header.e_phoff;

      --  Read each program header and load any loadable segments.
      for I in 1 .. ELF_Header.e_phnum loop
         Seek_File (Executable_File, Program_Header_Read_Offset, Result);
         if Is_Error (Result) then
            return;
         end if;

         Log_Debug ("Reading Program Header...", Logging_Tags);

         Bytes_To_Read := Natural (ELF_Header.e_phentsize);

         Read_File
           (Loading_Process,
            Executable_File,
            Program_Header'Address,
            Bytes_To_Read,
            Bytes_Read,
            Result);
         if Is_Error (Result) then
            return;
         elsif Bytes_Read /= Bytes_To_Read then
            Log_Error
              ("Invalid amount of data read: " & Bytes_Read'Image,
               Logging_Tags);
            Result := Unhandled_Exception;
            return;
         end if;

         Print_ELF64_Program_Header_Info (Program_Header);

         --  If this segment needs to be loaded.
         if Program_Header.p_type = PT_LOAD then
            Log_Debug ("Allocating segment physical memory...", Logging_Tags);

            --  Allocate physical memory for the segment.
            Allocate_Physical_Memory
              (Natural (Program_Header.p_memsz),
               Allocated_Physical_Address,
               Result);
            if Is_Error (Result) then
               return;
            end if;

            Region_Flags :=
              Parse_ELF_Program_Header_Flags_Into_Memory_Region_Flags
                (Program_Header.p_flags);

            Region_Flags.User := True;

            Current_Segment_Virtual_Address :=
              Unsigned_64_To_Address (Program_Header.p_vaddr)
              - Storage_Offset (Program_Header.p_offset);

            Log_Debug
              ("Mapping segment:"
               & ASCII.LF
               & "  Addr: "
               & Current_Segment_Virtual_Address'Image
               & ASCII.LF
               & "  Size: "
               & Program_Header.p_memsz'Image,
               Logging_Tags);

            New_Process.all.Memory_Space.Map
              (Current_Segment_Virtual_Address,
               Allocated_Physical_Address,
               Memory_Region_Size (Program_Header.p_memsz),
               Region_Flags,
               Result);
            --  Error already printed.
            if Is_Error (Result) then
               return;
            end if;

            Load_Segment_Data_Into_Allocated_Address : begin
               --  This is the virtual mapping address for the newly allocated
               --  physical memory.
               Region_Address : constant Virtual_Address_T :=
                 Get_Physical_Address_Virtual_Mapping
                   (Allocated_Physical_Address);

               Log_Debug ("Clearing segment memory...", Logging_Tags);

               --  Clear the allocated memory.
               Set (Region_Address, 0, Natural (Program_Header.p_memsz));

               --  If the segment has data, read this from the disk.
               if Program_Header.p_filesz > 0 then
                  Log_Debug ("Loading segment data...", Logging_Tags);

                  Seek_File (Executable_File, Program_Header.p_offset, Result);
                  if Is_Error (Result) then
                     return;
                  end if;

                  Bytes_To_Read := Natural (Program_Header.p_filesz);

                  --  Read the data directly into the newly allocated memory.
                  Read_File
                    (Loading_Process,
                     Executable_File,
                     Region_Address,
                     Bytes_To_Read,
                     Bytes_Read,
                     Result);
                  if Is_Error (Result) then
                     return;
                  elsif Bytes_Read /= Bytes_To_Read then
                     Log_Error
                       ("Invalid amount of data read: " & Bytes_Read'Image,
                        Logging_Tags);
                     Result := Unhandled_Exception;
                     return;
                  end if;

               end if;
            end Load_Segment_Data_Into_Allocated_Address;
         end if;

         --  Read the next program header.
         Program_Header_Read_Offset :=
           Program_Header_Read_Offset + Unsigned_64 (ELF_Header.e_phentsize);
      end loop;

      --  Set the process' entry point.
      New_Process.all.Process_Entry_Point :=
        Unsigned_64_To_Address (ELF_Header.e_entry);

      Log_Debug
        ("Finished loading new process. Adding to process queue.",
         Logging_Tags);

      Add_Process (New_Process, Result);
      if Is_Error (Result) then
         Panic ("Error adding new process");
      end if;
   exception
      when Constraint_Error =>
         Log_Error
           ("Constraint_Error: Load_New_Process_From_Filesystem.",
            Logging_Tags);
         Result := Constraint_Exception;
   end Load_New_Process_From_Filesystem;

   procedure Print_ELF_Header (ELF_Header : ELF.Elf64_File_Header_T) is
   begin
      --  @TODO.
      Log_Debug
        ("ELF Header: "
         & ASCII.LF
         & "  Magic:          "
         & ELF_Header.e_ident.Magic_Number,
         Logging_Tags);
   end Print_ELF_Header;

   procedure Print_ELF_Header_Program_Header_Info
     (ELF_Header : ELF.Elf64_File_Header_T) is
   begin
      Log_Debug
        ("Program Header Info:"
         & ASCII.LF
         & "  Program Header Count:   "
         & ELF_Header.e_phnum'Image
         & ASCII.LF
         & "  Program Headers Offset: "
         & ELF_Header.e_phoff'Image
         & ASCII.LF
         & "  Program Header Size:    "
         & ELF_Header.e_phentsize'Image,
         Logging_Tags);
   exception
      when Constraint_Error =>
         null;
   end Print_ELF_Header_Program_Header_Info;

   procedure Print_ELF64_Program_Header_Info
     (Program_Header : ELF.Elf64_Program_Header_T)
   is
      Flags_Read    : constant Boolean :=
        (Program_Header.p_flags and PF_R) /= 0;
      Flags_Write   : constant Boolean :=
        (Program_Header.p_flags and PF_W) /= 0;
      Flags_Execute : constant Boolean :=
        (Program_Header.p_flags and PF_X) /= 0;
   begin
      Log_Debug
        ("Program Header:"
         & ASCII.LF
         & "  Type:      "
         & Program_Header.p_type'Image
         & ASCII.LF
         & "  File Size: "
         & Program_Header.p_filesz'Image
         & ASCII.LF
         & "  VAddr:     "
         & Program_Header.p_vaddr'Image
         & ASCII.LF
         & "  MemSz:     "
         & Program_Header.p_memsz'Image
         & ASCII.LF
         & "  Flags:     "
         & ASCII.LF
         & "    Read:     "
         & Flags_Read'Image
         & ASCII.LF
         & "    Write:    "
         & Flags_Write'Image
         & ASCII.LF
         & "    Execute:  "
         & Flags_Execute'Image,
         Logging_Tags);
   exception
      when Constraint_Error =>
         null;
   end Print_ELF64_Program_Header_Info;

   function Validate_Executable_Is_Loadable
     (ELF_Header : Elf64_File_Header_T) return Boolean is
   begin
      if not ELF.Validate_Elf_Header_Magic_Number
               (ELF_Header.e_ident.Magic_Number)
      then
         return False;
      end if;

      if ELF_Header.e_ident.File_Class /= ELFCLASS64 then
         return False;
      end if;

      return True;
   end Validate_Executable_Is_Loadable;

end Loader;
