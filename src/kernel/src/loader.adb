-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with System;                  use System;
with System.Storage_Elements; use System.Storage_Elements;

with Logging;         use Logging;
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
      pragma
        Debug
          (Debug_Loader,
           Log_Debug ("Loading new process from filesystem: '" & Path & "'"));

      Open_File
        (Loading_Process,
         Path,
         (Access_Mode    => Read_Only,
          Creation_Flags => (others => False),
          Status_Flags   => (others => False)),
         Executable_File,
         Result);
      if Is_Error (Result) then
         return;
      end if;

      if Result = File_Not_Found then
         Log_Error ("Executable file not found.");
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
         Log_Error ("Invalid amount of data read: " & Bytes_Read'Image);
         Result := Unhandled_Exception;
         return;
      end if;

      if not Validate_Executable_Is_Loadable (ELF_Header) then
         Log_Error ("Invalid ELF header");
         Result := Unhandled_Exception;
         return;
      end if;

      Program_Header_Read_Offset := ELF_Header.e_phoff;

      --  Read each program header and load any loadable segments.
      for I in 1 .. ELF_Header.e_phnum loop
         Seek_File (Executable_File, Program_Header_Read_Offset, Result);
         if Is_Error (Result) then
            return;
         end if;

         pragma Debug (Debug_Loader, Log_Debug ("Reading Program Header..."));

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
            Log_Error ("Invalid amount of data read: " & Bytes_Read'Image);
            Result := Unhandled_Exception;
            return;
         end if;

         if Program_Header.p_type = PT_LOAD
           and then Program_Header.p_filesz > Program_Header.p_memsz
         then
            Log_Error ("Segment file size exceeds memory size");
            Result := Unhandled_Exception;
            return;
         end if;

         --  If this segment needs to be loaded.
         if Program_Header.p_type = PT_LOAD and then Program_Header.p_memsz > 0
         then
            pragma
              Debug
                (Debug_Loader,
                 Log_Debug ("Allocating segment physical memory..."));

            --  Program_Header.p_vaddr specifies the virtual address at which
            --  the loadable segment should be mapped.
            --  If the segment isn't page-aligned, we need to calculate the
            --  necessary offset to align the page.
            --  This offset will be *subtracted* from p_vaddr to get the actual
            --  address to map the segment to, and *added* to the address that
            --  the executable file data is loaded/copied to so that the file
            --  data lands at the correct address.
            --  e.g. if p_vaddr is 0x1003, we need to map the segment at 0x1000
            --  and load the file data at an offset of 3 bytes into the mapped
            --  memory so that the correct data sits at the correct address.
            Vaddr_Page_Align_Offset : constant Unsigned_64 :=
              Program_Header.p_vaddr mod 16#1000#;

            Total_Mapping_Size : constant Storage_Count :=
              Storage_Count (Program_Header.p_memsz)
              + Storage_Count (Vaddr_Page_Align_Offset);

            --  Allocate physical memory for the segment.
            Allocate_Physical_Memory
              (Total_Mapping_Size, Allocated_Physical_Address, Result);
            if Is_Error (Result) then
               return;
            end if;

            Region_Flags :=
              Parse_ELF_Program_Header_Flags_Into_Memory_Region_Flags
                (Program_Header.p_flags);

            Region_Flags.User := True;

            --  Subtract the page alignment offset from the virtual address so
            --  that the segment mapping is correctly page-aligned.
            Current_Segment_Virtual_Address :=
              Unsigned_64_To_Address
                (Program_Header.p_vaddr - Vaddr_Page_Align_Offset);

            pragma
              Debug
                (Debug_Loader,
                 Log_Debug
                   ("Mapping segment:"
                    & ASCII.LF
                    & "  Addr: "
                    & Current_Segment_Virtual_Address'Image
                    & ASCII.LF
                    & "  Size: "
                    & Program_Header.p_memsz'Image));

            New_Process.all.Memory_Space.Map
              (Current_Segment_Virtual_Address,
               Allocated_Physical_Address,
               Memory_Region_Size (Total_Mapping_Size),
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

               pragma
                 Debug
                   (Debug_Loader, Log_Debug ("Clearing segment memory..."));

               --  Clear the entire allocation, including alignment padding.
               Set (Region_Address, 0, Total_Mapping_Size);

               --  If the segment has data, read this from the disk.
               if Program_Header.p_filesz > 0 then
                  pragma
                    Debug
                      (Debug_Loader, Log_Debug ("Loading segment data..."));

                  Seek_File (Executable_File, Program_Header.p_offset, Result);
                  if Is_Error (Result) then
                     return;
                  end if;

                  Bytes_To_Read := Natural (Program_Header.p_filesz);

                  --  Add the page-alignment offset to the copy destination
                  --  address so that the file data is loaded at the correct
                  --  offset within the mapped memory region.
                  Read_File
                    (Loading_Process,
                     Executable_File,
                     Region_Address + Storage_Offset (Vaddr_Page_Align_Offset),
                     Bytes_To_Read,
                     Bytes_Read,
                     Result);
                  if Is_Error (Result) then
                     return;
                  elsif Bytes_Read /= Bytes_To_Read then
                     Log_Error
                       ("Invalid amount of data read: " & Bytes_Read'Image);
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

      pragma
        Debug
          (Debug_Loader,
           Log_Debug
             ("Finished loading new process. Adding to process queue."));

      Add_Process_To_Process_Queue (New_Process, Result);
      if Is_Error (Result) then
         Panic ("Error adding new process");
      end if;
   exception
      when Constraint_Error =>
         Log_Constraint_Error;
         Result := Constraint_Exception;
   end Load_New_Process_From_Filesystem;

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
