-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Filesystems.Block_Cache;       use Filesystems.Block_Cache;
with Filesystems.Node_Cache;        use Filesystems.Node_Cache;
with Filesystems.FAT.DOS_Filenames; use Filesystems.FAT.DOS_Filenames;
with Filesystems.FAT.FAT16;         use Filesystems.FAT.FAT16;
with Memory.Kernel;                 use Memory.Kernel;

package body Filesystems.FAT is
   procedure Parse_And_Validate_Boot_Sector
     (Boot_Sector         : aliased Boot_Sector_T;
      FAT_Filesystem_Info : out FAT_Filesystem_Info_T;
      Result              : out Function_Result) is
   begin
      Validate_FAT_Filesystem (Boot_Sector, Result);
      if Is_Error (Result) then
         return;
      end if;

      --  Parse the boot sector to populate the filesystem meta info.
      Parse_Boot_Sector (Boot_Sector, FAT_Filesystem_Info, Result);
      if Is_Error (Result) then
         return;
      end if;

      Result := Success;
   end Parse_And_Validate_Boot_Sector;

   procedure Populate_Filesystem_Meta_Info
     (Filesystem      : Filesystem_Access;
      Reading_Process : in out Process_Control_Block_T;
      Result          : out Function_Result)
   is
      Block_Address : Virtual_Address_T := Null_Address;

      Parse_And_Validate_Result : Function_Result := Unset;
   begin
      Filesystem.all.Filesystem_Meta_Info_Size :=
        FAT_Filesystem_Info_T'Size / 8;

      --  Allocate memory for the filesystem meta info structure, which will
      --  store the necessary information for file operations.
      Allocate_Kernel_Memory
        (Filesystem.all.Filesystem_Meta_Info_Size,
         Filesystem.all.Filesystem_Meta_Info_Address,
         Result);
      if Is_Error (Result) then
         return;
      end if;

      --  Read the boot sector.
      --  We don't know the real sector size yet, but the boot sector is
      --  always located in the first 512 bytes.
      Read_Block_From_Filesystem
        (Filesystem, Reading_Process, 0, Block_Address, Result);
      if Is_Error (Result) then
         return;
      end if;

      FAT_Filesystem_Info : FAT_Filesystem_Info_T
      with
        Import,
        Alignment => 1,
        Address   => Filesystem.all.Filesystem_Meta_Info_Address;

      Boot_Sector : aliased Boot_Sector_T
      with Import, Alignment => 1, Address => Block_Address;

      Parse_And_Validate_Boot_Sector
        (Boot_Sector, FAT_Filesystem_Info, Parse_And_Validate_Result);

      Release_Block (Filesystem, 0, Result);
      if Is_Error (Result) then
         return;
      elsif Is_Error (Parse_And_Validate_Result) then
         Result := Parse_And_Validate_Result;
         return;
      end if;

      Print_FAT_Filesystem_Info (FAT_Filesystem_Info);
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Populate_Filesystem_Meta_Info");
         Result := Constraint_Exception;
   end Populate_Filesystem_Meta_Info;

   procedure Populate_Filesystem_Meta_Info_If_Needed
     (Filesystem      : Filesystem_Access;
      Reading_Process : in out Process_Control_Block_T;
      Result          : out Function_Result) is
   begin
      if Filesystem.all.Filesystem_Meta_Info_Address = Null_Address then
         Populate_Filesystem_Meta_Info (Filesystem, Reading_Process, Result);
      else
         Result := Success;
      end if;
   exception
      when Constraint_Error =>
         Log_Error
           ("Constraint_Error: Populate_Filesystem_Meta_Info_If_Needed",
            Logging_Tags_FAT);
         Result := Constraint_Exception;
   end Populate_Filesystem_Meta_Info_If_Needed;

   procedure Create_File
     (Filesystem      : Filesystem_Access;
      Reading_Process : in out Process_Control_Block_T;
      Filename        : Filesystem_Path_T;
      Parent_Node     : Filesystem_Node_Access;
      New_Node        : out Filesystem_Node_Access;
      Result          : out Function_Result)
   is
      Existing_File : Filesystem_Node_Access := null;
   begin
      --  The parent node should never be null, because the filesystem should
      --  always be mounted, and have a parent in the root filesystem.
      Validate_Filesystem_And_Node
        (Filesystem, Parent_Node, Filesystem_Type_FAT, Result);
      if Is_Error (Result) then
         New_Node := null;
         return;
      end if;

      --  Ensure that the file doesn't already exist.
      --  This call will also populate the filesystem meta info if it hasn't
      --  already been read.
      Find_File
        (Filesystem,
         Reading_Process,
         Filename,
         Parent_Node,
         Existing_File,
         Result);
      if Is_Error (Result) then
         New_Node := null;
         return;
      elsif Existing_File /= null then
         Log_Debug
           ("File already exists: '" & Filename & "'", Logging_Tags_FAT);
         New_Node := null;
         Result := Invalid_Filename;
         return;
      end if;

      Log_Debug ("Creating file: '" & Filename & "'", Logging_Tags_FAT);

      Filesystem_Info : FAT_Filesystem_Info_T
      with
        Import,
        Alignment => 1,
        Address   => Filesystem.all.Filesystem_Meta_Info_Address;

      case Filesystem_Info.FAT_Type is
         when FAT_Type_FAT16 =>
            Create_File_FAT16
              (Filesystem,
               Reading_Process,
               Filesystem_Info,
               Filename,
               Parent_Node,
               New_Node,
               Result);

         when others         =>
            Log_Error ("FAT type not supported", Logging_Tags_FAT);
            New_Node := null;
            Result := Not_Supported;
      end case;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Create_File", Logging_Tags_FAT);
         New_Node := null;
         Result := Constraint_Exception;
   end Create_File;

   procedure Find_File
     (Filesystem      : Filesystem_Access;
      Reading_Process : in out Process_Control_Block_T;
      Filename        : Filesystem_Path_T;
      Parent_Node     : Filesystem_Node_Access;
      Found_Node      : out Filesystem_Node_Access;
      Result          : out Function_Result) is
   begin
      --  The parent node should never be null, because the filesystem should
      --  always be mounted, and have a parent in the root filesystem.
      Validate_Filesystem_And_Node
        (Filesystem, Parent_Node, Filesystem_Type_FAT, Result);
      if Is_Error (Result) then
         Found_Node := null;
         return;
      end if;

      Log_Debug ("Finding file: '" & Filename & "'", Logging_Tags_FAT);

      Populate_Filesystem_Meta_Info_If_Needed
        (Filesystem, Reading_Process, Result);
      if Is_Error (Result) then
         Found_Node := null;
         return;
      end if;

      Filesystem_Info : FAT_Filesystem_Info_T
      with
        Import,
        Alignment => 1,
        Address   => Filesystem.all.Filesystem_Meta_Info_Address;

      case Filesystem_Info.FAT_Type is
         when FAT_Type_FAT16 =>
            Find_File_FAT16
              (Filesystem,
               Reading_Process,
               Filesystem_Info,
               Filename,
               Parent_Node,
               Found_Node,
               Result);

         when others         =>
            Log_Error ("FAT type not supported", Logging_Tags_FAT);
            Found_Node := null;
            Result := Not_Supported;
      end case;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Find_File", Logging_Tags_FAT);
         Found_Node := null;
         Result := Constraint_Exception;
   end Find_File;

   procedure Parse_DOS_Directory_Entry
     (Dir_Entry       : FAT_Directory_Entry_T;
      Filename        : out Wide_String;
      Filename_Length : out Natural;
      Checksum        : out Unsigned_8;
      Result          : out Function_Result) is
   begin
      Filename_Length := 0;

      Checksum :=
        Get_DOS_Filename_Checksum (Dir_Entry.File_Name, Dir_Entry.File_Ext);

      Copy_Name : for I in 1 .. 8 loop
         --  The DOS filename is padded by spaces.
         --  If we encounter a space we can assume we've reached the end
         --  of the file name.
         exit Copy_Name when Dir_Entry.File_Name (I) = ' ';

         Filename (I) := ASCII_To_Wide_Char (Dir_Entry.File_Name (I));

         Filename_Length := Filename_Length + 1;
      end loop Copy_Name;

      --  If this DOS file entry has an extension, append it after a
      --  trailing '.' character at the end of the file name.
      if Dir_Entry.File_Ext (1) /= ' ' then
         Filename_Length := Filename_Length + 1;

         --  Place the '.' between the file name and the file extension.
         Filename (Filename_Length) := '.';

         --  Copy the file extension into the filename.
         Copy_Extension : for I in 1 .. 3 loop
            --  The DOS file extension is padded by spaces.
            --  If we encounter a space we can assume we've reached the end
            --  of the file name.
            exit Copy_Extension when Dir_Entry.File_Ext (I) = ' ';

            Filename_Length := Filename_Length + 1;

            Filename (Filename_Length) :=
              ASCII_To_Wide_Char (Dir_Entry.File_Ext (I));
         end loop Copy_Extension;
      end if;

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error
           ("Constraint_Error: Parse_DOS_Directory_Entry", Logging_Tags_FAT);
         Result := Unhandled_Exception;
   end Parse_DOS_Directory_Entry;

   procedure Print_FAT_Filesystem_Info
     (FAT_Filesystem_Info : FAT_Filesystem_Info_T) is
   begin
      Log_Debug
        ("FAT Filesystem Info:"
         & ASCII.LF
         & "  FAT12_16_Root_Directory_Sector:  "
         & FAT_Filesystem_Info.FAT12_16_Root_Directory_Sector'Image
         & ASCII.LF
         & "  Sectors_In_Root_Directory:       "
         & FAT_Filesystem_Info.Sectors_In_Root_Directory'Image
         & ASCII.LF
         & "  Bytes_Per_Sector:                "
         & FAT_Filesystem_Info.Bytes_Per_Sector'Image
         & ASCII.LF
         & "  First_FAT_Sector:                "
         & FAT_Filesystem_Info.First_FAT_Sector'Image
         & ASCII.LF
         & "  First_Data_Sector:               "
         & FAT_Filesystem_Info.First_Data_Sector'Image
         & ASCII.LF
         & "  Total_Sectors_In_All_FAT_Tables: "
         & FAT_Filesystem_Info.Total_Sectors_In_All_FAT_Tables'Image
         & ASCII.LF
         & "  FAT_Table_Count:                 "
         & FAT_Filesystem_Info.FAT_Table_Count'Image
         & ASCII.LF
         & "  Sectors_Per_Cluster:             "
         & FAT_Filesystem_Info.Sectors_Per_Cluster'Image,
         Logging_Tags_FAT);
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Print_FAT_Filesystem_Info");
   end Print_FAT_Filesystem_Info;

   procedure Parse_Boot_Sector
     (Boot_Sector     : Boot_Sector_T;
      Filesystem_Info : out FAT_Filesystem_Info_T;
      Result          : out Function_Result)
   is
      --  The extended BIOS parameter block.
      --  Used if the filesystem is FAT32.
      FAT32_EBPB : EBPB_FAT32_T
      with
        Import,
        Alignment => 1,
        Address   => Boot_Sector.EBPB_Reserved_Space'Address;
   begin
      Log_Debug ("Parsing boot sector...", Logging_Tags_FAT);

      --  On FAT32 systems, the 'Total_Sector_Count' field (BPB_TotSec16) is
      --  set to 0, and the actual total sector count is found in the
      --  'Large_Sector_Count' field (BPB_TotSec32).
      Filesystem_Info.Total_Sector_Count :=
        (if Boot_Sector.BPB.Total_Sector_Count = 0
         then Natural (Boot_Sector.BPB.Large_Sector_Count)
         else Natural (Boot_Sector.BPB.Total_Sector_Count));

      --  On FAT32 systems, the 'Table_Size' field (BPB_FATSz16) is set to 0,
      --  and the actual FAT table size is found in the 'BPB_FATSz32' field.
      Filesystem_Info.Sectors_In_FAT_Table :=
        Positive
          (if Boot_Sector.BPB.Table_Size > 0
           then Boot_Sector.BPB.Table_Size
           else FAT32_EBPB.Table_Size);

      Filesystem_Info.FAT_Table_Count :=
        Positive (Boot_Sector.BPB.Table_Count);

      Filesystem_Info.Total_Sectors_In_All_FAT_Tables :=
        Filesystem_Info.FAT_Table_Count * Filesystem_Info.Sectors_In_FAT_Table;

      Root_Entry_Size : constant Unsigned_32 :=
        Unsigned_32 (Boot_Sector.BPB.Root_Entry_Count) * 32;

      Total_Size : constant Unsigned_32 :=
        Root_Entry_Size + Unsigned_32 (Boot_Sector.BPB.Bytes_Per_Sector) - 1;

      Filesystem_Info.Sectors_In_Root_Directory :=
        Natural (Total_Size / Unsigned_32 (Boot_Sector.BPB.Bytes_Per_Sector));

      --  Calculate the total number of 'overhead' sectors that are taken up
      --  by the reserved sectors, FAT tables, and root directory, which are
      --  not part of the data region.
      Overhead_Sectors : constant Natural :=
        Filesystem_Info.Total_Sectors_In_All_FAT_Tables
        + Natural (Boot_Sector.BPB.Reserved_Sector_Count)
        + Filesystem_Info.Sectors_In_Root_Directory;

      --  Guard against the possibility of an underflow when calculating the
      --  number of data sectors caused by a corrupt BPB.
      if Filesystem_Info.Total_Sector_Count < Overhead_Sectors then
         Log_Error
           ("Invalid BPB: Total sectors less than reserved + FAT sectors.",
            Logging_Tags_FAT);

         Result := Invalid_Filesystem;
         return;
      end if;

      Data_Sectors : constant Natural :=
        Filesystem_Info.Total_Sector_Count - Overhead_Sectors;

      Filesystem_Info.Sectors_Per_Cluster :=
        Natural (Boot_Sector.BPB.Sectors_Per_Cluster);

      Filesystem_Info.Total_Clusters :=
        (Data_Sectors / Filesystem_Info.Sectors_Per_Cluster);

      Filesystem_Info.FAT_Type :=
        Get_Filesystem_Type (Filesystem_Info.Total_Clusters);

      Filesystem_Info.Bytes_Per_Sector :=
        Natural (Boot_Sector.BPB.Bytes_Per_Sector);

      Filesystem_Info.First_FAT_Sector :=
        Sector_Index_T (Boot_Sector.BPB.Reserved_Sector_Count);

      Filesystem_Info.First_Data_Sector :=
        Filesystem_Info.First_FAT_Sector
        + Sector_Index_T
            (Filesystem_Info.Sectors_In_Root_Directory
             + Filesystem_Info.Total_Sectors_In_All_FAT_Tables);

      Filesystem_Info.FAT_Table_Count := Natural (Boot_Sector.BPB.Table_Count);

      Filesystem_Info.FAT12_16_Root_Directory_Sector :=
        Filesystem_Info.First_Data_Sector
        - Sector_Index_T (Filesystem_Info.Sectors_In_Root_Directory);

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Parse_Boot_Sector");
         Result := Constraint_Exception;
   end Parse_Boot_Sector;

   procedure Read_FAT_Entry
     (Filesystem      : Filesystem_Access;
      Reading_Process : in out Process_Control_Block_T;
      Filesystem_Info : FAT_Filesystem_Info_T;
      Cluster         : Unsigned_32;
      FAT_Entry       : out Unsigned_32;
      Result          : out Function_Result)
   is
      FAT16_Table_Entry : Unsigned_16 := 0;
   begin
      case Filesystem_Info.FAT_Type is
         when FAT_Type_FAT16 =>
            Read_FAT16_Entry
              (Filesystem,
               Reading_Process,
               Filesystem_Info,
               Unsigned_16 (Cluster),
               FAT16_Table_Entry,
               Result);

            FAT_Entry := Unsigned_32 (FAT16_Table_Entry);

         when others         =>
            Log_Error ("FAT type not supported", Logging_Tags_FAT);
            FAT_Entry := 0;
            Result := Not_Supported;
      end case;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Read_FAT_Entry");
         Result := Constraint_Exception;
   end Read_FAT_Entry;

   procedure Write_FAT_Entry
     (Filesystem      : Filesystem_Access;
      Writing_Process : in out Process_Control_Block_T;
      Filesystem_Info : FAT_Filesystem_Info_T;
      Cluster         : Unsigned_32;
      FAT_Entry       : Unsigned_32;
      Result          : out Function_Result) is
   begin
      case Filesystem_Info.FAT_Type is
         when FAT_Type_FAT16 =>
            Write_FAT16_Entry
              (Filesystem,
               Writing_Process,
               Filesystem_Info,
               Unsigned_16 (Cluster),
               Unsigned_16 (FAT_Entry),
               Result);

         when others         =>
            Log_Error ("FAT type not supported", Logging_Tags_FAT);
            Result := Not_Supported;
      end case;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Write_FAT_Entry");
         Result := Constraint_Exception;
   end Write_FAT_Entry;

   procedure Parse_LFN_Directory_Entry
     (Dir_Entry       : FAT_Directory_Entry_T;
      Filename        : in out Wide_String;
      Filename_Length : in out Natural;
      Checksum        : out Unsigned_8;
      Is_Last_Entry   : out Boolean;
      Result          : out Function_Result)
   is
      --  The long file name entry being parsed.
      LFN_Entry : Long_File_Name_Directory_Entry
      with Import, Alignment => 1, Address => Dir_Entry'Address;

      --  The current offset into reading the name.
      Name_offset : Natural := 1;
   begin
      if not Is_LFN_Directory_Entry (Dir_Entry) then
         Checksum := 0;
         Is_Last_Entry := False;
         Result := Invalid_Argument;
         return;
      end if;

      if LFN_Entry.Sequence.Number = 0 then
         Checksum := 0;
         Is_Last_Entry := False;
         Result := Invalid_Argument;
         return;
      end if;

      Is_Last_Entry := LFN_Entry.Sequence.Last_Entry;
      if Is_Last_Entry then
         --  If this is the last entry, then we are starting to read the name
         --  from the beginning, so reset the filename length to 0.
         --  This helps deal with 'orphaned' LFN entries by ignoring any
         --  previous LFN entries that may have been read before encountering
         --  the new 'last entry'.
         Filename_Length := 0;
      end if;

      Checksum := LFN_Entry.Checksum;

      --  The offset into the name is always the sequence number
      --  multiplied by the maximum string length that each
      --  entry holds, which is 13.
      Name_offset := (Natural (LFN_Entry.Sequence.Number) - 1) * 13;

      --  Since each LFN entry contains 13 UCS-2 characters, ensure that
      --  adding another 13 characters to the name won't exceed the maximum
      --  length of a file name that we can support.
      if Name_offset + 13 > Filename'Length then
         Log_Error
           ("LFN entry filename exceeds maximum supported length.",
            Logging_Tags_FAT);
         Result := Invalid_Filename;
         return;
      end if;

      --  Copy each of the string sections contained in this
      --  file name entry into the current directory entry.
      for I in 1 .. 5 loop
         --  If a NULL word is encountered, this signifies the end of the
         --  name. Stop the copying process and exit the function.
         if LFN_Entry.Name_1 (I) = Wide_Character'Val (0) then
            Result := Success;
            return;
         end if;

         Filename_Length := Filename_Length + 1;
         Filename (Name_offset + I) := LFN_Entry.Name_1 (I);
      end loop;

      for I in 1 .. 6 loop
         if LFN_Entry.Name_2 (I) = Wide_Character'Val (0) then
            Result := Success;
            return;
         end if;

         Filename_Length := Filename_Length + 1;
         Filename (Name_offset + 5 + I) := LFN_Entry.Name_2 (I);
      end loop;

      for I in 1 .. 2 loop
         if LFN_Entry.Name_3 (I) = Wide_Character'Val (0) then
            Result := Success;
            return;
         end if;

         Filename_Length := Filename_Length + 1;
         Filename (Name_offset + 11 + I) := LFN_Entry.Name_3 (I);
      end loop;

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint error reading LFN", Logging_Tags_FAT);
         Result := Constraint_Exception;
   end Parse_LFN_Directory_Entry;

   procedure Read_File
     (Filesystem      : Filesystem_Access;
      Reading_Process : in out Process_Control_Block_T;
      Filesystem_Node : Filesystem_Node_Access;
      Buffer_Address  : Virtual_Address_T;
      Start_Offset    : Unsigned_64;
      Bytes_To_Read   : Natural;
      Bytes_Read      : out Natural;
      Result          : out Function_Result) is
   begin
      Bytes_Read := 0;

      Validate_Filesystem_And_Node
        (Filesystem, Filesystem_Node, Filesystem_Type_FAT, Result);
      if Is_Error (Result) then
         return;
      end if;

      Populate_Filesystem_Meta_Info_If_Needed
        (Filesystem, Reading_Process, Result);
      if Is_Error (Result) then
         return;
      end if;

      Filesystem_Info : FAT_Filesystem_Info_T
      with
        Import,
        Alignment => 1,
        Address   => Filesystem.all.Filesystem_Meta_Info_Address;

      Read_File_Data
        (Filesystem,
         Filesystem_Info,
         Reading_Process,
         Filesystem_Node,
         Buffer_Address,
         Start_Offset,
         Bytes_To_Read,
         Bytes_Read,
         Result);
      if Is_Error (Result) then
         return;
      end if;

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Filesystems.FAT.Read_File");
         Result := Constraint_Exception;
   end Read_File;

   procedure Validate_FAT_Filesystem
     (Boot_Sector : Boot_Sector_T; Result : out Function_Result) is
   begin
      if not (Boot_Sector.BPB.Bytes_Per_Sector in 512 | 1024 | 2048 | 4096)
      then
         Log_Error
           ("Filesystem Invalid: Invalid sector size.", Logging_Tags_FAT);
         Result := Invalid_Filesystem;
         return;
      end if;

      if Boot_Sector.BPB.Sectors_Per_Cluster = 0 then
         Log_Error
           ("Filesystem Invalid: Sectors per cluster cannot be zero.",
            Logging_Tags_FAT);
         Result := Invalid_Filesystem;
         return;
      end if;

      Result := Success;
   end Validate_FAT_Filesystem;

   procedure Read_FAT_Filename_Into_Filesystem_Node_Name
     (FAT_Filename         : Wide_String;
      FAT_Filename_Length  : Natural;
      Filesystem_Node_Name : out Filesystem_Node_Name_T;
      Result               : out Function_Result)
   is
      Current_Node_Name_Index : Natural := 1;

      --  Each UCS-2 character requires up to 3 bytes to encode in UTF-8.
      --  This buffer holds those 1-3 bytes for each character conversion.
      Converted_UCS2_Char_Buffer      : UTF8_Converted_Char_Buffer_T;
      --  The length of the converted UTF-8 character in bytes.
      Converted_UCS2_Char_Byte_Length : Integer := 0;
   begin
      Filesystem_Node_Name := Null_Filesystem_Node_Name;

      for I in 1 .. FAT_Filename_Length loop
         Encode_UCS2_Wide_Char_As_UTF8_Buffer
           (FAT_Filename (I),
            Converted_UCS2_Char_Buffer,
            Converted_UCS2_Char_Byte_Length);

         for J in 1 .. Converted_UCS2_Char_Byte_Length loop
            Filesystem_Node_Name.Byte_Length :=
              Filesystem_Node_Name.Byte_Length + 1;

            Filesystem_Node_Name.Value (Current_Node_Name_Index) :=
              Converted_UCS2_Char_Buffer (J);

            Current_Node_Name_Index := Current_Node_Name_Index + 1;
         end loop;
      end loop;

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error
           ("Constraint_Error: Read_FAT_Filename_Into_Filesystem_Node_Name");

         Result := Constraint_Exception;
   end Read_FAT_Filename_Into_Filesystem_Node_Name;

   procedure Does_FAT_Directory_Entry_Name_Match_Filename
     (Directory_Entry_Name        : Wide_String;
      Directory_Entry_Name_Length : Natural;
      Path_Name                   : Filesystem_Path_T;
      Match_Found                 : out Boolean;
      Result                      : out Function_Result)
   is
      --  The UCS-2-encoded filename converted to the OS' native UTF-8 format.
      --  This is used to compare against the target filename.
      UTF8_Encoded_Filename : Filesystem_Node_Name_T;
   begin
      --  Convert the cached long filename to its UTF-8 representation
      --  so we can compare it against the target filename.
      Read_FAT_Filename_Into_Filesystem_Node_Name
        (Directory_Entry_Name,
         Directory_Entry_Name_Length,
         UTF8_Encoded_Filename,
         Result);
      if Is_Error (Result) then
         Match_Found := False;
         return;
      end if;

      Log_Debug
        ("Parsed FAT file entry with filename: '"
         & UTF8_Encoded_Filename.Value (1 .. UTF8_Encoded_Filename.Byte_Length)
         & "'",
         Logging_Tags_FAT);

      --  Note that all operations on FAT filesystems are case-insensitive.
      --  As perFAT32 v1.03 spec page 30.
      Match_Found :=
        Does_Node_Name_Match_Path_Name
          (UTF8_Encoded_Filename, Path_Name, True);

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error
           ("Constraint_Error: Does_FAT_Directory_Entry_Name_Match_Filename");
         Match_Found := False;
         Result := Constraint_Exception;
   end Does_FAT_Directory_Entry_Name_Match_Filename;

   procedure Write_File
     (Filesystem      : Filesystem_Access;
      Writing_Process : in out Process_Control_Block_T;
      Filesystem_Node : Filesystem_Node_Access;
      Buffer_Address  : Virtual_Address_T;
      Start_Offset    : Unsigned_64;
      Bytes_To_Write  : Natural;
      Bytes_Written   : out Natural;
      Result          : out Function_Result)
   is
      Directory_Entry : FAT_Directory_Entry_T;

      Start_Cluster : Unsigned_32 := 0;
   begin
      Bytes_Written := 0;

      if Start_Offset + Unsigned_64 (Bytes_To_Write)
        > Unsigned_64 (Unsigned_32'Last)
      then
         Log_Error
           ("File size after write would exceed maximum supported size.",
            Logging_Tags_FAT);

         Result := Not_Supported;
         return;
      end if;

      Validate_Filesystem_And_Node
        (Filesystem, Filesystem_Node, Filesystem_Type_FAT, Result);
      if Is_Error (Result) then
         return;
      end if;

      Populate_Filesystem_Meta_Info_If_Needed
        (Filesystem, Writing_Process, Result);
      if Is_Error (Result) then
         return;
      end if;

      Filesystem_Info : FAT_Filesystem_Info_T
      with
        Import,
        Alignment => 1,
        Address   => Filesystem.all.Filesystem_Meta_Info_Address;

      --  Read the file's FAT directory entry, which we'll need to update with
      --  the new file size and starting cluster (if we need to allocate a new
      --  cluster for an empty file).
      Get_Filesystem_Node_Directory_Entry
        (Filesystem,
         Writing_Process,
         Filesystem_Info,
         Filesystem_Node,
         Directory_Entry,
         Result);
      if Is_Error (Result) then
         Bytes_Written := 0;
         return;
      end if;

      Start_Cluster := Unsigned_32 (Filesystem_Node.all.Data_Location);

      --  If the file doesn't have any clusters allocated yet, then we need to
      --  allocate the first cluster before we can write any data.
      if Is_Cluster_Free (Start_Cluster) then
         Allocate_Cluster
           (Filesystem,
            Writing_Process,
            Filesystem_Info,
            Start_Cluster,
            Result);
         if Is_Error (Result) then
            Bytes_Written := 0;
            return;
         end if;

         Log_Debug
           ("Allocated first cluster for file: " & Start_Cluster'Image,
            Logging_Tags_FAT);

         --  Update the file directory entry with the allocated cluster.
         Directory_Entry.First_Cluster_Low :=
           Unsigned_16 (Start_Cluster and 16#FFFF#);

         Directory_Entry.First_Cluster_High :=
           Unsigned_16 (Shift_Right (Start_Cluster, 16) and 16#FFFF#);

         Filesystem_Node.all.Data_Location := Unsigned_64 (Start_Cluster);
      end if;

      Write_File_Data
        (Filesystem,
         Filesystem_Info,
         Writing_Process,
         Filesystem_Node,
         Buffer_Address,
         Start_Offset,
         Bytes_To_Write,
         Bytes_Written,
         Result);
      if Is_Error (Result) then
         Bytes_Written := 0;
         return;
      end if;

      New_File_Size : constant Unsigned_32 :=
        Unsigned_32'Max
          (Unsigned_32 (Filesystem_Node.all.Size),
           Unsigned_32 (Start_Offset + Unsigned_64 (Bytes_Written)));

      Directory_Entry.File_Size := New_File_Size;
      Filesystem_Node.all.Size := Unsigned_64 (New_File_Size);
      --  @TODO: Update file modification timestamp in directory entry.

      Write_Filesystem_Node_Directory_Entry
        (Filesystem,
         Writing_Process,
         Filesystem_Info,
         Filesystem_Node,
         Directory_Entry,
         Result);
      if Is_Error (Result) then
         Bytes_Written := 0;
         return;
      end if;

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Write_File");
         Bytes_Written := 0;
         Result := Constraint_Exception;
   end Write_File;

   procedure Search_FAT_Directory_For_File
     (Filesystem                   : Filesystem_Access;
      Filesystem_Info              : FAT_Filesystem_Info_T;
      Directory                    : Directory_Index_T;
      Filename                     : Filesystem_Path_T;
      Parent_Node                  : Filesystem_Node_Access;
      Directory_Starting_Sector    : Sector_Index_T;
      Entry_Long_Filename          : in out Wide_String;
      Entry_Long_Filename_Length   : in out Natural;
      Entry_Long_Filename_Checksum : in out Unsigned_8;
      Checksum_Valid               : in out Boolean;
      Filesystem_Node              : out Filesystem_Node_Access;
      Last_Entry_Reached           : out Boolean;
      Result                       : out Function_Result)
   is
      DOS_Filename        : Wide_String (1 .. 12) :=
        [others => Wide_Character'Val (0)];
      DOS_Filename_Length : Natural := 0;

      Match_Found : Boolean := False;

      Is_Last_Entry : Boolean := False;
      New_Checksum  : Unsigned_8 := 0;
   begin
      Log_Debug
        ("Searching directory for file '" & Filename & "'", Logging_Tags_FAT);

      Filesystem_Node := null;

      --  Track when we've reached the last directory entry, which is
      --  indicated by a special marker in the first byte of the directory
      --  entry. This is done so that a caller can know when to stop scanning
      --  directory entry clusters.
      Last_Entry_Reached := False;

      for Dir_Idx in Directory'Range loop
         Log_Debug
           ("Scanning directory entry: " & Dir_Idx'Image, Logging_Tags_FAT);

         if Is_Last_Directory_Entry (Directory (Dir_Idx)) then
            Log_Debug ("Reached last directory entry.", Logging_Tags_FAT);
            Last_Entry_Reached := True;
            exit;
         end if;

         if not Is_Unused_Directory_Entry (Directory (Dir_Idx)) then
            --  If the entry attributes indicate that this is a long
            --  file name entry, read its section of the name into the cached
            --  filename buffer.
            if Is_LFN_Directory_Entry (Directory (Dir_Idx)) then
               Parse_LFN_Directory_Entry
                 (Directory (Dir_Idx),
                  Entry_Long_Filename,
                  Entry_Long_Filename_Length,
                  New_Checksum,
                  Is_Last_Entry,
                  Result);
               if Is_Error (Result) then
                  return;
               end if;

               if Is_Last_Entry then
                  --  If this is the last entry, then we are starting to read a
                  --  new long filename sequence. So reset the checksum state.
                  Entry_Long_Filename_Checksum := New_Checksum;
                  Checksum_Valid := True;
               else
                  --  Accumulate the checksum valid state for this sequence.
                  Checksum_Valid :=
                    Checksum_Valid
                    and then (New_Checksum = Entry_Long_Filename_Checksum);

                  if New_Checksum /= Entry_Long_Filename_Checksum then
                     Log_Debug
                       ("Directory entry "
                        & Dir_Idx'Image
                        & " checksum does not match expected value.",
                        Logging_Tags_FAT);
                  end if;
               end if;
            else
               Parse_DOS_Directory_Entry
                 (Directory (Dir_Idx),
                  DOS_Filename,
                  DOS_Filename_Length,
                  New_Checksum,
                  Result);
               if Is_Error (Result) then
                  return;
               end if;

               --  If we've read a long filename for this directory entry, then
               --  validate the long filename checksum against the DOS filename
               --  checksum.
               if Entry_Long_Filename_Length > 0 then
                  Checksum_Valid :=
                    Checksum_Valid
                    and then (New_Checksum = Entry_Long_Filename_Checksum);

                  if not Checksum_Valid then
                     Log_Debug
                       ("Filename checksum is invalid.", Logging_Tags_FAT);
                  end if;
               end if;

               --  Only compare against the long filename if we have a valid
               --  checksum, and we've actually read a long filename.
               Use_Long_Filename : constant Boolean :=
                 Checksum_Valid and then Entry_Long_Filename_Length > 0;

               --  Every sequence of LFN entries is followed by its
               --  corresponding DOS 8.3 entry, so at this point we have
               --  either the full filename of the long directory entry, or the
               --  full DOS directory entry, and can now convert it to UTF-8,
               --  and compare it against the target filename.
               if Use_Long_Filename then
                  Does_FAT_Directory_Entry_Name_Match_Filename
                    (Entry_Long_Filename,
                     Entry_Long_Filename_Length,
                     Filename,
                     Match_Found,
                     Result);
                  if Is_Error (Result) then
                     return;
                  end if;
               end if;

               --  As per the FAT32 v1.03 spec:
               --  " As soon as a short directory entry is encountered that is
               --  associated with the cached long name, the long name search
               --  operation will check the cached long name first and then the
               --  short name for a match."
               if not Match_Found then
                  Does_FAT_Directory_Entry_Name_Match_Filename
                    (DOS_Filename,
                     DOS_Filename_Length,
                     Filename,
                     Match_Found,
                     Result);
                  if Is_Error (Result) then
                     return;
                  end if;
               end if;

               if Match_Found then
                  --  Calculate the sector index of the matching directory
                  --  entry, and its index within that sector. These are used
                  --  to create the filesystem node index for this entry.
                  Entries_Per_Sector : constant Natural :=
                    Filesystem_Info.Bytes_Per_Sector / 32;

                  Current_Sector : constant Sector_Index_T :=
                    Directory_Starting_Sector
                    + Sector_Index_T (Dir_Idx / Entries_Per_Sector);

                  Index_Within_Sector : constant Natural :=
                    (Dir_Idx mod Entries_Per_Sector);

                  Log_Debug
                    ("Found matching directory entry.", Logging_Tags_FAT);

                  First_Cluster : constant Unsigned_32 :=
                    Get_First_Cluster_Of_Dir_Entry (Directory (Dir_Idx));

                  --  If we have a match, create a filesystem node cache entry,
                  --  set the node type, and exit.
                  Create_Filesystem_Node_Cache_Entry
                    (Filesystem,
                     Filename,
                     Filesystem_Node,
                     Result,
                     Size          =>
                       Unsigned_64 (Directory (Dir_Idx).File_Size),
                     Data_Location => Unsigned_64 (First_Cluster),
                     Index         =>
                       Get_Directory_Entry_Node_Index
                         (Unsigned_32 (Current_Sector), Index_Within_Sector),
                     Parent_Index  => Parent_Node.all.Index);
                  if Is_Error (Result) then
                     Filesystem_Node := null;
                     return;
                  end if;

                  Filesystem_Node.all.Node_Type :=
                    Get_Node_Type_From_Directory_Entry (Directory (Dir_Idx));

                  Result := Success;
                  return;
               end if;

               --  If we didn't find a match, reset the cached long filename.
               Entry_Long_Filename := [others => Wide_Character'Val (0)];
               Entry_Long_Filename_Length := 0;
               Checksum_Valid := False;
            end if;
         end if;
      end loop;

      Result := File_Not_Found;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Search_FAT_Directory_For_File");
         Result := Constraint_Exception;
   end Search_FAT_Directory_For_File;

   procedure Get_Directory_Entry_Sector_And_Index
     (Filesystem_Node     : Filesystem_Node_Access;
      Sector_Number       : out Sector_Index_T;
      Index_Within_Sector : out Natural;
      Result              : out Function_Result) is
   begin
      if Filesystem_Node = null then
         Log_Error ("Filesystem node is null.", Logging_Tags_FAT);
         Sector_Number := 0;
         Index_Within_Sector := 0;
         Result := Invalid_Argument;
         return;
      end if;

      Sector_Number :=
        Sector_Index_T (Shift_Right (Filesystem_Node.all.Index, 32));

      Index_Within_Sector :=
        Natural (Filesystem_Node.all.Index and 16#FFFF_FFFF#);

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Get_Directory_Entry_Sector_And_Index");
         Sector_Number := 0;
         Index_Within_Sector := 0;
         Result := Constraint_Exception;
   end Get_Directory_Entry_Sector_And_Index;

   procedure Get_Filesystem_Node_Directory_Entry
     (Filesystem      : Filesystem_Access;
      Calling_Process : in out Process_Control_Block_T;
      Filesystem_Info : FAT_Filesystem_Info_T;
      Filesystem_Node : Filesystem_Node_Access;
      Directory_Entry : out FAT_Directory_Entry_T;
      Result          : out Function_Result)
   is
      Block_Address              : Virtual_Address_T := Null_Address;
      Block_Number               : Block_Index_T := 0;
      Sector_Offset_Within_Block : Storage_Offset := 0;

      Directory_Entry_Sector              : Sector_Index_T := 0;
      Directory_Entry_Index_Within_Sector : Natural := 0;
   begin
      Directory_Entry :=
        (File_Name        => "        ",
         File_Ext         => "   ",
         Attributes       => (others => False),
         Reserved         => 0,
         Creation_Seconds => 0,
         File_Size        => 0,
         others           => 0);

      --  This all also validates the filesystem node entry is non-null.
      Get_Directory_Entry_Sector_And_Index
        (Filesystem_Node,
         Directory_Entry_Sector,
         Directory_Entry_Index_Within_Sector,
         Result);
      if Is_Error (Result) then
         return;
      end if;

      Get_Sector_Block_Number_And_Offset
        (Directory_Entry_Sector,
         Filesystem_Info.Bytes_Per_Sector,
         Block_Number,
         Sector_Offset_Within_Block,
         Result);
      if Is_Error (Result) then
         return;
      end if;

      Read_Block_From_Filesystem
        (Filesystem, Calling_Process, Block_Number, Block_Address, Result);
      if Is_Error (Result) then
         return;
      end if;

      Copy
        (Directory_Entry'Address,
         Block_Address
         + Sector_Offset_Within_Block
         + Storage_Offset (Directory_Entry_Index_Within_Sector * 32),
         32);

      Release_Block (Filesystem, Block_Number, Result);

      pragma Warnings (Off, "No_Exception_Propagation");
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Get_Filesystem_Node_Directory_Entry");
         Result := Constraint_Exception;
   end Get_Filesystem_Node_Directory_Entry;
   pragma Warnings (On, "No_Exception_Propagation");

   procedure Write_Filesystem_Node_Directory_Entry
     (Filesystem              : Filesystem_Access;
      Calling_Process         : in out Process_Control_Block_T;
      Filesystem_Info         : FAT_Filesystem_Info_T;
      Filesystem_Node         : Filesystem_Node_Access;
      Updated_Directory_Entry : FAT_Directory_Entry_T;
      Result                  : out Function_Result)
   is
      Block_Address              : Virtual_Address_T := Null_Address;
      Block_Number               : Block_Index_T := 0;
      Sector_Offset_Within_Block : Storage_Offset := 0;

      Directory_Entry_Sector              : Sector_Index_T := 0;
      Directory_Entry_Index_Within_Sector : Natural := 0;
   begin
      --  This all also validates the filesystem node entry is non-null.
      Get_Directory_Entry_Sector_And_Index
        (Filesystem_Node,
         Directory_Entry_Sector,
         Directory_Entry_Index_Within_Sector,
         Result);
      if Is_Error (Result) then
         return;
      end if;

      Get_Sector_Block_Number_And_Offset
        (Directory_Entry_Sector,
         Filesystem_Info.Bytes_Per_Sector,
         Block_Number,
         Sector_Offset_Within_Block,
         Result);
      if Is_Error (Result) then
         return;
      end if;

      Read_Block_From_Filesystem
        (Filesystem, Calling_Process, Block_Number, Block_Address, Result);
      if Is_Error (Result) then
         return;
      end if;

      Copy
        (Block_Address
         + Sector_Offset_Within_Block
         + Storage_Offset (Directory_Entry_Index_Within_Sector * 32),
         Updated_Directory_Entry'Address,
         32);

      Write_Block_To_Filesystem
        (Filesystem, Calling_Process, Block_Number, Result);
      if Is_Error (Result) then
         return;
      end if;

      Release_Block (Filesystem, Block_Number, Result);

      pragma Warnings (Off, "No_Exception_Propagation");
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Write_Filesystem_Node_Directory_Entry");
         Result := Constraint_Exception;
   end Write_Filesystem_Node_Directory_Entry;
   pragma Warnings (On, "No_Exception_Propagation");

   procedure Read_File_Data
     (Filesystem      : Filesystem_Access;
      Filesystem_Info : FAT_Filesystem_Info_T;
      Reading_Process : in out Process_Control_Block_T;
      Filesystem_Node : Filesystem_Node_Access;
      Buffer_Address  : Virtual_Address_T;
      Start_Offset    : Unsigned_64;
      Bytes_To_Read   : Natural;
      Bytes_Read      : out Natural;
      Result          : out Function_Result)
   is
      Current_Cluster       : Unsigned_32 := 0;
      Next_Cluster_In_Chain : Unsigned_32 := 0;
      Clusters_Scanned      : Natural := 0;

      Current_Sector             : Sector_Index_T := 0;
      Block_Address              : Virtual_Address_T := Null_Address;
      Block_Number               : Block_Index_T := 0;
      Sector_Offset_Within_Block : Storage_Offset := 0;

      Current_Offset       : Unsigned_64 := Start_Offset;
      Actual_Bytes_To_Read : Natural := Bytes_To_Read;
   begin
      Bytes_Read := 0;

      Validate_Read_Start_Offset_And_Get_Actual_Bytes_To_Read
        (Filesystem_Node,
         Start_Offset,
         Bytes_To_Read,
         Actual_Bytes_To_Read,
         Result);
      if Is_Error (Result) then
         return;
      end if;

      if Actual_Bytes_To_Read = 0 then
         Result := Success;
         return;
      end if;

      --  This conversion won't overflow, since all FAT filesystem nodes are
      --  guaranteed to have at-most a 32-bit Data_Location field value.
      Current_Cluster := Unsigned_32 (Filesystem_Node.all.Data_Location);

      Cluster_Size_In_Bytes : constant Natural :=
        Filesystem_Info.Sectors_Per_Cluster * Filesystem_Info.Bytes_Per_Sector;

      Follow_Cluster_Chain_Loop : loop
         if Is_Cluster_End_Of_Chain (Current_Cluster, Filesystem_Info.FAT_Type)
           or else Is_Cluster_Bad (Current_Cluster, Filesystem_Info.FAT_Type)
           or else Is_Cluster_Free (Current_Cluster)
         then
            Log_Error
              ("Invalid cluster in file's cluster chain.", Logging_Tags_FAT);

            Result := Invalid_Filesystem;
            return;
         end if;

         Cluster_Bytes_Upper_Bound : constant Unsigned_64 :=
           Unsigned_64 ((Clusters_Scanned + 1) * Cluster_Size_In_Bytes);

         --  Determine whether this cluster contains data we need to read.
         --  If not, skip reading the sectors in this cluster.
         Cluster_Contains_Required_Data : constant Boolean :=
           Current_Offset < Cluster_Bytes_Upper_Bound;

         if Cluster_Contains_Required_Data then
            First_Sector_Of_Cluster : constant Sector_Index_T :=
              Get_First_Sector_Of_Cluster
                (Current_Cluster,
                 Filesystem_Info.Sectors_Per_Cluster,
                 Filesystem_Info.First_Data_Sector);

            --  Calculate the first sector within this cluster that has data
            --  that we need to read.
            Offset_Within_Cluster : constant Natural :=
              Natural (Current_Offset mod Unsigned_64 (Cluster_Size_In_Bytes));

            Start_Sector_Within_Cluster : constant Natural :=
              Offset_Within_Cluster / Filesystem_Info.Bytes_Per_Sector;

            Read_Sectors_Loop : for I in
              Start_Sector_Within_Cluster
              .. Filesystem_Info.Sectors_Per_Cluster - 1
            loop
               Current_Sector := First_Sector_Of_Cluster + Sector_Index_T (I);

               Get_Sector_Block_Number_And_Offset
                 (Current_Sector,
                  Filesystem_Info.Bytes_Per_Sector,
                  Block_Number,
                  Sector_Offset_Within_Block,
                  Result);
               if Is_Error (Result) then
                  return;
               end if;

               Read_Block_From_Filesystem
                 (Filesystem,
                  Reading_Process,
                  Block_Number,
                  Block_Address,
                  Result);
               if Is_Error (Result) then
                  return;
               end if;

               --  Calculate the offset within this sector to copy from.
               Offset_Within_Sector : constant Natural :=
                 Natural
                   (Current_Offset
                    mod Unsigned_64 (Filesystem_Info.Bytes_Per_Sector));

               Bytes_Left_To_Read : constant Natural :=
                 Actual_Bytes_To_Read - Bytes_Read;

               --  Truncate the number of bytes to read from this sector
               --  if it exceeds the sector size.
               Bytes_To_Copy_From_Sector : constant Natural :=
                 (if Offset_Within_Sector + Bytes_Left_To_Read
                    > Filesystem_Info.Bytes_Per_Sector
                  then Filesystem_Info.Bytes_Per_Sector - Offset_Within_Sector
                  else Bytes_Left_To_Read);

               --  Read the data from this sector into the caller's buffer.
               Copy
                 (Buffer_Address + Storage_Offset (Bytes_Read),
                  Block_Address
                  + Sector_Offset_Within_Block
                  + Storage_Offset (Offset_Within_Sector),
                  Bytes_To_Copy_From_Sector);

               Release_Block (Filesystem, Block_Number, Result);
               if Is_Error (Result) then
                  return;
               end if;

               Bytes_Read := Bytes_Read + Bytes_To_Copy_From_Sector;

               exit Follow_Cluster_Chain_Loop when
                 Bytes_Read = Actual_Bytes_To_Read;

               Current_Offset :=
                 Current_Offset + Unsigned_64 (Bytes_To_Copy_From_Sector);
            end loop Read_Sectors_Loop;
         end if;

         Read_FAT_Entry
           (Filesystem,
            Reading_Process,
            Filesystem_Info,
            Current_Cluster,
            Next_Cluster_In_Chain,
            Result);
         if Is_Error (Result) then
            return;
         end if;

         Current_Cluster := Next_Cluster_In_Chain;
         Clusters_Scanned := Clusters_Scanned + 1;
      end loop Follow_Cluster_Chain_Loop;

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Read_File_Data");
         Result := Constraint_Exception;
   end Read_File_Data;

   procedure Extend_Cluster_Chain
     (Filesystem       : Filesystem_Access;
      Writing_Process  : in out Process_Control_Block_T;
      Filesystem_Info  : FAT_Filesystem_Info_T;
      Cluster          : Unsigned_32;
      New_Cluster      : out Unsigned_32;
      Result           : out Function_Result;
      Zero_New_Cluster : Boolean := False)
   is
      Current_Block              : Block_Index_T := 0;
      Block_Address              : Virtual_Address_T := Null_Address;
      Sector_Offset_Within_Block : Storage_Offset := 0;
   begin
      Allocate_Cluster
        (Filesystem, Writing_Process, Filesystem_Info, New_Cluster, Result);
      if Is_Error (Result) then
         return;
      end if;

      Write_FAT_Entry
        (Filesystem,
         Writing_Process,
         Filesystem_Info,
         Cluster,
         New_Cluster,
         Result);
      if Is_Error (Result) then
         return;
      end if;

      if Zero_New_Cluster then
         First_Sector_Of_Allocated_Cluster : constant Sector_Index_T :=
           Get_First_Sector_Of_Cluster
             (New_Cluster,
              Filesystem_Info.Sectors_Per_Cluster,
              Filesystem_Info.First_Data_Sector);

         Zero_Cluster_Loop : for I in
           0 .. Filesystem_Info.Sectors_Per_Cluster - 1
         loop
            Get_Sector_Block_Number_And_Offset
              (First_Sector_Of_Allocated_Cluster + Sector_Index_T (I),
               Filesystem_Info.Bytes_Per_Sector,
               Current_Block,
               Sector_Offset_Within_Block,
               Result);
            if Is_Error (Result) then
               return;
            end if;

            Read_Block_From_Filesystem
              (Filesystem,
               Writing_Process,
               Current_Block,
               Block_Address,
               Result);
            if Is_Error (Result) then
               return;
            end if;

            Set
              (Block_Address + Sector_Offset_Within_Block,
               0,
               Filesystem_Info.Bytes_Per_Sector);

            Write_Block_To_Filesystem
              (Filesystem, Writing_Process, Current_Block, Result);
            if Is_Error (Result) then
               return;
            end if;

            Release_Block (Filesystem, Current_Block, Result);
            if Is_Error (Result) then
               return;
            end if;
         end loop Zero_Cluster_Loop;
      end if;

      Result := Success;
   end Extend_Cluster_Chain;

   procedure Find_Free_Cluster
     (Filesystem      : Filesystem_Access;
      Writing_Process : in out Process_Control_Block_T;
      Filesystem_Info : FAT_Filesystem_Info_T;
      Free_Cluster    : out Unsigned_32;
      Result          : out Function_Result)
   is
      FAT16_Free_Cluster : Unsigned_16 := 0;
   begin
      case Filesystem_Info.FAT_Type is
         when FAT_Type_FAT16 =>
            Find_Free_Cluster_FAT16
              (Filesystem,
               Writing_Process,
               Filesystem_Info,
               FAT16_Free_Cluster,
               Result);

            Free_Cluster := Unsigned_32 (FAT16_Free_Cluster);

         when others         =>
            Log_Error ("FAT type not supported", Logging_Tags_FAT);
            Result := Not_Supported;
      end case;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Find_Free_Cluster");
         Result := Constraint_Exception;
   end Find_Free_Cluster;

   procedure Allocate_Cluster
     (Filesystem      : Filesystem_Access;
      Writing_Process : in out Process_Control_Block_T;
      Filesystem_Info : FAT_Filesystem_Info_T;
      New_Cluster     : out Unsigned_32;
      Result          : out Function_Result) is
   begin
      Find_Free_Cluster
        (Filesystem, Writing_Process, Filesystem_Info, New_Cluster, Result);
      if Is_Error (Result) then
         New_Cluster := 0;
         return;
      end if;

      --  Mark the newly allocated cluster as end-of-chain in the FAT.
      Write_FAT_Entry
        (Filesystem,
         Writing_Process,
         Filesystem_Info,
         New_Cluster,
         Get_EOC_Marker (Filesystem_Info.FAT_Type),
         Result);
      if Is_Error (Result) then
         New_Cluster := 0;
         return;
      end if;

      Log_Debug
        ("Allocated new cluster: " & New_Cluster'Image, Logging_Tags_FAT);

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Allocate_Cluster");
         Result := Constraint_Exception;
   end Allocate_Cluster;

   procedure Write_File_Data
     (Filesystem      : Filesystem_Access;
      Filesystem_Info : FAT_Filesystem_Info_T;
      Writing_Process : in out Process_Control_Block_T;
      Filesystem_Node : Filesystem_Node_Access;
      Buffer_Address  : Virtual_Address_T;
      Start_Offset    : Unsigned_64;
      Bytes_To_Write  : Natural;
      Bytes_Written   : out Natural;
      Result          : out Function_Result)
   is
      Current_Cluster       : Unsigned_32 := 0;
      Next_Cluster_In_Chain : Unsigned_32 := 0;
      Clusters_Scanned      : Natural := 0;

      Current_Sector             : Sector_Index_T := 0;
      Block_Address              : Virtual_Address_T := Null_Address;
      Block_Number               : Block_Index_T := 0;
      Sector_Offset_Within_Block : Storage_Offset := 0;

      Current_Offset : Unsigned_64 := Start_Offset;
   begin
      Bytes_Written := 0;

      --  This conversion won't overflow, since all FAT filesystem nodes are
      --  guaranteed to have at-most a 32-bit Data_Location field value.
      Current_Cluster := Unsigned_32 (Filesystem_Node.all.Data_Location);

      Cluster_Size_In_Bytes : constant Natural :=
        Filesystem_Info.Sectors_Per_Cluster * Filesystem_Info.Bytes_Per_Sector;

      Follow_Cluster_Chain_Loop : loop
         if Is_Cluster_Bad (Current_Cluster, Filesystem_Info.FAT_Type)
           or else Is_Cluster_Free (Current_Cluster)
         then
            Log_Error
              ("Invalid cluster in file's cluster chain.", Logging_Tags_FAT);

            Result := Invalid_Filesystem;
            return;
         end if;

         Cluster_Bytes_Upper_Bound : constant Unsigned_64 :=
           Unsigned_64 ((Clusters_Scanned + 1) * Cluster_Size_In_Bytes);

         --  Determine whether this cluster needs to be written to.
         Cluster_Needs_To_Be_Written : constant Boolean :=
           Current_Offset < Cluster_Bytes_Upper_Bound;

         if Cluster_Needs_To_Be_Written then
            First_Sector_Of_Cluster : constant Sector_Index_T :=
              Get_First_Sector_Of_Cluster
                (Current_Cluster,
                 Filesystem_Info.Sectors_Per_Cluster,
                 Filesystem_Info.First_Data_Sector);

            --  Calculate the first sector within this cluster that has data
            --  that we need to write.
            Offset_Within_Cluster : constant Natural :=
              Natural (Current_Offset mod Unsigned_64 (Cluster_Size_In_Bytes));

            Start_Sector_Within_Cluster : constant Natural :=
              Offset_Within_Cluster / Filesystem_Info.Bytes_Per_Sector;

            Read_Sectors_Loop : for I in
              Start_Sector_Within_Cluster
              .. Filesystem_Info.Sectors_Per_Cluster - 1
            loop
               Current_Sector := First_Sector_Of_Cluster + Sector_Index_T (I);

               Get_Sector_Block_Number_And_Offset
                 (Current_Sector,
                  Filesystem_Info.Bytes_Per_Sector,
                  Block_Number,
                  Sector_Offset_Within_Block,
                  Result);
               if Is_Error (Result) then
                  return;
               end if;

               Read_Block_From_Filesystem
                 (Filesystem,
                  Writing_Process,
                  Block_Number,
                  Block_Address,
                  Result);
               if Is_Error (Result) then
                  return;
               end if;

               --  Calculate the offset within this sector to copy from.
               Offset_Within_Sector : constant Natural :=
                 Natural
                   (Current_Offset
                    mod Unsigned_64 (Filesystem_Info.Bytes_Per_Sector));

               Bytes_Left_To_Write : constant Natural :=
                 Bytes_To_Write - Bytes_Written;

               --  Truncate the number of bytes to write to this sector
               --  if it exceeds the sector size.
               Bytes_To_Copy_To_Sector : constant Natural :=
                 (if Offset_Within_Sector + Bytes_Left_To_Write
                    > Filesystem_Info.Bytes_Per_Sector
                  then Filesystem_Info.Bytes_Per_Sector - Offset_Within_Sector
                  else Bytes_Left_To_Write);

               --  Write the data to this sector from the caller's buffer.
               Copy
                 (Block_Address
                  + Sector_Offset_Within_Block
                  + Storage_Offset (Offset_Within_Sector),
                  Buffer_Address + Storage_Offset (Bytes_Written),
                  Bytes_To_Copy_To_Sector);

               Write_Block_To_Filesystem
                 (Filesystem, Writing_Process, Block_Number, Result);
               if Is_Error (Result) then
                  return;
               end if;

               Release_Block (Filesystem, Block_Number, Result);
               if Is_Error (Result) then
                  return;
               end if;

               Bytes_Written := Bytes_Written + Bytes_To_Copy_To_Sector;

               exit Follow_Cluster_Chain_Loop when
                 Bytes_Written = Bytes_To_Write;

               Current_Offset :=
                 Current_Offset + Unsigned_64 (Bytes_To_Copy_To_Sector);
            end loop Read_Sectors_Loop;
         end if;

         Read_FAT_Entry
           (Filesystem,
            Writing_Process,
            Filesystem_Info,
            Current_Cluster,
            Next_Cluster_In_Chain,
            Result);
         if Is_Error (Result) then
            return;
         end if;

         if Is_Cluster_End_Of_Chain
              (Next_Cluster_In_Chain, Filesystem_Info.FAT_Type)
         then
            --  Allocate next cluster.
            Extend_Cluster_Chain
              (Filesystem,
               Writing_Process,
               Filesystem_Info,
               Current_Cluster,
               Next_Cluster_In_Chain,
               Result,
               True);
            if Is_Error (Result) then
               return;
            end if;
         end if;

         Current_Cluster := Next_Cluster_In_Chain;
         Clusters_Scanned := Clusters_Scanned + 1;
      end loop Follow_Cluster_Chain_Loop;

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Write_File_Data");
         Bytes_Written := 0;
         Result := Constraint_Exception;
   end Write_File_Data;

end Filesystems.FAT;
