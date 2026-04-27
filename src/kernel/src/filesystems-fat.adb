-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Filesystems.Block_Cache; use Filesystems.Block_Cache;
with Filesystems.FAT.FAT16;   use Filesystems.FAT.FAT16;
with Memory.Kernel;           use Memory.Kernel;
with Utilities;               use Utilities;

package body Filesystems.FAT is
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

      if Filesystem.all.Filesystem_Meta_Info_Address = Null_Address then
         Get_Filesystem_Meta_Info (Filesystem, Reading_Process, Result);
         if Is_Error (Result) then
            Found_Node := null;
            return;
         end if;
      end if;

      declare
         FAT_Filesystem_Info : FAT_Filesystem_Info_T
         with
           Import,
           Alignment => 1,
           Address   => Filesystem.all.Filesystem_Meta_Info_Address;
      begin
         if Parent_Node = null
           or else
             Parent_Node.all.Node_Type
             = Filesystem_Node_Type_Mounted_Filesystem
         then
            Find_File_In_Root_Directory
              (Filesystem,
               Reading_Process,
               FAT_Filesystem_Info,
               Filename,
               Parent_Node,
               Found_Node,
               Result);
         else
            Find_File_In_Directory
              (Filesystem,
               Reading_Process,
               FAT_Filesystem_Info,
               Filename,
               Parent_Node,
               Found_Node,
               Result);
         end if;

         if Is_Error (Result) then
            Found_Node := null;
            return;
         end if;
      end;

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Find_File", Logging_Tags_FAT);
         Found_Node := null;
         Result := Constraint_Exception;
   end Find_File;

   procedure Find_File_In_Directory
     (Filesystem      : Filesystem_Access;
      Reading_Process : in out Process_Control_Block_T;
      Filesystem_Info : FAT_Filesystem_Info_T;
      Filename        : Filesystem_Path_T;
      Parent_Node     : Filesystem_Node_Access;
      Filesystem_Node : out Filesystem_Node_Access;
      Result          : out Function_Result) is
   begin
      if Filesystem_Info.FAT_Type = FAT_Type_FAT16 then
         Find_File_In_FAT16_Directory
           (Filesystem,
            Reading_Process,
            Filesystem_Info,
            Filename,
            Parent_Node,
            Filesystem_Node,
            Result);
      else
         Log_Error ("FAT type not supported", Logging_Tags_FAT);
         Filesystem_Node := null;
         Result := Not_Supported;
      end if;
   end Find_File_In_Directory;

   procedure Find_File_In_Root_Directory
     (Filesystem      : Filesystem_Access;
      Reading_Process : in out Process_Control_Block_T;
      Filesystem_Info : FAT_Filesystem_Info_T;
      Filename        : Filesystem_Path_T;
      Parent_Node     : Filesystem_Node_Access;
      Filesystem_Node : out Filesystem_Node_Access;
      Result          : out Function_Result) is
   begin
      if Filesystem_Info.FAT_Type = FAT_Type_FAT16 then
         Find_File_In_FAT16_Root_Directory
           (Filesystem,
            Reading_Process,
            Filesystem_Info,
            Filename,
            Parent_Node,
            Filesystem_Node,
            Result);
         if Is_Error (Result) then
            Log_Error
              ("Error reading root dir: " & Result'Image, Logging_Tags_FAT);
         end if;
      else
         Log_Error ("FAT type not supported", Logging_Tags_FAT);
         Filesystem_Node := null;
         Result := Not_Supported;
      end if;
   exception
      when Constraint_Error =>
         Log_Error
           ("Constraint_Error: Find_File_In_Root_Directory", Logging_Tags_FAT);
         Filesystem_Node := null;
         Result := Constraint_Exception;
   end Find_File_In_Root_Directory;

   pragma
     Warnings
       (Off, "pragma Restrictions (No_Exception_Propagation) in effect");
   function Get_Buffer_Max_Directory_Entry_Count
     (Buffer_Size : Natural) return Natural is
   begin
      return Buffer_Size / 32;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Get_Buffer_Max_Directory_Entry_Count");
         return 0;
   end Get_Buffer_Max_Directory_Entry_Count;
   pragma
     Warnings (On, "pragma Restrictions (No_Exception_Propagation) in effect");

   procedure Get_Filesystem_Meta_Info
     (Filesystem      : Filesystem_Access;
      Reading_Process : in out Process_Control_Block_T;
      Result          : out Function_Result) is
   begin
      Filesystem.all.Filesystem_Meta_Info_Size :=
        FAT_Filesystem_Info_T'Size / 8;

      Allocate_Kernel_Memory
        (Filesystem.all.Filesystem_Meta_Info_Size,
         Filesystem.all.Filesystem_Meta_Info_Address,
         Result);
      if Is_Error (Result) then
         return;
      end if;

      declare
         FAT_Filesystem_Info : FAT_Filesystem_Info_T
         with
           Import,
           Alignment => 1,
           Address   => Filesystem.all.Filesystem_Meta_Info_Address;
      begin
         Read_Boot_Sector
           (Filesystem, Reading_Process, FAT_Filesystem_Info, Result);
         if Is_Error (Result) then
            return;
         end if;

         Print_FAT_Filesystem_Info (FAT_Filesystem_Info);

         Validate_FAT_Filesystem (FAT_Filesystem_Info, Result);
         if Is_Error (Result) then
            return;
         end if;
      end;

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Get_Filesystem_Meta_Info");
         Result := Constraint_Exception;
   end Get_Filesystem_Meta_Info;

   function Get_Filesystem_Node_First_FAT_Cluster
     (Node : Filesystem_Node_Access) return Unsigned_32 is
   begin
      return Get_First_Cluster_From_Index (Node.all.Index);
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Get_Filesystem_Node_First_FAT_Cluster");
         return 0;
   end Get_Filesystem_Node_First_FAT_Cluster;

   function Get_First_Cluster_From_Index
     (Index : Filesystem_Node_Index_T) return Unsigned_32 is
   begin
      return Unsigned_32 (Shift_Right (Index, 32));
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Get_First_Cluster_From_Index");
         return 0;
   end Get_First_Cluster_From_Index;

   function Is_Cluster_End_Of_Chain
     (Cluster : Unsigned_32; FAT_Type : FAT_Type_T) return Boolean is
   begin
      if FAT_Type = FAT_Type_FAT16 then
         return Cluster >= 16#FFF8#;
      elsif FAT_Type = FAT_Type_FAT12 then
         return Cluster >= 16#0FF8#;
      elsif FAT_Type = FAT_Type_FAT32 then
         return Cluster >= 16#0FFFFFF8#;
      end if;

      return True;
   end Is_Cluster_End_Of_Chain;

   procedure Read_DOS_Filename
     (Dir_Entry       : FAT_Directory_Entry_T;
      Filename        : in out Wide_String;
      Filename_Length : in out Natural;
      Result          : out Function_Result) is
   begin
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
         Log_Error ("Constraint_Error: Read_DOS_Filename", Logging_Tags_FAT);
         Result := Unhandled_Exception;
   end Read_DOS_Filename;

   function Get_Node_Type_From_Directory_Entry
     (Dir_Entry : FAT_Directory_Entry_T) return Filesystem_Node_Type_T is
   begin
      if Dir_Entry.Attributes.Directory then
         return Filesystem_Node_Type_Directory;
      end if;

      return Filesystem_Node_Type_File;
   end Get_Node_Type_From_Directory_Entry;

   function Get_FAT_Table_Size (Boot_Sector : Boot_Sector_T) return Unsigned_32
   is
      --  The extended BIOS parameter block, if the filesystem is FAT32.
      FAT32_EBPB : EBPB_FAT32_T
      with
        Import,
        Alignment => 1,
        Address   => Boot_Sector.EBPB_Reserved_Space'Address;
   begin
      if Boot_Sector.BPB.Table_Size > 0 then
         return Unsigned_32 (Boot_Sector.BPB.Table_Size);
      end if;

      return FAT32_EBPB.Table_Size;
   end Get_FAT_Table_Size;

   function Get_Filesystem_Type (Total_Clusters : Natural) return FAT_Type_T is
   begin
      if Total_Clusters < 4085 then
         return FAT_Type_FAT12;
      elsif Total_Clusters < 65525 then
         return FAT_Type_FAT16;
      elsif Total_Clusters < 268_435_445 then
         return FAT_Type_FAT32;
      end if;

      return FAT_Type_ExFAT;
   end Get_Filesystem_Type;

   function Get_Root_Directory_Sector_Count
     (Boot_Sector : Boot_Sector_T) return Unsigned_32 is
   begin
      Root_Entry_Size : constant Unsigned_32 :=
        Unsigned_32 (Boot_Sector.BPB.Root_Entry_Count) * 32;

      Total_Size : constant Unsigned_32 :=
        Root_Entry_Size + Unsigned_32 (Boot_Sector.BPB.Bytes_Per_Sector) - 1;

      return Total_Size / Unsigned_32 (Boot_Sector.BPB.Bytes_Per_Sector);
   exception
      when Constraint_Error =>
         Log_Error
           ("Constraint_Error: Get_Root_Directory_Sector_Count",
            Logging_Tags_FAT);
         return 0;
   end Get_Root_Directory_Sector_Count;

   function Get_Total_Sectors (Boot_Sector : Boot_Sector_T) return Unsigned_32
   is
   begin
      if Boot_Sector.BPB.Total_Sector_Count = 0 then
         return Boot_Sector.BPB.Large_Sector_Count;
      end if;

      return Unsigned_32 (Boot_Sector.BPB.Total_Sector_Count);
   end Get_Total_Sectors;

   procedure Get_Total_Clusters
     (Boot_Sector    : Boot_Sector_T;
      Total_Clusters : out Natural;
      Result         : out Function_Result) is
   begin
      Total_Sectors : constant Unsigned_32 := Get_Total_Sectors (Boot_Sector);
      Root_Directory_Sector_Count : constant Unsigned_32 :=
        Get_Root_Directory_Sector_Count (Boot_Sector);

      FAT_Table_Size : constant Unsigned_32 :=
        Get_FAT_Table_Size (Boot_Sector);

      FAT_Sector_Count : constant Unsigned_32 :=
        Unsigned_32 (Boot_Sector.BPB.Table_Count) * FAT_Table_Size;

      Overhead_Sectors : constant Unsigned_32 :=
        FAT_Sector_Count
        + Unsigned_32 (Boot_Sector.BPB.Reserved_Sector_Count)
        + Root_Directory_Sector_Count;

      --  Guard against the possibility of an underflow when calculating the
      --  number of data sectors caused by a corrupt BPB.
      if Total_Sectors < Overhead_Sectors then
         Log_Error
           ("Invalid BPB: Total sectors less than reserved + FAT sectors.",
            Logging_Tags_FAT);

         Total_Clusters := 0;
         Result := Unhandled_Exception;
         return;
      end if;

      Data_Sectors : constant Unsigned_32 := Total_Sectors - Overhead_Sectors;

      Total_Clusters :=
        Natural
          (Data_Sectors / Unsigned_32 (Boot_Sector.BPB.Sectors_Per_Cluster));

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Get_Total_Clusters", Logging_Tags_FAT);
         Total_Clusters := 0;
         Result := Constraint_Exception;
   end Get_Total_Clusters;

   procedure Print_FAT_Filesystem_Info
     (FAT_Filesystem_Info : FAT_Filesystem_Info_T) is
   begin
      Log_Debug
        ("FAT Filesystem Info:"
         & ASCII.LF
         & "  Root_Directory_Sector:       "
         & FAT_Filesystem_Info.Root_Directory_Sector'Image
         & ASCII.LF
         & "  Root_Directory_Sector_Count: "
         & FAT_Filesystem_Info.Root_Directory_Sector_Count'Image
         & ASCII.LF
         & "  Root_Directory_Buffer_Size:  "
         & FAT_Filesystem_Info.Root_Directory_Buffer_Size'Image
         & ASCII.LF
         & "  Bytes_Per_Sector:            "
         & FAT_Filesystem_Info.Bytes_Per_Sector'Image
         & ASCII.LF
         & "  First_FAT_Sector:            "
         & FAT_Filesystem_Info.First_FAT_Sector'Image
         & ASCII.LF
         & "  First_Data_Sector:           "
         & FAT_Filesystem_Info.First_Data_Sector'Image
         & ASCII.LF
         & "  FAT_Sector_Count:            "
         & FAT_Filesystem_Info.FAT_Sector_Count'Image
         & ASCII.LF
         & "  FAT_Buffer_Size:             "
         & FAT_Filesystem_Info.FAT_Buffer_Size'Image
         & ASCII.LF
         & "  Sectors_Per_Cluster:         "
         & FAT_Filesystem_Info.Sectors_Per_Cluster'Image,
         Logging_Tags_FAT);
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Print_FAT_Filesystem_Info");
   end Print_FAT_Filesystem_Info;

   procedure Get_Root_Directory_Sector
     (EBPB_Address                : Virtual_Address_T;
      FAT_Type                    : FAT_Type_T;
      Sectors_Per_Cluster         : Natural;
      First_Data_Sector           : Unsigned_64;
      Root_Directory_Sector_Count : Unsigned_32;
      Root_Directory_Sector       : out Unsigned_64;
      Result                      : out Function_Result)
   is
      --  The extended FAT32 BIOS parameter block,
      FAT32_EBPB : EBPB_FAT32_T
      with Import, Alignment => 1, Address => EBPB_Address;
   begin
      case FAT_Type is
         when FAT_Type_FAT32 .. FAT_Type_ExFAT =>
            Root_Directory_Sector :=
              Get_First_Sector_Of_Cluster
                (FAT32_EBPB.Root_Cluster,
                 Sectors_Per_Cluster,
                 First_Data_Sector);

         when others                           =>
            Root_Directory_Sector :=
              First_Data_Sector - Unsigned_64 (Root_Directory_Sector_Count);
      end case;

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error
           ("Constraint_Error: Get_Root_Directory_Sector", Logging_Tags_FAT);
         Root_Directory_Sector := 0;
         Result := Constraint_Exception;
   end Get_Root_Directory_Sector;

   procedure Read_Boot_Sector
     (Filesystem      : Filesystem_Access;
      Reading_Process : in out Process_Control_Block_T;
      Filesystem_Info : out FAT_Filesystem_Info_T;
      Result          : out Function_Result)
   is
      Sector_Address : Virtual_Address_T := Null_Address;
   begin
      Log_Debug ("Reading boot sector...", Logging_Tags_FAT);

      --  Read a single 512 byte sector.
      --  We don't know the real sector size yet, but the boot sector is
      --  always located in the first 512 bytes.
      Read_Sector_From_Filesystem
        (Filesystem, Reading_Process, 0, 512, Sector_Address, Result);
      if Is_Error (Result) then
         return;
      end if;

      Parse_Boot_Sector : declare
         Boot_Sector : aliased Boot_Sector_T
         with Import, Alignment => 1, Address => Sector_Address;

         Total_Clusters : Natural := 0;
      begin
         Get_Total_Clusters (Boot_Sector, Total_Clusters, Result);
         if Is_Error (Result) then
            return;
         end if;

         Filesystem_Info.FAT_Type := Get_Filesystem_Type (Total_Clusters);

         Filesystem_Info.Root_Directory_Sector_Count :=
           Get_Root_Directory_Sector_Count (Boot_Sector);

         Filesystem_Info.Bytes_Per_Sector :=
           Natural (Boot_Sector.BPB.Bytes_Per_Sector);

         Filesystem_Info.Root_Directory_Buffer_Size :=
           Filesystem_Info.Bytes_Per_Sector
           * Natural (Filesystem_Info.Root_Directory_Sector_Count);

         Filesystem_Info.FAT_Sector_Count := Get_FAT_Table_Size (Boot_Sector);

         Filesystem_Info.First_FAT_Sector :=
           Unsigned_64 (Boot_Sector.BPB.Reserved_Sector_Count);

         Filesystem_Info.FAT_Buffer_Size :=
           Natural (Filesystem_Info.FAT_Sector_Count)
           * Filesystem_Info.Bytes_Per_Sector;

         Filesystem_Info.Sectors_Per_Cluster :=
           Natural (Boot_Sector.BPB.Sectors_Per_Cluster);

         Total_Tables_Size : constant Unsigned_64 :=
           Unsigned_64 (Boot_Sector.BPB.Table_Count)
           * Unsigned_64 (Filesystem_Info.FAT_Sector_Count);

         Filesystem_Info.First_Data_Sector :=
           Unsigned_64 (Boot_Sector.BPB.Reserved_Sector_Count)
           + Unsigned_64 (Filesystem_Info.Root_Directory_Sector_Count)
           + Total_Tables_Size;

         Get_Root_Directory_Sector
           (Boot_Sector.EBPB_Reserved_Space'Address,
            Filesystem_Info.FAT_Type,
            Filesystem_Info.Sectors_Per_Cluster,
            Filesystem_Info.First_Data_Sector,
            Filesystem_Info.Root_Directory_Sector_Count,
            Filesystem_Info.Root_Directory_Sector,
            Result);
         if Is_Error (Result) then
            return;
         end if;

      end Parse_Boot_Sector;

      Release_Sector (Filesystem, 0, 512, Result);

   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Read_Boot_Sector");
         Result := Constraint_Exception;
   end Read_Boot_Sector;

   procedure Read_Cluster_Into_Buffer
     (Filesystem             : Filesystem_Access;
      Reading_Process        : in out Process_Control_Block_T;
      Filesystem_Info        : FAT_Filesystem_Info_T;
      Cluster                : Unsigned_32;
      Buffer_Virtual_Address : Virtual_Address_T;
      Result                 : out Function_Result)
   is
      Current_Read_Sector : Unsigned_64 := 0;
      Sector_Address      : Virtual_Address_T := Null_Address;

      Destination_Virtual_Address : Virtual_Address_T :=
        Buffer_Virtual_Address;
   begin
      Log_Debug ("Reading cluster: " & Cluster'Image, Logging_Tags_FAT);

      if Cluster < 2 then
         Log_Error
           ("Invalid cluster number: " & Cluster'Image, Logging_Tags_FAT);
         Result := Invalid_Argument;
         return;
      end if;

      Current_Read_Sector :=
        Get_First_Sector_Of_Cluster
          (Cluster,
           Filesystem_Info.Sectors_Per_Cluster,
           Filesystem_Info.First_Data_Sector);

      for I in 0 .. Filesystem_Info.Sectors_Per_Cluster - 1 loop
         Read_Sector_From_Filesystem
           (Filesystem,
            Reading_Process,
            Current_Read_Sector,
            Filesystem_Info.Bytes_Per_Sector,
            Sector_Address,
            Result);
         if Is_Error (Result) then
            return;
         end if;

         Copy
           (Destination_Virtual_Address,
            Sector_Address,
            Filesystem_Info.Bytes_Per_Sector);

         Destination_Virtual_Address :=
           Destination_Virtual_Address
           + Storage_Offset (Filesystem_Info.Bytes_Per_Sector);

         Release_Sector
           (Filesystem,
            Current_Read_Sector,
            Filesystem_Info.Bytes_Per_Sector,
            Result);
         if Is_Error (Result) then
            return;
         end if;

         Current_Read_Sector := Current_Read_Sector + 1;
      end loop;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Read_Cluster_Into_Buffer");
         Result := Constraint_Exception;
   end Read_Cluster_Into_Buffer;

   procedure Read_FAT_Entry
     (Filesystem      : Filesystem_Access;
      Reading_Process : in out Process_Control_Block_T;
      Filesystem_Info : FAT_Filesystem_Info_T;
      Cluster         : Unsigned_32;
      FAT_Entry       : out Unsigned_32;
      Result          : out Function_Result) is
   begin
      case Filesystem_Info.FAT_Type is
         when FAT_Type_FAT16 =>
            Read_FAT16_Table_Entry
              (Filesystem,
               Reading_Process,
               Filesystem_Info,
               Cluster,
               FAT_Entry,
               Result);

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

   procedure Read_LFN_Entry_Filename
     (Dir_Entry       : FAT_Directory_Entry_T;
      Filename        : in out Wide_String;
      Filename_Length : in out Natural;
      Result          : out Function_Result)
   is
      --  The long file name entry being parsed.
      LFN_Entry : Long_File_Name_Directory_Entry
      with Import, Alignment => 1, Address => Dir_Entry'Address;

      --  The current offset into reading the name.
      Name_offset : Natural := 1;
   begin
      if not Is_LFN_Directory_Entry (Dir_Entry) then
         Result := Invalid_Argument;
         return;
      end if;

      if LFN_Entry.Sequence.Number = 0 then
         Result := Invalid_Argument;
         return;
      end if;

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
   end Read_LFN_Entry_Filename;

   procedure Search_FAT_Directory_For_File
     (Filesystem      : Filesystem_Access;
      Directory       : Directory_Index_T;
      Filename        : Filesystem_Path_T;
      Parent_Node     : Filesystem_Node_Access;
      Filesystem_Node : out Filesystem_Node_Access;
      Result          : out Function_Result)
   is
      --  Whether a long file name entry is currently being read.
      Entry_Has_Long_Filename : Boolean := False;
      --  The UCS-2-encoded filename read from the current directory entry.
      Entry_Filename          :
        Wide_String (1 .. Filesystem_Node_Name_Max_Byte_Length) :=
          [others => Wide_Character'Val (0)];
      Entry_Filename_Length   : Natural := 0;

      --  The UCS-2-encoded filename converted to the OS' native UTF-8 format.
      --  This is used to compare against the target filename.
      UTF8_Encoded_Filename             : Filesystem_Node_Name_T;
      UTF8_Encoded_Filename_Byte_Length : Natural := 0;
   begin
      Log_Debug
        ("Searching directory for file '" & Filename & "'", Logging_Tags_FAT);

      Filesystem_Node := null;

      for Dir_Idx in 1 .. Directory'Length loop
         Log_Debug
           ("Scanning directory entry: " & Dir_Idx'Image, Logging_Tags_FAT);

         if Is_Last_Directory_Entry (Directory (Dir_Idx)) then
            Log_Debug ("Reached last directory entry.", Logging_Tags_FAT);
            exit;
         end if;

         if not Is_Unused_Directory_Entry (Directory (Dir_Idx)) then
            --  If the entry attributes indicate that this is a long
            --  file name entry, then parse it differently.
            if Is_LFN_Directory_Entry (Directory (Dir_Idx)) then
               Entry_Has_Long_Filename := True;

               --  Read this section of the name from the LFN entry.
               Read_LFN_Entry_Filename
                 (Directory (Dir_Idx),
                  Entry_Filename,
                  Entry_Filename_Length,
                  Result);
               if Is_Error (Result) then
                  return;
               end if;
            else
               --  If we're not in the process of reading a long file name
               --  then read the DOS filename from the 8.3 entry.
               if not Entry_Has_Long_Filename then
                  Read_DOS_Filename
                    (Directory (Dir_Idx),
                     Entry_Filename,
                     Entry_Filename_Length,
                     Result);
                  if Is_Error (Result) then
                     return;
                  end if;
               end if;

               --  Every sequence of LFN entries is followed by its
               --  corresponding DOS 8.3 entry, so at this point we have
               --  either the full filename of the long directory entry, or the
               --  full DOS directory entry, and can now convert it to UTF-8,
               --  and compare it against the target filename.
               Read_FAT_Filename_Into_Filesystem_Node_Name
                 (Entry_Filename,
                  Entry_Filename_Length,
                  UTF8_Encoded_Filename,
                  UTF8_Encoded_Filename_Byte_Length,
                  Result);
               if Is_Error (Result) then
                  return;
               end if;

               Log_Debug
                 ("Parsed FAT file entry with filename: '"
                  & UTF8_Encoded_Filename
                      (1 .. UTF8_Encoded_Filename_Byte_Length)
                  & "'",
                  Logging_Tags_FAT);

               --  Check if the parsed filename matches the target filename.
               if Does_Node_Name_Match_Path_Name
                    (UTF8_Encoded_Filename,
                     UTF8_Encoded_Filename_Byte_Length,
                     Filename)
               then
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
                       Get_Directory_Entry_Node_Index (First_Cluster, Dir_Idx),
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

               Entry_Filename := [others => Wide_Character'Val (0)];
               Entry_Filename_Length := 0;
               Entry_Has_Long_Filename := False;
            end if;
         end if;
      end loop;

      Result := File_Not_Found;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Search_FAT_Directory_For_File");
         Result := Constraint_Exception;
   end Search_FAT_Directory_For_File;

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

      if Filesystem.all.Filesystem_Meta_Info_Address = Null_Address then
         Get_Filesystem_Meta_Info (Filesystem, Reading_Process, Result);
         if Is_Error (Result) then
            return;
         end if;
      end if;

      declare
         Filesystem_Info : FAT_Filesystem_Info_T
         with
           Import,
           Alignment => 1,
           Address   => Filesystem.all.Filesystem_Meta_Info_Address;
      begin
         Read_File_Clusters
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
      end;

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Filesystems.FAT.Read_File");
         Result := Constraint_Exception;
   end Read_File;

   procedure Read_File_Clusters
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
      Cluster_Buffer_Address : Virtual_Address_T := Null_Address;

      --  An alternative 'result' for freeing the allocated buffer, so that
      --  we don't overwrite the main result variable.
      Free_Buffer_Result : Function_Result := Unset;

      Cluster_Size                  : Natural := 0;
      Starting_Cluster_Chain_Length : Natural := 0;

      Current_Read_Cluster  : Unsigned_32 := 0;
      Next_Cluster_In_Chain : Unsigned_32 := 0;

      Current_Offset             : Unsigned_64 := Start_Offset;
      Offset_Within_Cluster      : Natural := 0;
      Bytes_To_Copy_From_Cluster : Natural := 0;
      Bytes_Left_To_Read         : Natural := 0;

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

      --  Allocate a buffer to hold each cluster we need to read.
      Cluster_Size :=
        Filesystem_Info.Sectors_Per_Cluster * Filesystem_Info.Bytes_Per_Sector;

      Allocate_Kernel_Memory (Cluster_Size, Cluster_Buffer_Address, Result);
      if Is_Error (Result) then
         return;
      end if;

      Current_Read_Cluster := Unsigned_32 (Filesystem_Node.all.Data_Location);

      --  Follow the file's cluster chain until we get to the file descriptor's
      --  current offset. From there we can start reading the required data.
      Starting_Cluster_Chain_Length :=
        Natural (Start_Offset / Unsigned_64 (Cluster_Size));

      if Starting_Cluster_Chain_Length > 0 then
         Log_Debug ("Reading starting cluster chain...", Logging_Tags_FAT);

         for I in 1 .. Starting_Cluster_Chain_Length loop
            --  Check for the premature end of the cluster chain.
            if Is_Cluster_End_Of_Chain
                 (Current_Read_Cluster, Filesystem_Info.FAT_Type)
            then
               Log_Debug ("End of cluster chain reached.", Logging_Tags_FAT);
               exit;
            end if;

            Read_FAT_Entry
              (Filesystem,
               Reading_Process,
               Filesystem_Info,
               Current_Read_Cluster,
               Next_Cluster_In_Chain,
               Result);
            if Is_Error (Result) then
               goto Free_Read_File_Buffer;
            end if;

            Current_Read_Cluster := Next_Cluster_In_Chain;
         end loop;
      end if;

      --  Read each of the required clusters into the buffer, copying out
      --  the necessary bytes from each cluster until the requested
      --  number of bytes have been read.
      loop
         Bytes_Left_To_Read := Actual_Bytes_To_Read - Bytes_Read;

         Offset_Within_Cluster :=
           Natural (Current_Offset mod Unsigned_64 (Cluster_Size));

         --  Truncate the number of bytes to copy within this cluster
         --  if it exceeds the cluster size.
         if Offset_Within_Cluster + Bytes_Left_To_Read > Cluster_Size then
            Bytes_To_Copy_From_Cluster := Cluster_Size - Offset_Within_Cluster;
         else
            Bytes_To_Copy_From_Cluster := Bytes_Left_To_Read;
         end if;

         Read_Cluster_Into_Buffer
           (Filesystem,
            Reading_Process,
            Filesystem_Info,
            Current_Read_Cluster,
            Cluster_Buffer_Address,
            Result);
         if Is_Error (Result) then
            goto Free_Read_File_Buffer;
         end if;

         --  Copy the required bytes from the cluster into the buffer.
         Copy
           (Buffer_Address + Storage_Offset (Bytes_Read),
            Cluster_Buffer_Address + Storage_Offset (Offset_Within_Cluster),
            Bytes_To_Copy_From_Cluster);

         --  If we've read all the bytes we need to, exit.
         Bytes_Read := Bytes_Read + Bytes_To_Copy_From_Cluster;
         if Bytes_Read = Actual_Bytes_To_Read then
            exit;
         end if;

         Current_Offset :=
           Current_Offset + Unsigned_64 (Bytes_To_Copy_From_Cluster);

         --  Check for the premature end of cluster chain.
         if Is_Cluster_End_Of_Chain
              (Current_Read_Cluster, Filesystem_Info.FAT_Type)
         then
            Log_Debug ("End of cluster chain reached.", Logging_Tags_FAT);
            exit;
         end if;

         Read_FAT_Entry
           (Filesystem,
            Reading_Process,
            Filesystem_Info,
            Current_Read_Cluster,
            Next_Cluster_In_Chain,
            Result);
         if Is_Error (Result) then
            goto Free_Read_File_Buffer;
         end if;

         Current_Read_Cluster := Next_Cluster_In_Chain;
      end loop;

      <<Free_Read_File_Buffer>>
      Free_Kernel_Memory (Cluster_Buffer_Address, Free_Buffer_Result);
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Read_File_Clusters");
         Result := Constraint_Exception;
   end Read_File_Clusters;

   procedure Validate_FAT_Filesystem
     (Filesystem_Info : FAT_Filesystem_Info_T; Result : out Function_Result) is
   begin
      Sector_Size_Valid : constant Boolean :=
        (Filesystem_Info.Bytes_Per_Sector = 512)
        or else (Filesystem_Info.Bytes_Per_Sector = 1024)
        or else (Filesystem_Info.Bytes_Per_Sector = 2048)
        or else (Filesystem_Info.Bytes_Per_Sector = 4096);

      if not Sector_Size_Valid then
         Log_Error
           ("Filesystem Invalid: Invalid sector size.", Logging_Tags_FAT);
         Result := Invalid_Filesystem;
         return;
      end if;

      if Filesystem_Info.Sectors_Per_Cluster = 0 then
         Log_Error
           ("Filesystem Invalid: Sectors per cluster cannot be zero.",
            Logging_Tags_FAT);
         Result := Invalid_Filesystem;
         return;
      end if;

      if Unsigned_64 (Filesystem_Info.Root_Directory_Sector_Count)
        > Filesystem_Info.First_Data_Sector
      then
         Log_Error
           ("Filesystem Invalid: "
            & "Root directory cannot be located within data region.",
            Logging_Tags_FAT);
         Result := Invalid_Filesystem;
         return;
      end if;

      Result := Success;
   end Validate_FAT_Filesystem;

   procedure Read_FAT_Filename_Into_Filesystem_Node_Name
     (FAT_Filename                     : Wide_String;
      FAT_Filename_Length              : Natural;
      Filesystem_Node_Name             : out Filesystem_Node_Name_T;
      Filesystem_Node_Name_Byte_Length : out Integer;
      Result                           : out Function_Result)
   is
      Current_Node_Name_Index : Natural := 1;

      --  Each UCS-2 character requires up to 3 bytes to encode in UTF-8.
      --  This buffer holds those 1-3 bytes for each character conversion.
      Converted_UCS2_Char_Buffer      : UTF8_Converted_Char_Buffer_T;
      --  The length of the converted UTF-8 character in bytes.
      Converted_UCS2_Char_Byte_Length : Integer := 0;
   begin
      Filesystem_Node_Name := [others => Character'Val (0)];
      Filesystem_Node_Name_Byte_Length := 0;

      for I in 1 .. FAT_Filename_Length loop
         Encode_UCS2_Wide_Char_As_UTF8_Buffer
           (FAT_Filename (I),
            Converted_UCS2_Char_Buffer,
            Converted_UCS2_Char_Byte_Length);

         for J in 1 .. Converted_UCS2_Char_Byte_Length loop
            Filesystem_Node_Name_Byte_Length :=
              Filesystem_Node_Name_Byte_Length + 1;

            Filesystem_Node_Name (Current_Node_Name_Index) :=
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
end Filesystems.FAT;
