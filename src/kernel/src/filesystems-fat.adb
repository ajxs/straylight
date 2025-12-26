-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Filesystems.Block_Cache; use Filesystems.Block_Cache;
with Memory;                  use Memory;
with System_State;            use System_State;

package body Filesystems.FAT is
   procedure Find_File
     (Filesystem      : Filesystem_Access;
      Reading_Process : in out Process_Control_Block_T;
      Filename        : Wide_String;
      Parent_Node     : Filesystem_Node_Access;
      Found_Node      : out Filesystem_Node_Access;
      Result          : out Function_Result) is
   begin
      if not Is_Valid_Filesystem_Pointer (Filesystem)
        or else Filesystem.all.Filesystem_Type /= Filesystem_Type_FAT
      then
         Log_Error ("Invalid FAT filesystem", Logging_Tags_FAT);
         Found_Node := null;
         Result := Invalid_Argument;
         return;
      end if;

      Log_Debug_Wide ("Finding file: '" & Filename & "'", Logging_Tags_FAT);

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
           or else Parent_Node.all.Node_Type
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
         Result := Constraint_Exception;
   end Find_File;

   procedure Find_File_In_Directory
     (Filesystem      : Filesystem_Access;
      Reading_Process : in out Process_Control_Block_T;
      Filesystem_Info : FAT_Filesystem_Info_T;
      Filename        : Wide_String;
      Parent_Node     : Filesystem_Node_Access;
      Filesystem_Node : out Filesystem_Node_Access;
      Result          : out Function_Result) is
   begin
      if Filesystem_Info.FAT_Type = FAT16 then
         Find_File_In_FAT16_Directory
           (Filesystem,
            Reading_Process,
            Filesystem_Info,
            Filename,
            Parent_Node,
            Filesystem_Node,
            Result);
      else
         Log_Error
           ("Find_File_In_Directory only supports FAT16 at present.",
            Logging_Tags_FAT);
         Filesystem_Node := null;
         Result := Invalid_Argument;
      end if;
   end Find_File_In_Directory;

   procedure Find_File_In_FAT16_Directory
     (Filesystem      : Filesystem_Access;
      Reading_Process : in out Process_Control_Block_T;
      Filesystem_Info : FAT_Filesystem_Info_T;
      Filename        : Wide_String;
      Parent_Node     : Filesystem_Node_Access;
      Filesystem_Node : out Filesystem_Node_Access;
      Result          : out Function_Result)
   is
      Cluster_Buffer_Address : Virtual_Address_T := Null_Address;
      Cluster_Size           : Positive;

      --  The maximum number of directory entries that can fit in the buffer.
      Maximum_Directory_Entries : Natural;

      Current_Read_Cluster  : Unsigned_32 := 0;
      Next_Cluster_In_Chain : Unsigned_32 := 0;

      --  An alternative 'result' for freeing the allocated buffer, so that
      --  we don't overwrite the main result variable.
      Free_Buffer_Result : Function_Result := Unset;
   begin
      Log_Debug ("Finding file in FAT16 directory", Logging_Tags_FAT);

      Cluster_Size :=
        Filesystem_Info.Sectors_Per_Cluster * Filesystem_Info.Bytes_Per_Sector;

      Maximum_Directory_Entries :=
        Get_Buffer_Max_Directory_Entry_Count (Cluster_Size);

      Current_Read_Cluster :=
        Get_First_Cluster_From_Index (Parent_Node.all.Index);

      Allocate_Kernel_Memory (Cluster_Size, Cluster_Buffer_Address, Result);
      if Is_Error (Result) then
         return;
      end if;

      --  Read each cluster in the directory's cluster chain into the buffer,
      --  parsing the directory entries contained in the cluster as we go.
      --  This is done for the sake of efficiency.
      --  If we find the file we're looking for in the first cluster we
      --  read, we can avoid reading further clusters.
      loop
         Read_Cluster_Into_Buffer
           (Filesystem,
            Reading_Process,
            Filesystem_Info,
            Current_Read_Cluster,
            Cluster_Buffer_Address,
            Result);
         if Is_Error (Result) then
            goto Free_Buffer;
         end if;

         Parse_Directory_Buffer :
         declare
            Directory : Directory_Index_T (1 .. Maximum_Directory_Entries)
            with Import, Address => Cluster_Buffer_Address, Alignment => 1;
         begin
            Search_FAT_Directory_For_File
              (Filesystem,
               Directory,
               Filename,
               Parent_Node,
               Filesystem_Node,
               Result);
            if Is_Error (Result) then
               goto Free_Buffer;
            elsif Result = Success then
               Log_Debug ("Found matching file entry.", Logging_Tags_FAT);
               Result := Success;
               goto Free_Buffer;
            end if;
         end Parse_Directory_Buffer;

         --  Check for end of cluster chain.
         if Is_Cluster_End_Of_Chain
              (Current_Read_Cluster, Filesystem_Info.FAT_Type)
         then
            Log_Debug ("End of cluster chain reached.", Logging_Tags_FAT);
            exit;
         end if;

         --  Read the next cluster in the FAT chain.
         Read_FAT_Entry
           (Filesystem,
            Reading_Process,
            Filesystem_Info,
            Current_Read_Cluster,
            Next_Cluster_In_Chain,
            Result);
         if Is_Error (Result) then
            goto Free_Buffer;
         end if;

         Log_Debug
           ("Next directory cluster: " & Next_Cluster_In_Chain'Image,
            Logging_Tags_FAT);

         Current_Read_Cluster := Next_Cluster_In_Chain;
      end loop;

      Log_Debug ("File not found in FAT directory.", Logging_Tags_FAT);
      Result := File_Not_Found;

      <<Free_Buffer>>
      Free_Kernel_Memory (Cluster_Buffer_Address, Free_Buffer_Result);
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Find_File_In_FAT16_Directory");
         Result := Constraint_Exception;
   end Find_File_In_FAT16_Directory;

   procedure Find_File_In_FAT16_Root_Directory
     (Filesystem      : Filesystem_Access;
      Reading_Process : in out Process_Control_Block_T;
      Filesystem_Info : FAT_Filesystem_Info_T;
      Filename        : Wide_String;
      Parent_Node     : Filesystem_Node_Access;
      Filesystem_Node : out Filesystem_Node_Access;
      Result          : out Function_Result)
   is
      Directory_Buffer_Address : Virtual_Address_T := Null_Address;

      Maximum_Directory_Entries : Natural := 0;

      --  An alternative 'result' for freeing the allocated buffer, so that
      --  we don't overwrite the main result variable.
      Free_Buffer_Result : Function_Result := Unset;
   begin
      Allocate_Kernel_Memory
        (Filesystem_Info.Root_Directory_Buffer_Size,
         Directory_Buffer_Address,
         Result);
      if Is_Error (Result) then
         return;
      end if;

      Maximum_Directory_Entries :=
        Get_Buffer_Max_Directory_Entry_Count
          (Filesystem_Info.Root_Directory_Buffer_Size);

      --  Because there's no cluster chain for the root directory in FAT16,
      --  we need to read the entire root directory into memory in one go,
      --  and then parse all the entries.
      Read_FAT16_Root_Directory_Into_Buffer
        (Filesystem,
         Reading_Process,
         Filesystem_Info,
         Directory_Buffer_Address,
         Result);
      if Is_Error (Result) then
         goto Free_Buffer;
      end if;

      Parse_Directory_Buffer :
      declare
         Directory : Directory_Index_T (1 .. Maximum_Directory_Entries)
         with Import, Address => Directory_Buffer_Address, Alignment => 1;
      begin
         Search_FAT_Directory_For_File
           (Filesystem,
            Directory,
            Filename,
            Parent_Node,
            Filesystem_Node,
            Result);
         if Is_Error (Result) then
            goto Free_Buffer;
         elsif Result = Success then
            Log_Debug ("Found matching file entry.", Logging_Tags_FAT);
            Result := Success;
            goto Free_Buffer;
         end if;
      end Parse_Directory_Buffer;

      Log_Debug ("File not found in root FAT directory.", Logging_Tags_FAT);
      Result := File_Not_Found;

      <<Free_Buffer>>
      Free_Kernel_Memory (Directory_Buffer_Address, Free_Buffer_Result);
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Find_File_In_FAT16_Root_Directory");
         Result := Constraint_Exception;
   end Find_File_In_FAT16_Root_Directory;

   procedure Find_File_In_Root_Directory
     (Filesystem      : Filesystem_Access;
      Reading_Process : in out Process_Control_Block_T;
      Filesystem_Info : FAT_Filesystem_Info_T;
      Filename        : Wide_String;
      Parent_Node     : Filesystem_Node_Access;
      Filesystem_Node : out Filesystem_Node_Access;
      Result          : out Function_Result) is
   begin
      if Filesystem_Info.FAT_Type = FAT16 then
         Find_File_In_FAT16_Root_Directory
           (Filesystem,
            Reading_Process,
            Filesystem_Info,
            Filename,
            Parent_Node,
            Filesystem_Node,
            Result);
         if Is_Error (Result) then
            Log_Error ("Error reading root dir: " & Result'Image);
         end if;
      end if;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Find_File_In_Root_Directory");
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

   function Get_Directory_Entry_First_Sector
     (Filesystem_Info : FAT_Filesystem_Info_T;
      Dir_Entry       : FAT_Directory_Entry_T) return Unsigned_64
   is
      First_Cluster : Unsigned_32 := 0;
   begin
      --  Combine the two 16-bit halves of the cluster.
      First_Cluster := Get_First_Cluster_Of_Dir_Entry (Dir_Entry);

      return Get_First_Sector_Of_Cluster (Filesystem_Info, First_Cluster);
   end Get_Directory_Entry_First_Sector;

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
      if FAT_Type = FAT16 then
         return Cluster >= 16#FFF8#;
      elsif FAT_Type = FAT12 then
         return Cluster >= 16#0FF8#;
      elsif FAT_Type = FAT32 then
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
      Copy_Name :
      for I in 1 .. 8 loop
         --  The DOS filename is padded by spaces.
         --  If we encounter a space we can assume we've reached the end
         --  of the file name.
         exit Copy_Name when Dir_Entry.File_Name (I) = ' ';

         Filename (I) :=
           Wide_Character'Val (Character'Pos (Dir_Entry.File_Name (I)));

         --  Increment the name length.
         Filename_Length := Filename_Length + 1;
      end loop Copy_Name;

      --  If this DOS file entry has an extension
      --  then it is appended after a trailing '.'
      --  character at the end of the file name.
      if Dir_Entry.File_Ext (1) /= ' ' then
         Filename_Length := Filename_Length + 1;

         --  Place the '.' between the file name and the file extension.
         Filename (Filename_Length) := '.';

         --  Copy the file extension into the filename.
         Copy_Extension :
         for I in 1 .. 3 loop
            --  The DOS file extension is padded by spaces.
            --  If we encounter a space we can assume we've reached the end
            --  of the file name.
            exit Copy_Extension when Dir_Entry.File_Ext (I) = ' ';

            Filename_Length := Filename_Length + 1;

            --  Convert each DOS file extension character to a wide character.
            Filename (Filename_Length) :=
              Wide_Character'Val (Character'Pos (Dir_Entry.File_Ext (I)));
         end loop Copy_Extension;
      end if;

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint error reading LFN");
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

   function Get_FAT_Sector_Count (Boot_Sector : Boot_Sector_T) return Natural
   is
      --  The extended BIOS parameter block, if the filesystem is FAT32.
      FAT32_EBPB : FAT32_Extended_BIOS_Parameter_Block
      with
        Import,
        Alignment => 1,
        Address   => Boot_Sector.EBPB_Reserved_Space'Address;
   begin
      if Boot_Sector.BPB.Table_Size > 0 then
         return Natural (Boot_Sector.BPB.Table_Size);
      end if;

      return Natural (FAT32_EBPB.Table_Size);
   exception
      when Constraint_Error =>
         return 0;
   end Get_FAT_Sector_Count;

   function Get_Filesystem_Type (Total_Clusters : Natural) return FAT_Type_T is
   begin
      if Total_Clusters < 4085 then
         return FAT12;
      elsif Total_Clusters < 65525 then
         return FAT16;
      elsif Total_Clusters < 268_435_445 then
         return FAT32;
      end if;

      return ExFAT;
   end Get_Filesystem_Type;

   function Get_Root_Directory_Sector_Count
     (Boot_Sector : Boot_Sector_T) return Natural
   is
      Root_Entry_Size : Natural := 0;
      Total_Size      : Natural := 0;
   begin
      Root_Entry_Size := Natural (Boot_Sector.BPB.Root_Entry_Count * 32);
      Total_Size :=
        Root_Entry_Size + Natural (Boot_Sector.BPB.Bytes_Per_Sector) - 1;

      return Total_Size / Natural (Boot_Sector.BPB.Bytes_Per_Sector);
   exception
      when Constraint_Error =>
         return 0;
   end Get_Root_Directory_Sector_Count;

   function Get_Total_Clusters (Boot_Sector : Boot_Sector_T) return Natural is
      --  The number of sectors in each File Allocation Table.
      Sectors_Per_FAT  : Unsigned_32 := 0;
      FAT_Sector_Count : Unsigned_32 := 0;
      Total_Sectors    : Unsigned_32 := 0;
      Data_Sectors     : Unsigned_32 := 0;
   begin
      Sectors_Per_FAT := Unsigned_32 (Get_FAT_Sector_Count (Boot_Sector));

      if Boot_Sector.BPB.Total_Sector_Count = 0 then
         Total_Sectors := Boot_Sector.BPB.Large_Sector_Count;
      else
         Total_Sectors := Unsigned_32 (Boot_Sector.BPB.Total_Sector_Count);
      end if;

      FAT_Sector_Count :=
        Unsigned_32 (Boot_Sector.BPB.Table_Count) * Sectors_Per_FAT;

      Data_Sectors :=
        Total_Sectors
        - (FAT_Sector_Count
           + Unsigned_32 (Boot_Sector.BPB.Reserved_Sector_Count));

      return
        Natural
          (Data_Sectors / Unsigned_32 (Boot_Sector.BPB.Sectors_Per_Cluster));
   exception
      when Constraint_Error =>
         return 0;
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

      Parse_Boot_Sector :
      declare
         Boot_Sector : aliased Boot_Sector_T
         with Import, Alignment => 1, Address => Sector_Address;

         --  The extended FAT32 BIOS parameter block,
         FAT32_EBPB : FAT32_Extended_BIOS_Parameter_Block
         with
           Import,
           Alignment => 1,
           Address   => Boot_Sector.EBPB_Reserved_Space'Address;

         Total_Clusters    : Natural := 0;
         Total_Tables_Size : Natural := 0;
      begin
         Total_Clusters := Get_Total_Clusters (Boot_Sector);

         Filesystem_Info.FAT_Type := Get_Filesystem_Type (Total_Clusters);

         Filesystem_Info.Root_Directory_Sector_Count :=
           Get_Root_Directory_Sector_Count (Boot_Sector);

         Filesystem_Info.Bytes_Per_Sector :=
           Natural (Boot_Sector.BPB.Bytes_Per_Sector);

         Filesystem_Info.Root_Directory_Buffer_Size :=
           Filesystem_Info.Bytes_Per_Sector
           * Filesystem_Info.Root_Directory_Sector_Count;

         Filesystem_Info.FAT_Sector_Count :=
           Get_FAT_Sector_Count (Boot_Sector);

         Filesystem_Info.First_FAT_Sector :=
           Unsigned_64 (Boot_Sector.BPB.Reserved_Sector_Count);

         Filesystem_Info.FAT_Buffer_Size :=
           Filesystem_Info.FAT_Sector_Count * Filesystem_Info.Bytes_Per_Sector;

         Filesystem_Info.Sectors_Per_Cluster :=
           Natural (Boot_Sector.BPB.Sectors_Per_Cluster);

         Total_Tables_Size :=
           Natural (Boot_Sector.BPB.Table_Count)
           * Natural (Boot_Sector.BPB.Table_Size);

         Filesystem_Info.First_Data_Sector :=
           Unsigned_64 (Boot_Sector.BPB.Reserved_Sector_Count)
           + Unsigned_64 (Filesystem_Info.Root_Directory_Sector_Count)
           + Unsigned_64 (Total_Tables_Size);

         if Filesystem_Info.FAT_Type = FAT32 then
            Filesystem_Info.Root_Directory_Sector :=
              Unsigned_64 (FAT32_EBPB.Root_Cluster);
         else
            Filesystem_Info.Root_Directory_Sector :=
              Filesystem_Info.First_Data_Sector
              - Unsigned_64 (Filesystem_Info.Root_Directory_Sector_Count);
         end if;

      end Parse_Boot_Sector;

      Print_FAT_Filesystem_Info (Filesystem_Info);

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

      Current_Read_Sector :=
        Get_First_Sector_Of_Cluster (Filesystem_Info, Cluster);

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
      Result          : out Function_Result)
   is
      Sector_Number  : Unsigned_64 := 0;
      Sector_Address : Virtual_Address_T := Null_Address;
   begin
      Sector_Number :=
        Filesystem_Info.First_FAT_Sector
        + Unsigned_64
            ((Cluster * 2) / Unsigned_32 (Filesystem_Info.Bytes_Per_Sector));

      Read_Sector_From_Filesystem
        (Filesystem,
         Reading_Process,
         Sector_Number,
         Filesystem_Info.Bytes_Per_Sector,
         Sector_Address,
         Result);
      if Is_Error (Result) then
         FAT_Entry := 0;
         return;
      end if;

      if Filesystem_Info.FAT_Type = FAT16 then
         declare
            Index : Natural := 0;

            FAT16_Table : FAT16_Table_T (0 .. 255)
            with Import, Alignment => 1, Address => Sector_Address;
         begin
            Index :=
              Natural
                ((Cluster * 2)
                 mod Unsigned_32 (Filesystem_Info.Bytes_Per_Sector))
              / 2;

            FAT_Entry := Unsigned_32 (FAT16_Table (Index));
         end;
      else
         Log_Error ("Read_FAT_Entry only supports FAT16 at present.");
         FAT_Entry := 0;
         Result := Invalid_Argument;
         return;
      end if;

      Release_Sector
        (Filesystem, Sector_Number, Filesystem_Info.Bytes_Per_Sector, Result);

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Read_FAT_Entry");
         Result := Constraint_Exception;
   end Read_FAT_Entry;

   procedure Read_FAT16_Root_Directory_Into_Buffer
     (Filesystem             : Filesystem_Access;
      Reading_Process        : in out Process_Control_Block_T;
      Filesystem_Info        : FAT_Filesystem_Info_T;
      Buffer_Virtual_Address : Virtual_Address_T;
      Result                 : out Function_Result)
   is
      Current_Read_Sector : Unsigned_64 := 0;
      Sector_Address      : Virtual_Address_T := Null_Address;

      Destination_Virtual_Address : Virtual_Address_T :=
        Buffer_Virtual_Address;
   begin
      Log_Debug ("Reading FAT16 root directory...", Logging_Tags_FAT);

      Current_Read_Sector := Filesystem_Info.Root_Directory_Sector;

      for Sector_Idx in 1 .. Filesystem_Info.Root_Directory_Sector_Count loop
         --  Read each FAT logical sector into memory.
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

         Release_Sector
           (Filesystem,
            Current_Read_Sector,
            Filesystem_Info.Bytes_Per_Sector,
            Result);

         --  Increment the device read offset.
         Current_Read_Sector := Current_Read_Sector + 1;

         Destination_Virtual_Address :=
           Destination_Virtual_Address
           + Storage_Offset (Filesystem_Info.Bytes_Per_Sector);
      end loop;

      Log_Debug ("Finished Reading FAT16 Root Directory", Logging_Tags_FAT);

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Read_FAT16_Root_Directory_Into_Buffer");
         Result := Constraint_Exception;
   end Read_FAT16_Root_Directory_Into_Buffer;

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
      if not Is_LFN_Entry (Dir_Entry) then
         Result := Invalid_Argument;
         return;
      end if;

      --  The offset into the name is always the sequence number
      --  multiplied by the maximum string length that each
      --  entry holds, which is 13.
      Name_offset := (Natural (LFN_Entry.Sequence.Number) - 1) * 13;

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
         Log_Error ("Constraint error reading LFN");
         Result := Constraint_Exception;
   end Read_LFN_Entry_Filename;

   procedure Search_FAT_Directory_For_File
     (Filesystem      : Filesystem_Access;
      Directory       : Directory_Index_T;
      Filename        : Wide_String;
      Parent_Node     : Filesystem_Node_Access;
      Filesystem_Node : out Filesystem_Node_Access;
      Result          : out Function_Result)
   is
      --  Whether a long file name entry is currently being read.
      Entry_Has_Long_Filename : Boolean := False;
      Entry_Filename          : Filesystem_Node_Path_T :=
        [others => Wide_Character'Val (0)];
      Entry_Filename_Length   : Natural := 0;
   begin
      Log_Debug_Wide
        ("Searching directory for file '" & Filename & "'", Logging_Tags_FAT);

      Filesystem_Node := null;

      for Dir_Idx in 1 .. Directory'Length loop
         Log_Debug
           ("Scanning directory entry: " & Dir_Idx'Image, Logging_Tags_FAT);

         if Is_Last_Directory_Entry (Directory (Dir_Idx)) then
            Log_Debug ("Reached last directory entry.", Logging_Tags_FAT);
            exit;
         end if;

         if not Is_Unused_Entry (Directory (Dir_Idx)) then
            --  If the entry attributes indicate that this is a long
            --  file name entry, then parse it differently.
            if Is_LFN_Entry (Directory (Dir_Idx)) then
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
               if Entry_Has_Long_Filename = False then
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
               --  full DOS directory entry, and can now compare it against the
               --  target filename to see if we've found a match.

               Log_Debug_Wide
                 ("Parsed entry with filename: '"
                  & Entry_Filename (1 .. Entry_Filename_Length)
                  & "'",
                  Logging_Tags_FAT);

               --  Check if the parsed filename matches the target filename.
               if Compare_Node_Name_With_Wide_String
                    (Entry_Filename, Entry_Filename_Length, Filename)
               then
                  --  If we have a match, create a filesystem node cache entry,
                  --  set the node type, and exit.
                  Create_Filesystem_Node_Cache_Entry
                    (Filesystem,
                     Entry_Filename,
                     Filesystem_Node,
                     Result,
                     Size          =>
                       Unsigned_64 (Directory (Dir_Idx).File_Size),
                     Data_Location =>
                       Unsigned_64
                         (Get_First_Cluster_Of_Dir_Entry
                            (Directory (Dir_Idx))),
                     Index         =>
                       Get_Directory_Entry_Node_Index
                         (Get_First_Cluster_Of_Dir_Entry (Directory (Dir_Idx)),
                          Dir_Idx),
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

      if not Is_Valid_Filesystem_Pointer (Filesystem)
        or else Filesystem.all.Filesystem_Type /= Filesystem_Type_FAT
      then
         Log_Error ("Invalid FAT filesystem", Logging_Tags_FAT);
         Result := Invalid_Argument;
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
   begin
      Bytes_Read := 0;

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
         Bytes_Left_To_Read := Bytes_To_Read - Bytes_Read;

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
         if Bytes_Read = Bytes_To_Read then
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

      Result := Success;
   end Validate_FAT_Filesystem;
end Filesystems.FAT;
