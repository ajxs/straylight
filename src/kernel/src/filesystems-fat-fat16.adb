with Filesystems.FAT.DOS_Filenames; use Filesystems.FAT.DOS_Filenames;
with Filesystems.Block_Cache;       use Filesystems.Block_Cache;
with Filesystems.Node_Cache;        use Filesystems.Node_Cache;

package body Filesystems.FAT.FAT16 is
   procedure Create_LFN_Directory_Entry
     (Filename              : Filesystem_Path_T;
      Sequence_Number       : Natural;
      Is_Last_Entry         : Boolean;
      DOS_Filename_Checksum : Unsigned_8;
      LFN_Entry             : out Long_File_Name_Directory_Entry;
      Result                : out Function_Result) is
   begin
      Log_Debug
        ("Creating LFN directory entry with sequence number: "
         & Sequence_Number'Image
         & " and is_last_entry: "
         & Is_Last_Entry'Image,
         Logging_Tags_FAT);

      Filename_Slice_Start : constant Natural := (Sequence_Number - 1) * 13;
      Filename_Slice_Length : constant Natural :=
        (if Filename_Slice_Start + 13 <= Filename'Length
         then 13
         else Filename'Length - Filename_Slice_Start);

      Filename_Slice_End : constant Natural :=
        Filename_Slice_Start + Filename_Slice_Length - 1;

      Name_Slice : constant String (1 .. Filename_Slice_Length) :=
        Filename
          (Filename'First + Filename_Slice_Start
           .. Filename'First + Filename_Slice_End);

      Log_Debug ("Filename slice: '" & Name_Slice & "'", Logging_Tags_FAT);

      --  (from FAT32 File System Specification v1.03)
      --  "Names are also NUL terminated and padded with 0xFFFF characters in
      --  order to detect corruption of long name fields by errant disk
      --  utilities"
      Wide_Name : Wide_String (1 .. 13) :=
        [others => Wide_Character'Val (16#FFFF#)];
      for I in Name_Slice'Range loop
         Wide_Name (I) := ASCII_To_Wide_Char (Name_Slice (I));
      end loop;

      --  "A name that fits exactly in a n long directory entries
      --  (i.e. is an integer multiple of 13) is not NUL terminated and not
      --  padded with 0xFFFFs."
      if Is_Last_Entry and then Filename_Slice_Length < 13 then
         Wide_Name (Filename_Slice_Length + 1) := Wide_Character'Val (0);
      end if;

      LFN_Entry :=
        (Sequence      =>
           (Number     => File_Name_Number_T (Sequence_Number),
            Last_Entry => Is_Last_Entry,
            others     => False),
         Name_1        => Wide_Name (1 .. 5),
         --  Set the attributes to indicate that this is an LFN entry.
         Attributes    =>
           (Read_Only    => True,
            Hidden       => True,
            System_Entry => True,
            Volume_Label => True,
            Directory    => False,
            Archive      => False,
            Device       => False,
            Reserved     => False),
         Entry_Type    => 0,
         Checksum      => DOS_Filename_Checksum,
         Name_2        => Wide_Name (6 .. 11),
         First_Cluster => 0,
         Name_3        => Wide_Name (12 .. 13));

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Create_LFN_Directory_Entry");
         Result := Constraint_Exception;
   end Create_LFN_Directory_Entry;

   procedure Scan_Directory_For_Unused_Entries
     (Directory                        : Directory_Index_T;
      Total_Number_Of_Entries_Required : Natural;
      Total_Entries_Parsed             : Natural;
      Current_Free_Entry_Count         : in out Natural;
      Found_Required_Entries           : in out Boolean;
      First_Free_Entry_Index           : in out Natural;
      Result                           : out Function_Result) is
   begin
      Log_Debug
        ("Scanning for "
         & Total_Number_Of_Entries_Required'Image
         & " free directory entries.",
         Logging_Tags_FAT);

      Parse_Directory_Loop : for Dir_Idx in 0 .. Directory'Length - 1 loop
         if Is_Free_Directory_Entry (Directory (Dir_Idx)) then
            Log_Debug
              ("Found free directory entry at idx:" & Dir_Idx'Image,
               Logging_Tags_FAT);

            if Current_Free_Entry_Count = 0 then
               First_Free_Entry_Index := Dir_Idx + Total_Entries_Parsed;
            end if;

            Current_Free_Entry_Count := @ + 1;
         else
            Current_Free_Entry_Count := 0;
         end if;

         if Current_Free_Entry_Count = Total_Number_Of_Entries_Required then
            Found_Required_Entries := True;

            Log_Debug
              ("Found "
               & Current_Free_Entry_Count'Image
               & " free directory entries starting at index "
               & First_Free_Entry_Index'Image,
               Logging_Tags_FAT);

            exit Parse_Directory_Loop;
         end if;
      end loop Parse_Directory_Loop;

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Scan_Directory_For_Unused_Entries");
         Result := Constraint_Exception;
   end Scan_Directory_For_Unused_Entries;

   procedure Search_Root_Directory_For_Unused_Entries
     (Filesystem                       : Filesystem_Access;
      Reading_Process                  : in out Process_Control_Block_T;
      Filesystem_Info                  : FAT_Filesystem_Info_T;
      Total_Number_Of_Entries_Required : Natural;
      Last_Sector                      : Sector_Index_T;
      Entries_Per_Sector               : Natural;
      First_Free_Entry_Index           : out Natural;
      Result                           : out Function_Result)
   is
      Current_Sector             : Sector_Index_T := 0;
      Block_Address              : Virtual_Address_T := Null_Address;
      Sector_Offset_Within_Block : Storage_Offset := 0;
      Current_Block              : Block_Index_T := 0;

      Current_Free_Entry_Count : Natural := 0;
      Found_Required_Entries   : Boolean := False;
      Total_Entries_Parsed     : Natural := 0;
      Scan_Directory_Result    : Function_Result := Unset;
   begin
      Current_Sector := Filesystem_Info.FAT12_16_Root_Directory_Sector;
      First_Free_Entry_Index := 0;

      Read_Sectors_Loop : loop
         Get_Sector_Block_Number_And_Offset
           (Current_Sector,
            Filesystem_Info.Bytes_Per_Sector,
            Current_Block,
            Sector_Offset_Within_Block,
            Result);
         if Is_Error (Result) then
            return;
         end if;

         Read_Block_From_Filesystem
           (Filesystem, Reading_Process, Current_Block, Block_Address, Result);
         if Is_Error (Result) then
            return;
         end if;

         Remaining_Sectors : constant Natural :=
           Natural (Last_Sector - Current_Sector);

         Number_Of_Sectors_Within_This_Block : constant Natural :=
           Natural'Min
             (Remaining_Sectors,
              (Block_Size - Natural (Sector_Offset_Within_Block))
              / Filesystem_Info.Bytes_Per_Sector);

         Number_Of_Directory_Entries_In_This_Block : constant Natural :=
           Entries_Per_Sector * Number_Of_Sectors_Within_This_Block;

         Directory :
           Directory_Index_T
             (0 .. Number_Of_Directory_Entries_In_This_Block - 1)
         with
           Import,
           Address   => Block_Address + Sector_Offset_Within_Block,
           Alignment => 1;

         Scan_Directory_For_Unused_Entries
           (Directory,
            Total_Number_Of_Entries_Required,
            Total_Entries_Parsed,
            Current_Free_Entry_Count,
            Found_Required_Entries,
            First_Free_Entry_Index,
            Scan_Directory_Result);

         Release_Block (Filesystem, Current_Block, Result);
         if Is_Error (Result) then
            return;
         end if;

         if Is_Error (Scan_Directory_Result) then
            Result := Scan_Directory_Result;
            return;
         end if;

         exit Read_Sectors_Loop when Found_Required_Entries;

         Current_Sector :=
           @ + Sector_Index_T (Number_Of_Sectors_Within_This_Block);

         Total_Entries_Parsed := @ + Number_Of_Directory_Entries_In_This_Block;

         exit Read_Sectors_Loop when Current_Sector >= Last_Sector;
      end loop Read_Sectors_Loop;

      if not Found_Required_Entries then
         Log_Error
           ("Not enough free directory entries found in root directory.");
         Result := No_Free_Entries;
         return;
      end if;

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error
           ("Constraint_Error: Search_Root_Directory_For_Unused_Entries");
         Result := Constraint_Exception;
   end Search_Root_Directory_For_Unused_Entries;

   procedure Create_File_In_Root_Directory_FAT16
     (Filesystem                     : Filesystem_Access;
      Reading_Process                : in out Process_Control_Block_T;
      Filesystem_Info                : FAT_Filesystem_Info_T;
      Filename                       : Filesystem_Path_T;
      DOS_Filename                   : FAT_DOS_File_Name_T;
      DOS_Extension                  : FAT_DOS_File_Ext_T;
      DOS_Checksum                   : Unsigned_8 := 0;
      Number_Of_LFN_Entries_Required : Natural := 0;
      Parent_Node                    : Filesystem_Node_Access;
      New_Node                       : out Filesystem_Node_Access;
      Result                         : out Function_Result)
   is
      Current_Sector             : Sector_Index_T := 0;
      Block_Address              : Virtual_Address_T := Null_Address;
      Sector_Offset_Within_Block : Storage_Offset := 0;
      Current_Block              : Block_Index_T := 0;

      DOS_Entry_Sector          : Sector_Index_T := 0;
      DOS_Entry_Index_In_Sector : Natural := 0;

      First_Free_Entry_Index : Natural := 0;

      Current_Updated_Entry_Count  : Natural := 0;
      All_Required_Entries_Updated : Boolean := False;

      Total_Entries_Parsed : Natural := 0;

      Release_Block_Result : Function_Result := Unset;
   begin
      New_Node := null;

      Total_Number_Of_Entries_Required : constant Natural :=
        Number_Of_LFN_Entries_Required + 1;

      Last_Sector : constant Sector_Index_T :=
        Filesystem_Info.FAT12_16_Root_Directory_Sector
        + Sector_Index_T (Filesystem_Info.Sectors_In_Root_Directory);

      Entries_Per_Sector : constant Natural :=
        Filesystem_Info.Bytes_Per_Sector / 32;

      Search_Root_Directory_For_Unused_Entries
        (Filesystem,
         Reading_Process,
         Filesystem_Info,
         Total_Number_Of_Entries_Required,
         Last_Sector,
         Entries_Per_Sector,
         First_Free_Entry_Index,
         Result);
      if Is_Error (Result) then
         return;
      end if;

      Current_Sector :=
        Filesystem_Info.FAT12_16_Root_Directory_Sector
        + Sector_Index_T (First_Free_Entry_Index / Entries_Per_Sector);

      Total_Entries_Parsed :=
        (First_Free_Entry_Index / Entries_Per_Sector) * Entries_Per_Sector;

      Update_Sectors_Loop : loop
         Get_Sector_Block_Number_And_Offset
           (Current_Sector,
            Filesystem_Info.Bytes_Per_Sector,
            Current_Block,
            Sector_Offset_Within_Block,
            Result);
         if Is_Error (Result) then
            return;
         end if;

         Read_Block_From_Filesystem
           (Filesystem, Reading_Process, Current_Block, Block_Address, Result);
         if Is_Error (Result) then
            return;
         end if;

         Remaining_Sectors : constant Natural :=
           Natural (Last_Sector - Current_Sector);

         Number_Of_Sectors_Within_This_Block : constant Natural :=
           Natural'Min
             (Remaining_Sectors,
              (Block_Size - Natural (Sector_Offset_Within_Block))
              / Filesystem_Info.Bytes_Per_Sector);

         Number_Of_Directory_Entries_In_This_Block : constant Natural :=
           Entries_Per_Sector * Number_Of_Sectors_Within_This_Block;

         Update_Directory_Buffer : declare
            Directory :
              Directory_Index_T
                (0 .. Number_Of_Directory_Entries_In_This_Block - 1)
            with
              Import,
              Address   => Block_Address + Sector_Offset_Within_Block,
              Alignment => 1;

            LFN_Directory :
              LFN_Directory_Index_T
                (0 .. Number_Of_Directory_Entries_In_This_Block - 1)
            with
              Import,
              Address   => Block_Address + Sector_Offset_Within_Block,
              Alignment => 1;
         begin
            Update_Entry_Loop : loop
               All_Required_Entries_Updated :=
                 Current_Updated_Entry_Count
                 = Total_Number_Of_Entries_Required;

               exit Update_Entry_Loop when All_Required_Entries_Updated;

               Next_Free_Entry_Index : constant Natural :=
                 First_Free_Entry_Index + Current_Updated_Entry_Count;

               --  Test whether the next entry we need to write is actually
               --  within the current block.
               Next_Entry_To_Write_Is_In_This_Block : constant Boolean :=
                 Next_Free_Entry_Index
                 in Total_Entries_Parsed
                  ..
                    Total_Entries_Parsed
                    + Number_Of_Directory_Entries_In_This_Block
                    - 1;

               exit Update_Entry_Loop when
                 not Next_Entry_To_Write_Is_In_This_Block;

               Index_Within_Current_Block : constant Natural :=
                 Next_Free_Entry_Index - Total_Entries_Parsed;

               if Current_Updated_Entry_Count < Number_Of_LFN_Entries_Required
               then
                  Create_LFN_Directory_Entry
                    (Filename,
                     Number_Of_LFN_Entries_Required
                     - Current_Updated_Entry_Count,
                     Current_Updated_Entry_Count = 0,
                     DOS_Checksum,
                     LFN_Directory (Index_Within_Current_Block),
                     Result);
                  if Is_Error (Result) then
                     Release_Block
                       (Filesystem, Current_Block, Release_Block_Result);
                     return;
                  end if;

                  Log_Debug
                    ("Wrote LFN entry at:" & Index_Within_Current_Block'Image,
                     Logging_Tags_FAT);
               else
                  Directory (Index_Within_Current_Block) :=
                    (File_Name          => DOS_Filename,
                     File_Ext           => DOS_Extension,
                     Attributes         => (others => False),
                     Reserved           => 0,
                     Creation_Seconds   => 0,
                     Creation_Time      => 0,
                     Creation_Date      => 0,
                     Last_Accessed_Date => 0,
                     First_Cluster_High => 0,
                     Last_Modified_Time => 0,
                     Last_Modified_Date => 0,
                     First_Cluster_Low  => 0,
                     File_Size          => 0);

                  --  Save the sector index of the newly created DOS directory
                  --  entry, and its index within that sector. These are used
                  --  to create the filesystem node index for this entry.
                  DOS_Entry_Sector :=
                    Current_Sector
                    + Sector_Index_T
                        (Index_Within_Current_Block / Entries_Per_Sector);

                  DOS_Entry_Index_In_Sector :=
                    (Index_Within_Current_Block mod Entries_Per_Sector);

                  Log_Debug
                    ("Wrote DOS entry at:" & Index_Within_Current_Block'Image,
                     Logging_Tags_FAT);
               end if;

               Current_Updated_Entry_Count := @ + 1;
            end loop Update_Entry_Loop;

            Write_Block_To_Filesystem
              (Filesystem, Reading_Process, Current_Block, Result);
            if Is_Error (Result) then
               Release_Block (Filesystem, Current_Block, Release_Block_Result);
               return;
            end if;
         end Update_Directory_Buffer;

         Release_Block (Filesystem, Current_Block, Result);
         if Is_Error (Result) then
            return;
         end if;

         exit Update_Sectors_Loop when All_Required_Entries_Updated;

         Total_Entries_Parsed := @ + Number_Of_Directory_Entries_In_This_Block;

         Current_Sector :=
           @ + Sector_Index_T (Number_Of_Sectors_Within_This_Block);

         exit Update_Sectors_Loop when Current_Sector >= Last_Sector;
      end loop Update_Sectors_Loop;

      --  Create the new filesystem node.
      Create_Filesystem_Node_Cache_Entry
        (Filesystem,
         Filename,
         New_Node,
         Result,
         Size          => 0,
         Data_Location => 0,
         Index         =>
           Get_Directory_Entry_Node_Index
             (Unsigned_32 (DOS_Entry_Sector), DOS_Entry_Index_In_Sector),
         Parent_Index  => Parent_Node.all.Index);
      if Is_Error (Result) then
         New_Node := null;
         return;
      end if;

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Create_File_In_Root_Directory_FAT16");
         Result := Constraint_Exception;
   end Create_File_In_Root_Directory_FAT16;

   procedure Create_File_In_Directory_FAT16
     (Filesystem                     : Filesystem_Access;
      Reading_Process                : in out Process_Control_Block_T;
      Filesystem_Info                : FAT_Filesystem_Info_T;
      Filename                       : Filesystem_Path_T;
      DOS_Filename                   : FAT_DOS_File_Name_T;
      DOS_Extension                  : FAT_DOS_File_Ext_T;
      DOS_Checksum                   : Unsigned_8 := 0;
      Number_Of_LFN_Entries_Required : Natural := 0;
      Parent_Node                    : Filesystem_Node_Access;
      New_Node                       : out Filesystem_Node_Access;
      Result                         : out Function_Result)
   is
      Current_Cluster       : Unsigned_16 := 0;
      Next_Cluster_In_Chain : Unsigned_16 := 0;

      DOS_Entry_Sector          : Sector_Index_T := 0;
      DOS_Entry_Index_In_Sector : Natural := 0;

      Block_Address              : Virtual_Address_T := Null_Address;
      Sector_Offset_Within_Block : Storage_Offset := 0;
      Current_Block              : Block_Index_T := 0;

      First_Free_Entry_Index : Natural := 0;

      Current_Free_Entry_Count : Natural := 0;
      Found_Required_Entries   : Boolean := False;
      Total_Entries_Parsed     : Natural := 0;
      Scan_Directory_Result    : Function_Result := Unset;

      Current_Updated_Entry_Count  : Natural := 0;
      All_Required_Entries_Updated : Boolean := False;
      Release_Block_Result         : Function_Result := Unset;
   begin
      New_Node := null;

      Total_Number_Of_Entries_Required : constant Natural :=
        Number_Of_LFN_Entries_Required + 1;

      Directory_Entries_In_Sector : constant Natural :=
        Filesystem_Info.Bytes_Per_Sector / 32;

      --  Perform an initial first pass scan of all the clusters/sectors in
      --  the directory to find the required number of consecutive free
      --  directory entries, and to record the index of the first free entry.
      --  This two-pass implementation is required because of the possibility
      --  that the consecutive free entries may span across multiple
      --  sectors/clusters.
      Current_Cluster := Unsigned_16 (Parent_Node.all.Data_Location);

      if Current_Cluster = 0 then
         Log_Debug
           ("Directory has no clusters allocated to it.", Logging_Tags_FAT);
         Result := No_Free_Entries;
         return;
      end if;

      Follow_Cluster_Chain_Loop : loop
         First_Sector_Of_Cluster : constant Sector_Index_T :=
           Get_First_Sector_Of_Cluster
             (Unsigned_32 (Current_Cluster),
              Filesystem_Info.Sectors_Per_Cluster,
              Filesystem_Info.First_Data_Sector);

         --  According to the FAT32 v1.03 spec there can be 1 .. 128 sectors
         --  in a FAT16 cluster, and the value is always a power of 2.
         --  Bytes per sector is always 512, 1024, 2048 or 4096.
         Read_Sectors_Loop : for I in
           0 .. Filesystem_Info.Sectors_Per_Cluster - 1
         loop
            Current_Sector : constant Sector_Index_T :=
              First_Sector_Of_Cluster + Sector_Index_T (I);

            Get_Sector_Block_Number_And_Offset
              (Current_Sector,
               Filesystem_Info.Bytes_Per_Sector,
               Current_Block,
               Sector_Offset_Within_Block,
               Result);
            if Is_Error (Result) then
               return;
            end if;

            Read_Block_From_Filesystem
              (Filesystem,
               Reading_Process,
               Current_Block,
               Block_Address,
               Result);
            if Is_Error (Result) then
               return;
            end if;

            Directory :
              Directory_Index_T (0 .. Directory_Entries_In_Sector - 1)
            with
              Import,
              Address   => Block_Address + Sector_Offset_Within_Block,
              Alignment => 1;

            Scan_Directory_For_Unused_Entries
              (Directory,
               Total_Number_Of_Entries_Required,
               Total_Entries_Parsed,
               Current_Free_Entry_Count,
               Found_Required_Entries,
               First_Free_Entry_Index,
               Scan_Directory_Result);

            Release_Block (Filesystem, Current_Block, Result);
            if Is_Error (Result) then
               return;
            end if;

            if Is_Error (Scan_Directory_Result) then
               Result := Scan_Directory_Result;
               return;
            end if;

            Total_Entries_Parsed := @ + Directory_Entries_In_Sector;

            exit Follow_Cluster_Chain_Loop when Found_Required_Entries;
         end loop Read_Sectors_Loop;

         --  Read the next cluster in the FAT chain.
         Read_FAT16_Entry
           (Filesystem,
            Reading_Process,
            Filesystem_Info,
            Current_Cluster,
            Next_Cluster_In_Chain,
            Result);
         if Is_Error (Result) then
            return;
         end if;

         if Is_Cluster_End_Of_Chain
              (Unsigned_32 (Next_Cluster_In_Chain), Filesystem_Info.FAT_Type)
         then
            Log_Debug
              ("Reached end of cluster chain while scanning for "
               & "free directory entries. Allocating next cluster.",
               Logging_Tags_FAT);

            Extend_Cluster_Chain
              (Filesystem,
               Reading_Process,
               Filesystem_Info,
               Unsigned_32 (Current_Cluster),
               Unsigned_32 (Next_Cluster_In_Chain),
               Result,
               Zero_New_Cluster => True);
            if Is_Error (Result) then
               return;
            end if;
         end if;

         --  Check for end of cluster chain.
         exit Follow_Cluster_Chain_Loop when
           Is_Cluster_End_Of_Chain
             (Unsigned_32 (Next_Cluster_In_Chain), Filesystem_Info.FAT_Type);

         Current_Cluster := Next_Cluster_In_Chain;
      end loop Follow_Cluster_Chain_Loop;

      if not Found_Required_Entries then
         Log_Error ("Not enough free directory entries found in directory.");
         Result := No_Free_Entries;
         return;
      end if;

      --  We now have the start index of the free directory entries to update.
      --  perform a second pass to update all the required clusters/sectors.
      Current_Cluster := Unsigned_16 (Parent_Node.all.Data_Location);

      Total_Entries_Parsed := 0;

      Update_Cluster_Chain_Loop : loop
         First_Sector_Of_Cluster : constant Sector_Index_T :=
           Get_First_Sector_Of_Cluster
             (Unsigned_32 (Current_Cluster),
              Filesystem_Info.Sectors_Per_Cluster,
              Filesystem_Info.First_Data_Sector);

         Update_Sectors_Loop : for I in
           0 .. Filesystem_Info.Sectors_Per_Cluster - 1
         loop
            Sector_Last_Entry_Index : constant Natural :=
              Total_Entries_Parsed + Directory_Entries_In_Sector - 1;

            --  Test whether the sector we're about to read contains the
            --  next free directory entry to update. If not, skip reading the
            --  sector entirely and move on.
            if Sector_Last_Entry_Index >= First_Free_Entry_Index then
               Current_Sector : constant Sector_Index_T :=
                 First_Sector_Of_Cluster + Sector_Index_T (I);

               Get_Sector_Block_Number_And_Offset
                 (Current_Sector,
                  Filesystem_Info.Bytes_Per_Sector,
                  Current_Block,
                  Sector_Offset_Within_Block,
                  Result);
               if Is_Error (Result) then
                  return;
               end if;

               Read_Block_From_Filesystem
                 (Filesystem,
                  Reading_Process,
                  Current_Block,
                  Block_Address,
                  Result);
               if Is_Error (Result) then
                  return;
               end if;

               Directory :
                 Directory_Index_T (0 .. Directory_Entries_In_Sector - 1)
               with
                 Import,
                 Address   => Block_Address + Sector_Offset_Within_Block,
                 Alignment => 1;

               LFN_Directory :
                 LFN_Directory_Index_T (0 .. Directory_Entries_In_Sector - 1)
               with
                 Import,
                 Address   => Block_Address + Sector_Offset_Within_Block,
                 Alignment => 1;

               Update_Entry_Loop : loop
                  All_Required_Entries_Updated :=
                    Current_Updated_Entry_Count
                    = Total_Number_Of_Entries_Required;

                  exit Update_Entry_Loop when All_Required_Entries_Updated;

                  Next_Free_Entry_Index : constant Natural :=
                    First_Free_Entry_Index + Current_Updated_Entry_Count;

                  --  Test whether the next entry we need to write is
                  --  actually within the current sector.
                  Next_Entry_Is_In_This_Sector : constant Boolean :=
                    Next_Free_Entry_Index
                    in Total_Entries_Parsed
                     .. Total_Entries_Parsed + Directory_Entries_In_Sector - 1;

                  exit Update_Entry_Loop when not Next_Entry_Is_In_This_Sector;

                  Index_Within_Curr_Sector : constant Natural :=
                    Next_Free_Entry_Index - Total_Entries_Parsed;

                  if Current_Updated_Entry_Count
                    < Number_Of_LFN_Entries_Required
                  then
                     Create_LFN_Directory_Entry
                       (Filename,
                        Number_Of_LFN_Entries_Required
                        - Current_Updated_Entry_Count,
                        Current_Updated_Entry_Count = 0,
                        DOS_Checksum,
                        LFN_Directory (Index_Within_Curr_Sector),
                        Result);
                     if Is_Error (Result) then
                        Release_Block
                          (Filesystem, Current_Block, Release_Block_Result);
                        return;
                     end if;

                     Log_Debug
                       ("Wrote LFN entry at:" & Index_Within_Curr_Sector'Image,
                        Logging_Tags_FAT);
                  else
                     Directory (Index_Within_Curr_Sector) :=
                       (File_Name          => DOS_Filename,
                        File_Ext           => DOS_Extension,
                        Attributes         => (others => False),
                        Reserved           => 0,
                        Creation_Seconds   => 0,
                        Creation_Time      => 0,
                        Creation_Date      => 0,
                        Last_Accessed_Date => 0,
                        First_Cluster_High => 0,
                        Last_Modified_Time => 0,
                        Last_Modified_Date => 0,
                        First_Cluster_Low  => 0,
                        File_Size          => 0);

                     DOS_Entry_Sector := Current_Sector;

                     DOS_Entry_Index_In_Sector := Index_Within_Curr_Sector;

                     Log_Debug
                       ("Wrote DOS entry at:" & Index_Within_Curr_Sector'Image,
                        Logging_Tags_FAT);
                  end if;

                  Current_Updated_Entry_Count := @ + 1;
               end loop Update_Entry_Loop;

               Write_Block_To_Filesystem_And_Release
                 (Filesystem, Reading_Process, Current_Block, Result);
               if Is_Error (Result) then
                  return;
               end if;
            end if;

            Total_Entries_Parsed := @ + Directory_Entries_In_Sector;

            exit Update_Cluster_Chain_Loop when All_Required_Entries_Updated;
         end loop Update_Sectors_Loop;

         --  Read the next cluster in the FAT chain.
         Read_FAT16_Entry
           (Filesystem,
            Reading_Process,
            Filesystem_Info,
            Current_Cluster,
            Next_Cluster_In_Chain,
            Result);
         if Is_Error (Result) then
            return;
         end if;

         --  Check for end of cluster chain.
         exit Update_Cluster_Chain_Loop when
           Is_Cluster_End_Of_Chain
             (Unsigned_32 (Next_Cluster_In_Chain), Filesystem_Info.FAT_Type);

         Current_Cluster := Next_Cluster_In_Chain;
      end loop Update_Cluster_Chain_Loop;

      --  Create the new filesystem node.
      Create_Filesystem_Node_Cache_Entry
        (Filesystem,
         Filename,
         New_Node,
         Result,
         Size          => 0,
         Data_Location => 0,
         Index         =>
           Get_Directory_Entry_Node_Index
             (Unsigned_32 (DOS_Entry_Sector), DOS_Entry_Index_In_Sector),
         Parent_Index  => Parent_Node.all.Index);
      if Is_Error (Result) then
         New_Node := null;
         return;
      end if;

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Create_File_In_Directory_FAT16");
         Result := Constraint_Exception;
   end Create_File_In_Directory_FAT16;

   procedure Create_File_FAT16
     (Filesystem      : Filesystem_Access;
      Reading_Process : in out Process_Control_Block_T;
      Filesystem_Info : FAT_Filesystem_Info_T;
      Filename        : Filesystem_Path_T;
      Parent_Node     : Filesystem_Node_Access;
      New_Node        : out Filesystem_Node_Access;
      Result          : out Function_Result)
   is
      DOS_Filename                   : FAT_DOS_File_Name_T;
      DOS_Extension                  : FAT_DOS_File_Ext_T;
      DOS_Checksum                   : Unsigned_8 := 0;
      Are_LFN_Entries_Required       : Boolean := False;
      Number_Of_LFN_Entries_Required : Natural := 0;
   begin
      --  Create the DOS filename/extension up-front, so we can determine
      --  whether LFN entries are needed for this filename.
      --  If the conversion is lossy, we need LFN entries to preserve the
      --  original filename. Note that a change in case isn't considered lossy,
      --  since FAT compares filenames in a case-insensitive manner.
      Create_DOS_Filename
        (Filename,
         DOS_Filename,
         DOS_Extension,
         Are_LFN_Entries_Required,
         Result);
      if Is_Error (Result) then
         return;
      end if;

      if Are_LFN_Entries_Required then
         Number_Of_LFN_Entries_Required :=
           (Filename'Length / 13)
           + (if Filename'Length mod 13 = 0 then 0 else 1);

         DOS_Checksum :=
           Get_DOS_Filename_Checksum (DOS_Filename, DOS_Extension);
      end if;

      if Parent_Node.all.Node_Type = Filesystem_Node_Type_Mounted_Filesystem
      then
         Create_File_In_Root_Directory_FAT16
           (Filesystem,
            Reading_Process,
            Filesystem_Info,
            Filename,
            DOS_Filename,
            DOS_Extension,
            DOS_Checksum,
            Number_Of_LFN_Entries_Required,
            Parent_Node,
            New_Node,
            Result);
      else
         Create_File_In_Directory_FAT16
           (Filesystem,
            Reading_Process,
            Filesystem_Info,
            Filename,
            DOS_Filename,
            DOS_Extension,
            DOS_Checksum,
            Number_Of_LFN_Entries_Required,
            Parent_Node,
            New_Node,
            Result);
      end if;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Create_File_FAT16");
         New_Node := null;
         Result := Constraint_Exception;
   end Create_File_FAT16;

   procedure Find_File_FAT16
     (Filesystem      : Filesystem_Access;
      Reading_Process : in out Process_Control_Block_T;
      Filesystem_Info : FAT_Filesystem_Info_T;
      Filename        : Filesystem_Path_T;
      Parent_Node     : Filesystem_Node_Access;
      Found_Node      : out Filesystem_Node_Access;
      Result          : out Function_Result) is
   begin
      if Parent_Node.all.Node_Type = Filesystem_Node_Type_Mounted_Filesystem
      then
         Find_File_In_FAT16_Root_Directory
           (Filesystem,
            Reading_Process,
            Filesystem_Info,
            Filename,
            Parent_Node,
            Found_Node,
            Result);
      else
         Find_File_In_FAT16_Directory
           (Filesystem,
            Reading_Process,
            Filesystem_Info,
            Filename,
            Parent_Node,
            Found_Node,
            Result);
      end if;

      if Is_Error (Result) then
         Found_Node := null;
         return;
      end if;

   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Find_File_FAT16");
         Found_Node := null;
         Result := Constraint_Exception;
   end Find_File_FAT16;

   procedure Find_File_In_FAT16_Directory
     (Filesystem      : Filesystem_Access;
      Reading_Process : in out Process_Control_Block_T;
      Filesystem_Info : FAT_Filesystem_Info_T;
      Filename        : Filesystem_Path_T;
      Parent_Node     : Filesystem_Node_Access;
      Filesystem_Node : out Filesystem_Node_Access;
      Result          : out Function_Result)
   is
      Current_Cluster       : Unsigned_16 := 0;
      Next_Cluster_In_Chain : Unsigned_16 := 0;

      Block_Address              : Virtual_Address_T := Null_Address;
      Sector_Offset_Within_Block : Storage_Offset := 0;
      Current_Block              : Block_Index_T := 0;

      Search_Result      : Function_Result := Unset;
      Last_Entry_Reached : Boolean := False;

      --  The UCS-2-encoded filename read from the current directory entry.
      Entry_Long_Filename          :
        Wide_String (1 .. Filesystem_Node_Name_Max_Byte_Length) :=
          [others => Wide_Character'Val (0)];
      Entry_Long_Filename_Length   : Natural := 0;
      Entry_Long_Filename_Checksum : Unsigned_8 := 0;
      Checksum_Valid               : Boolean := False;
   begin
      Log_Debug ("Finding file in FAT16 directory", Logging_Tags_FAT);

      Directory_Entries_In_Sector : constant Natural :=
        Filesystem_Info.Bytes_Per_Sector / 32;

      Current_Cluster := Unsigned_16 (Parent_Node.all.Data_Location);

      Follow_Cluster_Chain_Loop : loop
         First_Sector_Of_Cluster : constant Sector_Index_T :=
           Get_First_Sector_Of_Cluster
             (Unsigned_32 (Current_Cluster),
              Filesystem_Info.Sectors_Per_Cluster,
              Filesystem_Info.First_Data_Sector);

         --  According to the FAT32 v1.03 spec there can be 1 .. 128 sectors
         --  in a FAT16 cluster, and the value is always a power of 2.
         --  Bytes per sector is always 512, 1024, 2048 or 4096.
         Read_Sectors_Loop : for I in
           0 .. Filesystem_Info.Sectors_Per_Cluster - 1
         loop
            Current_Sector : constant Sector_Index_T :=
              First_Sector_Of_Cluster + Sector_Index_T (I);

            Get_Sector_Block_Number_And_Offset
              (Current_Sector,
               Filesystem_Info.Bytes_Per_Sector,
               Current_Block,
               Sector_Offset_Within_Block,
               Result);
            if Is_Error (Result) then
               return;
            end if;

            Read_Block_From_Filesystem
              (Filesystem,
               Reading_Process,
               Current_Block,
               Block_Address,
               Result);
            if Is_Error (Result) then
               return;
            end if;

            Directory :
              Directory_Index_T (0 .. Directory_Entries_In_Sector - 1)
            with
              Import,
              Address   => Block_Address + Sector_Offset_Within_Block,
              Alignment => 1;

            Search_FAT_Directory_For_File
              (Filesystem,
               Filesystem_Info,
               Directory,
               Filename,
               Parent_Node,
               Current_Sector,
               Entry_Long_Filename,
               Entry_Long_Filename_Length,
               Entry_Long_Filename_Checksum,
               Checksum_Valid,
               Filesystem_Node,
               Last_Entry_Reached,
               Search_Result);

            Release_Block (Filesystem, Current_Block, Result);
            if Is_Error (Result) then
               return;
            end if;

            Result := Search_Result;

            if Is_Error (Result) then
               return;
            elsif Result = Success then
               Log_Debug ("Found matching file entry.", Logging_Tags_FAT);
               return;
            elsif Last_Entry_Reached then
               Log_Debug
                 ("Last entry in directory reached.", Logging_Tags_FAT);
               exit Follow_Cluster_Chain_Loop;
            end if;
         end loop Read_Sectors_Loop;

         --  Check for end of cluster chain.
         exit Follow_Cluster_Chain_Loop when
           Is_Cluster_End_Of_Chain
             (Unsigned_32 (Current_Cluster), Filesystem_Info.FAT_Type);

         --  Read the next cluster in the FAT chain.
         Read_FAT16_Entry
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
      end loop Follow_Cluster_Chain_Loop;

      Log_Debug ("File not found in FAT directory.", Logging_Tags_FAT);
      Result := File_Not_Found;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Find_File_In_FAT16_Directory");
         Result := Constraint_Exception;
   end Find_File_In_FAT16_Directory;

   procedure Find_File_In_FAT16_Root_Directory
     (Filesystem      : Filesystem_Access;
      Reading_Process : in out Process_Control_Block_T;
      Filesystem_Info : FAT_Filesystem_Info_T;
      Filename        : Filesystem_Path_T;
      Parent_Node     : Filesystem_Node_Access;
      Filesystem_Node : out Filesystem_Node_Access;
      Result          : out Function_Result)
   is
      Search_Result      : Function_Result := Unset;
      Last_Entry_Reached : Boolean := False;

      --  The UCS-2-encoded filename read from the current directory entry.
      Entry_Long_Filename          :
        Wide_String (1 .. Filesystem_Node_Name_Max_Byte_Length) :=
          [others => Wide_Character'Val (0)];
      Entry_Long_Filename_Length   : Natural := 0;
      Entry_Long_Filename_Checksum : Unsigned_8 := 0;
      Checksum_Valid               : Boolean := False;

      Block_Address              : Virtual_Address_T := Null_Address;
      Current_Sector             : Sector_Index_T := 0;
      Current_Block              : Block_Index_T := 0;
      Sector_Offset_Within_Block : Storage_Offset := 0;
   begin
      Last_Sector : constant Sector_Index_T :=
        Filesystem_Info.FAT12_16_Root_Directory_Sector
        + Sector_Index_T (Filesystem_Info.Sectors_In_Root_Directory);

      Current_Sector := Filesystem_Info.FAT12_16_Root_Directory_Sector;

      Read_Sectors_Loop : loop
         Get_Sector_Block_Number_And_Offset
           (Current_Sector,
            Filesystem_Info.Bytes_Per_Sector,
            Current_Block,
            Sector_Offset_Within_Block,
            Result);
         if Is_Error (Result) then
            return;
         end if;

         Read_Block_From_Filesystem
           (Filesystem, Reading_Process, Current_Block, Block_Address, Result);
         if Is_Error (Result) then
            return;
         end if;

         Remaining_Sectors : constant Natural :=
           Natural (Last_Sector - Current_Sector);

         --  We need to take into account that the starting sector of the root
         --  directory may not be block-aligned. e.g.
         --  If the root directory starts at sector 33, and the sector size is
         --  512 bytes and the block size is 4096 bytes, then the first block
         --  read will contain sectors 33 - 40 (7 sectors), and the directory
         --  entries for the root directory will start at an offset of 512
         --  bytes into that block.
         Sectors_Within_This_Block : constant Natural :=
           Natural'Min
             (Natural (Block_Size - Sector_Offset_Within_Block)
              / Filesystem_Info.Bytes_Per_Sector,
              Remaining_Sectors);

         Directory_Entries_In_This_Block : constant Natural :=
           (Sectors_Within_This_Block * Filesystem_Info.Bytes_Per_Sector) / 32;

         Directory :
           Directory_Index_T (0 .. Directory_Entries_In_This_Block - 1)
         with
           Import,
           Address   => Block_Address + Sector_Offset_Within_Block,
           Alignment => 1;

         Search_FAT_Directory_For_File
           (Filesystem,
            Filesystem_Info,
            Directory,
            Filename,
            Parent_Node,
            Current_Sector,
            Entry_Long_Filename,
            Entry_Long_Filename_Length,
            Entry_Long_Filename_Checksum,
            Checksum_Valid,
            Filesystem_Node,
            Last_Entry_Reached,
            Search_Result);

         Release_Block (Filesystem, Current_Block, Result);
         if Is_Error (Result) then
            return;
         end if;

         Result := Search_Result;

         if Is_Error (Result) then
            return;
         elsif Result = Success then
            Log_Debug ("Found matching file entry.", Logging_Tags_FAT);
            return;
         elsif Last_Entry_Reached then
            Log_Debug ("Last entry in directory reached.", Logging_Tags_FAT);
            exit Read_Sectors_Loop;
         end if;

         Current_Sector :=
           Current_Sector + Sector_Index_T (Sectors_Within_This_Block);

         exit Read_Sectors_Loop when Current_Sector >= Last_Sector;
      end loop Read_Sectors_Loop;

      Result := File_Not_Found;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Find_File_In_FAT16_Root_Directory");
         Result := Constraint_Exception;
   end Find_File_In_FAT16_Root_Directory;

   procedure Get_FAT16_Table_Entry_Block_Number_And_Sector_Offset
     (Filesystem_Info            : FAT_Filesystem_Info_T;
      FAT_Index                  : Natural;
      Cluster                    : Unsigned_16;
      Block_Number               : out Unsigned_64;
      Sector_Offset_Within_Block : out Storage_Offset;
      Result                     : out Function_Result)
   is
      Sector_Number : Sector_Index_T := 0;
   begin
      Get_FAT16_Table_Entry_Sector_Number
        (Filesystem_Info, FAT_Index, Cluster, Sector_Number, Result);
      if Is_Error (Result) then
         Block_Number := 0;
         Sector_Offset_Within_Block := 0;
         return;
      end if;

      Get_Sector_Block_Number_And_Offset
        (Sector_Number,
         Filesystem_Info.Bytes_Per_Sector,
         Block_Number,
         Sector_Offset_Within_Block,
         Result);
      if Is_Error (Result) then
         return;
      end if;

      Result := Success;
   end Get_FAT16_Table_Entry_Block_Number_And_Sector_Offset;

   procedure Get_FAT16_Table_Entry_Sector_Number
     (Filesystem_Info : FAT_Filesystem_Info_T;
      FAT_Index       : Natural;
      Cluster         : Unsigned_16;
      Sector_Number   : out Sector_Index_T;
      Result          : out Function_Result) is
   begin
      --  The number of sectors to reach the start of the specified FAT table.
      FAT_Table_Offset : constant Sector_Index_T :=
        Sector_Index_T (FAT_Index * Filesystem_Info.Sectors_In_FAT_Table);

      FAT_Sector_Offset : constant Sector_Index_T :=
        Sector_Index_T
          (Positive (Cluster) * 2 / Filesystem_Info.Bytes_Per_Sector);

      Sector_Number :=
        Filesystem_Info.First_FAT_Sector
        + FAT_Table_Offset
        + FAT_Sector_Offset;

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Get_FAT16_Table_Entry_Sector_Number");
         Sector_Number := 0;
         Result := Constraint_Exception;
   end Get_FAT16_Table_Entry_Sector_Number;

   procedure Read_FAT16_Entry
     (Filesystem      : Filesystem_Access;
      Reading_Process : in out Process_Control_Block_T;
      Filesystem_Info : FAT_Filesystem_Info_T;
      Cluster         : Unsigned_16;
      FAT_Entry       : out Unsigned_16;
      Result          : out Function_Result)
   is
      Block_Address              : Virtual_Address_T := Null_Address;
      Block_Number               : Block_Index_T := 0;
      Sector_Offset_Within_Block : Storage_Offset := 0;
   begin
      --  Uses the primary FAT table (index 0) to read the FAT entry,
      --  since all FAT tables should be identical.
      Get_FAT16_Table_Entry_Block_Number_And_Sector_Offset
        (Filesystem_Info,
         0,
         Cluster,
         Block_Number,
         Sector_Offset_Within_Block,
         Result);
      if Is_Error (Result) then
         FAT_Entry := 0;
         return;
      end if;

      Read_Block_From_Filesystem
        (Filesystem, Reading_Process, Block_Number, Block_Address, Result);
      if Is_Error (Result) then
         FAT_Entry := 0;
         return;
      end if;

      Cluster_Index_Within_Sector : constant Natural :=
        Natural
          (Cluster mod Unsigned_16 (Filesystem_Info.Bytes_Per_Sector / 2));

      Number_Of_Entries_Within_Sector : constant Natural :=
        Natural (Filesystem_Info.Bytes_Per_Sector / 2);

      --  Despite the fact that we're loading an entire block, since the
      --  filesystem reasons about its layout in terms of variable-size
      --  'sectors' we only want to work with a single sector, since we
      --  can't guarantee its size.
      FAT16_Table : FAT16_Table_T (0 .. Number_Of_Entries_Within_Sector - 1)
      with
        Import,
        Alignment => 1,
        Address   => Block_Address + Sector_Offset_Within_Block;

      FAT_Entry := FAT16_Table (Cluster_Index_Within_Sector);

      --  Result set by this call.
      Release_Block (Filesystem, Block_Number, Result);
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Read_FAT16_Entry");
         FAT_Entry := 0;
         Result := Constraint_Exception;
   end Read_FAT16_Entry;

   procedure Write_FAT16_Entry
     (Filesystem      : Filesystem_Access;
      Writing_Process : in out Process_Control_Block_T;
      Filesystem_Info : FAT_Filesystem_Info_T;
      Cluster         : Unsigned_16;
      FAT_Entry       : Unsigned_16;
      Result          : out Function_Result)
   is
      Block_Address              : Virtual_Address_T := Null_Address;
      Block_Number               : Block_Index_T := 0;
      Sector_Offset_Within_Block : Storage_Offset := 0;
   begin
      Number_Of_Entries_Within_Sector : constant Natural :=
        Natural (Filesystem_Info.Bytes_Per_Sector / 2);

      Cluster_Index_Within_Sector : constant Natural :=
        Natural
          (Cluster mod Unsigned_16 (Filesystem_Info.Bytes_Per_Sector / 2));

      for FAT_Table_Idx in 0 .. Filesystem_Info.FAT_Table_Count - 1 loop
         Get_FAT16_Table_Entry_Block_Number_And_Sector_Offset
           (Filesystem_Info,
            FAT_Table_Idx,
            Cluster,
            Block_Number,
            Sector_Offset_Within_Block,
            Result);
         if Is_Error (Result) then
            return;
         end if;

         Read_Block_From_Filesystem
           (Filesystem, Writing_Process, Block_Number, Block_Address, Result);
         if Is_Error (Result) then
            return;
         end if;

         FAT16_Table : FAT16_Table_T (0 .. Number_Of_Entries_Within_Sector - 1)
         with
           Import,
           Alignment => 1,
           Address   => Block_Address + Sector_Offset_Within_Block;

         FAT16_Table (Cluster_Index_Within_Sector) := FAT_Entry;

         Write_Block_To_Filesystem_And_Release
           (Filesystem, Writing_Process, Block_Number, Result);
         if Is_Error (Result) then
            return;
         end if;
      end loop;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Write_FAT16_Entry");
         Result := Constraint_Exception;
   end Write_FAT16_Entry;

   procedure Find_Free_Cluster_FAT16
     (Filesystem      : Filesystem_Access;
      Writing_Process : in out Process_Control_Block_T;
      Filesystem_Info : FAT_Filesystem_Info_T;
      Free_Cluster    : out Unsigned_16;
      Result          : out Function_Result)
   is
      Block_Address              : Virtual_Address_T := Null_Address;
      Sector_Offset_Within_Block : Storage_Offset := 0;
      Current_Block              : Block_Index_T := 0;
      Found_Free_Cluster         : Boolean := False;
   begin
      Clusters_Per_Sector : constant Integer :=
        (Filesystem_Info.Bytes_Per_Sector / 2);

      Last_FAT_Sector_Number : constant Sector_Index_T :=
        Sector_Index_T
          (Filesystem_Info.First_FAT_Sector
           + Sector_Index_T (Filesystem_Info.Sectors_In_FAT_Table)
           - 1);

      --  Loop through the FAT sectors starting from the sector containing
      --  the starting cluster's FAT entry, and read each sector into a buffer.
      --  For each sector, loop through the FAT entries contained in the sector
      --  to see if any of them are free.
      Read_Sectors_Loop : for Sector_Number in
        Filesystem_Info.First_FAT_Sector .. Last_FAT_Sector_Number
      loop
         Get_Sector_Block_Number_And_Offset
           (Sector_Number,
            Filesystem_Info.Bytes_Per_Sector,
            Current_Block,
            Sector_Offset_Within_Block,
            Result);
         if Is_Error (Result) then
            Free_Cluster := 0;
            return;
         end if;

         Read_Block_From_Filesystem
           (Filesystem, Writing_Process, Current_Block, Block_Address, Result);
         if Is_Error (Result) then
            Free_Cluster := 0;
            return;
         end if;

         --  Check all of the FAT entries in the sector for a free cluster.
         --  If the current sector is the starting sector, we need to start
         --  checking from index 2, rather than from the index 0, which
         --  contains reserved entries.
         Start_Index : constant Integer :=
           (if Sector_Number = Filesystem_Info.First_FAT_Sector then 2 else 0);

         FAT16_Table_Sector : FAT16_Table_T (0 .. Clusters_Per_Sector - 1)
         with
           Import,
           Alignment => 1,
           Address   => Block_Address + Sector_Offset_Within_Block;

         Check_Cluster_Loop : for Current_Cluster in
           Start_Index .. Clusters_Per_Sector - 1
         loop
            if Is_Cluster_Free
                 (Unsigned_32 (FAT16_Table_Sector (Current_Cluster)))
            then
               Found_Free_Cluster := True;
               Free_Cluster :=
                 Unsigned_16 (Sector_Number - Filesystem_Info.First_FAT_Sector)
                 * Unsigned_16 (Clusters_Per_Sector)
                 + Unsigned_16 (Current_Cluster);

               exit Check_Cluster_Loop;
            end if;
         end loop Check_Cluster_Loop;

         Release_Block (Filesystem, Current_Block, Result);
         if Is_Error (Result) then
            Free_Cluster := 0;
            return;
         end if;

         if Found_Free_Cluster then
            Log_Debug
              ("Found free FAT16 cluster: " & Free_Cluster'Image,
               Logging_Tags_FAT);

            Result := Success;
            return;
         end if;
      end loop Read_Sectors_Loop;

      Free_Cluster := 0;
      Result := No_Free_Clusters;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Find_Free_Cluster_FAT16");
         Free_Cluster := 0;
         Result := Constraint_Exception;
   end Find_Free_Cluster_FAT16;

end Filesystems.FAT.FAT16;
