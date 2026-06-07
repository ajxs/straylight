with Filesystems.FAT.DOS_Filenames; use Filesystems.FAT.DOS_Filenames;
with Filesystems.Block_Cache;       use Filesystems.Block_Cache;

package body Filesystems.FAT.FAT16 is
   procedure Create_DOS_Directory_Entry
     (Filename              : Filesystem_Path_T;
      Directory_Entry       : out FAT_Directory_Entry_T;
      DOS_Filename_Checksum : out Unsigned_8;
      Result                : out Function_Result)
   is
      Conversion_Is_Lossy : Boolean := False;
   begin
      Directory_Entry :=
        (File_Name          => [others => ' '],
         File_Ext           => [others => ' '],
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

      --  @TODO: After creating the DOS filename, if the conversion was lossy
      --  we should check if the resulting DOS filename collides with any
      --  existing DOS filenames in the same directory.
      Create_DOS_Filename
        (Filename,
         Directory_Entry.File_Name,
         Directory_Entry.File_Ext,
         Conversion_Is_Lossy,
         Result);
      if Is_Error (Result) then
         DOS_Filename_Checksum := 0;
         return;
      end if;

      DOS_Filename_Checksum :=
        Get_DOS_Filename_Checksum
          (Directory_Entry.File_Name, Directory_Entry.File_Ext);

      Log_Debug ("Conversion: " & Conversion_Is_Lossy'Image, Logging_Tags_FAT);

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Create_DOS_Directory_Entry");
         DOS_Filename_Checksum := 0;
         Result := Constraint_Exception;
   end Create_DOS_Directory_Entry;

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

   procedure Get_Number_Of_LFN_Entries_Needed_For_Filename
     (Filename                     : Filesystem_Path_T;
      Number_Of_LFN_Entries_Needed : out Natural;
      Result                       : out Function_Result) is
   begin
      Number_Of_LFN_Entries_Needed :=
        (Filename'Length / 13) + (if Filename'Length mod 13 = 0 then 0 else 1);
      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error
           ("Constraint_Error: Get_Number_Of_LFN_Entries_Needed_For_Filename");
         Number_Of_LFN_Entries_Needed := 0;
         Result := Constraint_Exception;
   end Get_Number_Of_LFN_Entries_Needed_For_Filename;

   procedure Create_Long_File_Name_Entries
     (Filename                 : Filesystem_Path_T;
      Directory_Sector_Address : Virtual_Address_T;
      Number_Of_Entries_Needed : Natural;
      Starting_Index           : Natural;
      DOS_Filename_Checksum    : Unsigned_8;
      Result                   : out Function_Result) is
   begin
      Parse_LFN_Directory_Buffer : declare
         LFN_Directory_Buffer : LFN_Directory_Index_T (1 .. 16)
         with Import, Address => Directory_Sector_Address, Alignment => 1;
      begin
         for I in 0 .. Number_Of_Entries_Needed - 1 loop
            Create_LFN_Directory_Entry
              (Filename,
               Number_Of_Entries_Needed - I,
               I = 0,
               DOS_Filename_Checksum,
               LFN_Directory_Buffer (Starting_Index + I),
               Result);
            if Is_Error (Result) then
               return;
            end if;
         end loop;
      end Parse_LFN_Directory_Buffer;

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Create_Long_File_Name_Entries");
         Result := Constraint_Exception;
   end Create_Long_File_Name_Entries;

   function Are_LFN_Entries_Required
     (Filename : Filesystem_Path_T) return Boolean
   is
      Last_Dot_Index : Integer := 0;
   begin
      --  If any character in the filename is lowercase, we need LFN entries.
      --  Technically, Windows NT supports mixed-case DOS entries, but for
      --  the sake of simplicity this driver only uses uppercase characters
      --  in DOS filename entries.
      for I in Filename'Range loop
         if Filename (I) in 'a' .. 'z' then
            return True;
         end if;
      end loop;

      --  Find the last dot instance in the filename.
      for I in reverse Filename'Range loop
         if Filename (I) = '.' then
            Last_Dot_Index := I;
            exit;
         end if;
      end loop;

      if Last_Dot_Index = 0 then
         return Filename'Length > 8;
      end if;

      if Last_Dot_Index - Filename'First > 8 then
         return True;
      end if;

      if Filename'Last - Last_Dot_Index > 3 then
         return True;
      end if;

      return False;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Are_LFN_Entries_Required");
         return False;
   end Are_LFN_Entries_Required;

   procedure Create_File_In_Root_Directory_FAT16
     (Filesystem      : Filesystem_Access;
      Reading_Process : in out Process_Control_Block_T;
      Filesystem_Info : FAT_Filesystem_Info_T;
      Filename        : Filesystem_Path_T;
      Parent_Node     : Filesystem_Node_Access;
      New_Node        : out Filesystem_Node_Access;
      Result          : out Function_Result)
   is
      pragma Unreferenced (Parent_Node, New_Node);

      Sector_Address : Virtual_Address_T := Null_Address;
      Current_Sector : Sector_Index_T := 0;

      Was_New_Entry_Created : Boolean := False;
      New_Dir_Entry_Index   : Natural := 0;

      Number_Of_LFN_Entries_Needed : Natural := 0;
   begin
      Current_Sector := Filesystem_Info.FAT12_16_Root_Directory_Sector;

      LFN_Entries_Are_Required : constant Boolean :=
        Are_LFN_Entries_Required (Filename);

      if LFN_Entries_Are_Required then
         Get_Number_Of_LFN_Entries_Needed_For_Filename
           (Filename, Number_Of_LFN_Entries_Needed, Result);
         if Is_Error (Result) then
            return;
         end if;
      end if;

      Total_Entries_Needed : constant Natural :=
        Number_Of_LFN_Entries_Needed + 1;

      --  A current limitation of this implementation is that LFN entries don't
      --  span across sector boundaries. This may be resolved in a future
      --  update.
      Read_Sectors_Loop : for I in
        1 .. Filesystem_Info.Sectors_In_Root_Directory
      loop
         --  Read each FAT logical sector in the root directory into memory.
         Read_Sector_From_Filesystem
           (Filesystem,
            Reading_Process,
            Current_Sector,
            Filesystem_Info.Bytes_Per_Sector,
            Sector_Address,
            Result);
         if Is_Error (Result) then
            return;
         end if;

         Directory_Entries_In_This_Block : constant Natural :=
           Filesystem_Info.Bytes_Per_Sector / 32;

         Parse_Directory_Buffer : declare
            Directory :
              Directory_Index_T (1 .. Directory_Entries_In_This_Block)
            with Import, Address => Sector_Address, Alignment => 1;

            Current_Free_Entry_Count : Natural := 0;
            DOS_Filename_Checksum    : Unsigned_8 := 0;
         begin
            Log_Debug
              ("Scanning for "
               & Total_Entries_Needed'Image
               & " free directory entries.",
               Logging_Tags_FAT);

            Parse_Directory_Loop : for Dir_Idx in 1 .. Directory'Length loop
               if Is_Free_Directory_Entry (Directory (Dir_Idx)) then
                  Log_Debug
                    ("Found free directory entry at idx:" & Dir_Idx'Image,
                     Logging_Tags_FAT);

                  if Current_Free_Entry_Count = 0 then
                     New_Dir_Entry_Index := Dir_Idx;
                  end if;

                  Current_Free_Entry_Count := Current_Free_Entry_Count + 1;
                  if Current_Free_Entry_Count = Total_Entries_Needed then
                     Log_Debug
                       ("Found "
                        & Current_Free_Entry_Count'Image
                        & " free directory entries at index "
                        & New_Dir_Entry_Index'Image,
                        Logging_Tags_FAT);

                     DOS_Entry_Index : constant Natural :=
                       New_Dir_Entry_Index + Number_Of_LFN_Entries_Needed;

                     Create_DOS_Directory_Entry
                       (Filename,
                        Directory (DOS_Entry_Index),
                        DOS_Filename_Checksum,
                        Result);
                     if Is_Error (Result) then
                        return;
                     end if;

                     if LFN_Entries_Are_Required then
                        Create_Long_File_Name_Entries
                          (Filename,
                           Sector_Address,
                           Number_Of_LFN_Entries_Needed,
                           New_Dir_Entry_Index,
                           DOS_Filename_Checksum,
                           Result);
                        if Is_Error (Result) then
                           return;
                        end if;
                     end if;

                     Write_Sector_To_Filesystem
                       (Filesystem,
                        Reading_Process,
                        Current_Sector,
                        Filesystem_Info.Bytes_Per_Sector,
                        Result);
                     if Is_Error (Result) then
                        return;
                     end if;

                     Was_New_Entry_Created := True;
                     exit Parse_Directory_Loop;
                  end if;
               else
                  Current_Free_Entry_Count := 0;
               end if;
            end loop Parse_Directory_Loop;
         end Parse_Directory_Buffer;

         Release_Sector
           (Filesystem,
            Current_Sector,
            Filesystem_Info.Bytes_Per_Sector,
            Result);
         if Is_Error (Result) then
            return;
         end if;

         if Was_New_Entry_Created then
            Result := Success;
            return;
         end if;

         --  Increment the current read sector.
         Current_Sector := Current_Sector + 1;
      end loop Read_Sectors_Loop;

      Result := No_Free_Entries;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Create_File_In_Root_Directory_FAT16");
         Result := Constraint_Exception;
   end Create_File_In_Root_Directory_FAT16;

   procedure Create_File_FAT16
     (Filesystem      : Filesystem_Access;
      Reading_Process : in out Process_Control_Block_T;
      Filesystem_Info : FAT_Filesystem_Info_T;
      Filename        : Filesystem_Path_T;
      Parent_Node     : Filesystem_Node_Access;
      New_Node        : out Filesystem_Node_Access;
      Result          : out Function_Result) is
   begin
      if Parent_Node.all.Node_Type = Filesystem_Node_Type_Mounted_Filesystem
      then
         Create_File_In_Root_Directory_FAT16
           (Filesystem,
            Reading_Process,
            Filesystem_Info,
            Filename,
            Parent_Node,
            New_Node,
            Result);
      else
         --  Creating files in non-root directories is not yet implemented.
         Log_Error
           ("Creating files in non-root FAT16 directories is not supported.");
         New_Node := null;
         Result := Not_Supported;
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
        Get_Buffer_Max_Directory_Entry_Count
          (Filesystem_Info.Bytes_Per_Sector);

      Current_Cluster :=
        Get_First_Cluster_From_Index_FAT16 (Parent_Node.all.Index);

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

            Parse_Directory_Buffer : declare
               Directory : Directory_Index_T (1 .. Directory_Entries_In_Sector)
               with
                 Import,
                 Address   => Block_Address + Sector_Offset_Within_Block,
                 Alignment => 1;
            begin
               Search_FAT_Directory_For_File
                 (Filesystem,
                  Directory,
                  Filename,
                  Parent_Node,
                  Entry_Long_Filename,
                  Entry_Long_Filename_Length,
                  Entry_Long_Filename_Checksum,
                  Checksum_Valid,
                  Filesystem_Node,
                  Last_Entry_Reached,
                  Search_Result);
            end Parse_Directory_Buffer;

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
         Get_FAT16_Entry
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
           Get_Buffer_Max_Directory_Entry_Count
             (Sectors_Within_This_Block * Filesystem_Info.Bytes_Per_Sector);

         Parse_Directory_Buffer : declare
            Directory :
              Directory_Index_T (1 .. Directory_Entries_In_This_Block)
            with
              Import,
              Address   => Block_Address + Sector_Offset_Within_Block,
              Alignment => 1;
         begin
            Search_FAT_Directory_For_File
              (Filesystem,
               Directory,
               Filename,
               Parent_Node,
               Entry_Long_Filename,
               Entry_Long_Filename_Length,
               Entry_Long_Filename_Checksum,
               Checksum_Valid,
               Filesystem_Node,
               Last_Entry_Reached,
               Search_Result);
         end Parse_Directory_Buffer;

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

   procedure Get_FAT16_Entry
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

      declare
         --  Despite the fact that we're loading an entire block, since the
         --  filesystem reasons about its layout in terms of variable-size
         --  'sectors' we only want to work with a single sector, since we
         --  can't guarantee its size.
         FAT16_Table : FAT16_Table_T (0 .. Number_Of_Entries_Within_Sector - 1)
         with
           Import,
           Alignment => 1,
           Address   => Block_Address + Sector_Offset_Within_Block;
      begin
         FAT_Entry := FAT16_Table (Cluster_Index_Within_Sector);
      end;

      --  Result set by this call.
      Release_Block (Filesystem, Block_Number, Result);
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Get_FAT16_Entry");
         FAT_Entry := 0;
         Result := Constraint_Exception;
   end Get_FAT16_Entry;

   procedure Write_Table_Entry_FAT16
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

         declare
            FAT16_Table :
              FAT16_Table_T (0 .. Number_Of_Entries_Within_Sector - 1)
            with
              Import,
              Alignment => 1,
              Address   => Block_Address + Sector_Offset_Within_Block;
         begin
            FAT16_Table (Cluster_Index_Within_Sector) := FAT_Entry;
         end;

         Write_Block_To_Filesystem
           (Filesystem, Writing_Process, Block_Number, Result);
         if Is_Error (Result) then
            return;
         end if;

         --  Result set by this call.
         Release_Block (Filesystem, Block_Number, Result);
      end loop;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Write_Table_Entry_FAT16");
         Result := Constraint_Exception;
   end Write_Table_Entry_FAT16;

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
         declare
            --  If the current sector is the starting sector, we need to start
            --  checking from index 2, rather than from the index 0, which
            --  contains reserved entries.
            Start_Index : constant Integer :=
              (if Sector_Number = Filesystem_Info.First_FAT_Sector
               then 2
               else 0);

            FAT16_Table_Sector : FAT16_Table_T (0 .. Clusters_Per_Sector - 1)
            with
              Import,
              Alignment => 1,
              Address   => Block_Address + Sector_Offset_Within_Block;
         begin
            Check_Cluster_Loop : for Current_Cluster in
              Start_Index .. Clusters_Per_Sector - 1
            loop
               if Is_Cluster_Free
                    (Unsigned_32 (FAT16_Table_Sector (Current_Cluster)))
               then
                  Found_Free_Cluster := True;
                  Free_Cluster :=
                    Unsigned_16
                      (Sector_Number - Filesystem_Info.First_FAT_Sector)
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
         end;
      end loop Read_Sectors_Loop;

      Free_Cluster := 0;
      Result := No_Free_Clusters;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Find_Free_Cluster_FAT16");
         Free_Cluster := 0;
         Result := Constraint_Exception;
   end Find_Free_Cluster_FAT16;

   procedure Allocate_Cluster_FAT16
     (Filesystem      : Filesystem_Access;
      Writing_Process : in out Process_Control_Block_T;
      Filesystem_Info : FAT_Filesystem_Info_T;
      New_Cluster     : out Unsigned_16;
      Result          : out Function_Result) is
   begin
      Find_Free_Cluster_FAT16
        (Filesystem, Writing_Process, Filesystem_Info, New_Cluster, Result);
      if Is_Error (Result) then
         New_Cluster := 0;
         return;
      end if;

      Log_Error
        ("Found free FAT16 cluster: " & New_Cluster'Image, Logging_Tags_FAT);

      --  Mark the newly allocated cluster as end-of-chain in the FAT.
      Write_Table_Entry_FAT16
        (Filesystem,
         Writing_Process,
         Filesystem_Info,
         New_Cluster,
         Cluster_Marker_EOC_FAT16,
         Result);
      if Is_Error (Result) then
         New_Cluster := 0;
         return;
      end if;

      Log_Error
        ("Allocated free FAT16 cluster: " & New_Cluster'Image,
         Logging_Tags_FAT);

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Allocate_Cluster_FAT16");
         New_Cluster := 0;
         Result := Constraint_Exception;
   end Allocate_Cluster_FAT16;

   procedure Extend_Cluster_Chain_FAT16
     (Filesystem      : Filesystem_Access;
      Writing_Process : in out Process_Control_Block_T;
      Filesystem_Info : FAT_Filesystem_Info_T;
      Cluster         : Unsigned_16;
      New_Cluster     : out Unsigned_16;
      Result          : out Function_Result) is
   begin
      Allocate_Cluster_FAT16
        (Filesystem, Writing_Process, Filesystem_Info, New_Cluster, Result);
      if Is_Error (Result) then
         return;
      end if;

      Write_Table_Entry_FAT16
        (Filesystem,
         Writing_Process,
         Filesystem_Info,
         Cluster,
         New_Cluster,
         Result);
      if Is_Error (Result) then
         return;
      end if;

      Result := Success;
   end Extend_Cluster_Chain_FAT16;

   function Get_First_Cluster_From_Index_FAT16
     (Index : Filesystem_Node_Index_T) return Unsigned_16 is
   begin
      return Unsigned_16 (Shift_Right (Index, 32));
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Get_First_Cluster_From_Index_FAT16");
         return 0;
   end Get_First_Cluster_From_Index_FAT16;

end Filesystems.FAT.FAT16;
