with Filesystems.Block_Cache; use Filesystems.Block_Cache;
with Memory.Kernel;           use Memory.Kernel;

package body Filesystems.FAT.FAT16 is
   procedure Find_File_In_FAT16_Directory
     (Filesystem      : Filesystem_Access;
      Reading_Process : in out Process_Control_Block_T;
      Filesystem_Info : FAT_Filesystem_Info_T;
      Filename        : Filesystem_Path_T;
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

         Parse_Directory_Buffer : declare
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
      Filename        : Filesystem_Path_T;
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
      Read_Sectors_Into_Buffer
        (Filesystem,
         Filesystem_Info,
         Reading_Process,
         Filesystem_Info.Root_Directory_Sector,
         Integer (Filesystem_Info.Root_Directory_Sector_Count),
         Directory_Buffer_Address,
         Result);
      if Is_Error (Result) then
         goto Free_Buffer;
      end if;

      Parse_Directory_Buffer : declare
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

         --  All results fall-through to free the allocated buffer.
         if Result = Success then
            Log_Debug ("Found matching file entry.", Logging_Tags_FAT);
         elsif Result = File_Not_Found then
            Log_Debug
              ("File not found in root FAT directory.", Logging_Tags_FAT);
         end if;
      end Parse_Directory_Buffer;

      <<Free_Buffer>>
      Free_Kernel_Memory (Directory_Buffer_Address, Free_Buffer_Result);
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Find_File_In_FAT16_Root_Directory");
         Result := Constraint_Exception;
   end Find_File_In_FAT16_Root_Directory;

   procedure Get_FAT16_Table_Entry_Sector_Number
     (Filesystem_Info : FAT_Filesystem_Info_T;
      FAT_Index       : Natural;
      Cluster         : Unsigned_32;
      Sector_Number   : out Unsigned_64;
      Result          : out Function_Result) is
   begin
      FAT_Table_Offset : constant Unsigned_64 :=
        Unsigned_64 (FAT_Index)
        * Unsigned_64 (Filesystem_Info.FAT_Sector_Count);

      Sector_Number :=
        Filesystem_Info.First_FAT_Sector
        + FAT_Table_Offset
        + Unsigned_64
            ((Cluster * 2) / Unsigned_32 (Filesystem_Info.Bytes_Per_Sector));

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Get_FAT16_Table_Entry_Sector_Number");
         Sector_Number := 0;
         Result := Constraint_Exception;
   end Get_FAT16_Table_Entry_Sector_Number;

   procedure Get_FAT16_Table_Cluster_Index
     (Filesystem_Info : FAT_Filesystem_Info_T;
      Cluster         : Unsigned_32;
      Cluster_Index   : out Natural;
      Result          : out Function_Result) is
   begin
      Cluster_Index :=
        Natural
          (Cluster mod Unsigned_32 (Filesystem_Info.Bytes_Per_Sector / 2));

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Get_FAT16_Table_Cluster_Index");
         Cluster_Index := 0;
         Result := Constraint_Exception;
   end Get_FAT16_Table_Cluster_Index;

   procedure Read_FAT16_Table_Entry
     (Filesystem      : Filesystem_Access;
      Reading_Process : in out Process_Control_Block_T;
      Filesystem_Info : FAT_Filesystem_Info_T;
      Cluster         : Unsigned_32;
      FAT_Entry       : out Unsigned_16;
      Result          : out Function_Result)
   is
      Sector_Address : Virtual_Address_T := Null_Address;
      Sector_Number  : Sector_Index_T := 0;
      Cluster_Index  : Natural := 0;
   begin
      --  Uses the primary FAT table (index 0) to read the FAT entry,
      --  since all FAT tables should be identical.
      Get_FAT16_Table_Entry_Sector_Number
        (Filesystem_Info, 0, Cluster, Sector_Number, Result);
      if Is_Error (Result) then
         FAT_Entry := 0;
         return;
      end if;

      Get_FAT16_Table_Cluster_Index
        (Filesystem_Info, Cluster, Cluster_Index, Result);
      if Is_Error (Result) then
         FAT_Entry := 0;
         return;
      end if;

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

      declare
         FAT16_Table :
           FAT16_Table_T (0 .. (Filesystem_Info.Bytes_Per_Sector / 2) - 1)
         with Import, Alignment => 1, Address => Sector_Address;
      begin
         FAT_Entry := FAT16_Table (Cluster_Index);
      end;

      --  Result set by this call.
      Release_Sector
        (Filesystem, Sector_Number, Filesystem_Info.Bytes_Per_Sector, Result);
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Read_FAT16_Table_Entry");
         FAT_Entry := 0;
         Result := Constraint_Exception;
   end Read_FAT16_Table_Entry;

   procedure Write_FAT16_Table_Entry
     (Filesystem      : Filesystem_Access;
      Writing_Process : in out Process_Control_Block_T;
      Filesystem_Info : FAT_Filesystem_Info_T;
      Cluster         : Unsigned_32;
      FAT_Entry       : Unsigned_16;
      Result          : out Function_Result)
   is
      Sector_Address : Virtual_Address_T := Null_Address;
      Sector_Number  : Sector_Index_T := 0;
      Cluster_Index  : Natural := 0;
   begin
      if Filesystem_Info.FAT_Table_Count = 0 then
         Log_Error ("FAT_Table_Count is 0 in Write_FAT16_Table_Entry");
         Result := Invalid_Filesystem;
         return;
      end if;

      Get_FAT16_Table_Cluster_Index
        (Filesystem_Info, Cluster, Cluster_Index, Result);
      if Is_Error (Result) then
         return;
      end if;

      for FAT_Table_Idx in 0 .. Filesystem_Info.FAT_Table_Count - 1 loop
         Get_FAT16_Table_Entry_Sector_Number
           (Filesystem_Info, FAT_Table_Idx, Cluster, Sector_Number, Result);
         if Is_Error (Result) then
            return;
         end if;

         Read_Sector_From_Filesystem
           (Filesystem,
            Writing_Process,
            Sector_Number,
            Filesystem_Info.Bytes_Per_Sector,
            Sector_Address,
            Result);
         if Is_Error (Result) then
            return;
         end if;

         declare
            FAT16_Table :
              FAT16_Table_T (0 .. (Filesystem_Info.Bytes_Per_Sector / 2) - 1)
            with Import, Alignment => 1, Address => Sector_Address;
         begin
            FAT16_Table (Cluster_Index) := FAT_Entry;
         end;

         Write_Sector_To_Filesystem
           (Filesystem,
            Writing_Process,
            Sector_Number,
            Filesystem_Info.Bytes_Per_Sector,
            Result);
         if Is_Error (Result) then
            return;
         end if;

         --  Result set by this call.
         Release_Sector
           (Filesystem,
            Sector_Number,
            Filesystem_Info.Bytes_Per_Sector,
            Result);
      end loop;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Write_FAT16_Table_Entry");
         Result := Constraint_Exception;
   end Write_FAT16_Table_Entry;

   procedure Find_Free_FAT16_Cluster
     (Filesystem          : Filesystem_Access;
      Writing_Process     : in out Process_Control_Block_T;
      Filesystem_Info     : FAT_Filesystem_Info_T;
      Start_Cluster_Index : Unsigned_32;
      Free_Cluster        : out Unsigned_32;
      Result              : out Function_Result)
   is
      Sector_Data_Address   : Virtual_Address_T := Null_Address;
      Release_Sector_Result : Function_Result := Unset;

      Start_Sector_Number : Sector_Index_T := 0;
   begin
      --  Find out what sector the starting cluster's FAT Table entry is in.
      Get_FAT16_Table_Entry_Sector_Number
        (Filesystem_Info, 0, Start_Cluster_Index, Start_Sector_Number, Result);
      if Is_Error (Result) then
         Free_Cluster := 0;
         return;
      end if;

      Total_Clusters_In_Sector : constant Integer :=
        (Filesystem_Info.Bytes_Per_Sector / 2);

      Last_FAT_Sector_Number : constant Sector_Index_T :=
        Sector_Index_T
          (Filesystem_Info.First_FAT_Sector
           + Unsigned_64 (Filesystem_Info.FAT_Sector_Count)
           - 1);

      --  Loop through the FAT sectors starting from the sector containing
      --  the starting cluster's FAT entry, and read each sector into a buffer.
      --  For each sector, loop through the FAT entries contained in the sector
      --  to see if any of them are free.
      Read_Sectors_Loop : for Curr_Sector in
        Start_Sector_Number .. Last_FAT_Sector_Number
      loop
         Read_Sector_From_Filesystem
           (Filesystem,
            Writing_Process,
            Curr_Sector,
            Filesystem_Info.Bytes_Per_Sector,
            Sector_Data_Address,
            Result);
         if Is_Error (Result) then
            Free_Cluster := 0;
            return;
         end if;

         --  Check all of the FAT entries in the sector for a free cluster.
         declare
            --  If the current sector is the starting sector, we need to start
            --  checking from the starting cluster index, rather than from the
            --  beginning of the sector.
            Start_Index : constant Integer :=
              (if Curr_Sector = Start_Sector_Number
               then
                 Integer
                   (Start_Cluster_Index
                    mod Unsigned_32 (Total_Clusters_In_Sector))
               else 0);

            FAT16_Table_Sector :
              FAT16_Table_T (0 .. Total_Clusters_In_Sector - 1)
            with Import, Alignment => 1, Address => Sector_Data_Address;
         begin
            Check_Cluster_Loop : for Curr_Cluster in
              Start_Index .. Total_Clusters_In_Sector - 1
            loop
               if Is_Cluster_Free
                    (Unsigned_32 (FAT16_Table_Sector (Curr_Cluster)))
               then
                  Free_Cluster :=
                    Unsigned_32
                      (Curr_Sector - Filesystem_Info.First_FAT_Sector)
                    * Unsigned_32 (Total_Clusters_In_Sector)
                    + Unsigned_32 (Curr_Cluster);

                  Log_Debug
                    ("Found free FAT16 cluster: " & Free_Cluster'Image,
                     Logging_Tags_FAT);

                  Release_Sector
                    (Filesystem,
                     Curr_Sector,
                     Filesystem_Info.Bytes_Per_Sector,
                     Result);
                  if Is_Error (Result) then
                     Free_Cluster := 0;
                     return;
                  end if;

                  Result := Success;
                  return;
               end if;
            end loop Check_Cluster_Loop;
         end;

         Release_Sector
           (Filesystem,
            Curr_Sector,
            Filesystem_Info.Bytes_Per_Sector,
            Release_Sector_Result);
         if Is_Error (Release_Sector_Result) then
            Result := Release_Sector_Result;
         end if;
      end loop Read_Sectors_Loop;

      Free_Cluster := 0;
      Result := No_Free_Clusters;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Find_Free_FAT16_Cluster");
         Free_Cluster := 0;
         Result := Constraint_Exception;
   end Find_Free_FAT16_Cluster;

end Filesystems.FAT.FAT16;
