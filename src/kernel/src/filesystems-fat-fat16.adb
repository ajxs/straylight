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
      Read_FAT16_Root_Directory_Into_Buffer
        (Filesystem,
         Reading_Process,
         Filesystem_Info,
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
         if Is_Error (Result) then
            return;
         end if;

         --  Increment the device read offset.
         Current_Read_Sector := Current_Read_Sector + 1;

         Destination_Virtual_Address :=
           Destination_Virtual_Address
           + Storage_Offset (Filesystem_Info.Bytes_Per_Sector);
      end loop;

      Log_Debug ("Finished Reading FAT16 Root Directory", Logging_Tags_FAT);

      Result := Success;
   end Read_FAT16_Root_Directory_Into_Buffer;

   procedure Get_FAT16_Table_Entry_Sector_Number
     (Filesystem_Info : FAT_Filesystem_Info_T;
      Cluster         : Unsigned_32;
      Sector_Number   : out Unsigned_64;
      Result          : out Function_Result) is
   begin
      Sector_Number :=
        Filesystem_Info.First_FAT_Sector
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
      FAT_Entry       : out Unsigned_32;
      Result          : out Function_Result)
   is
      Sector_Address : Virtual_Address_T := Null_Address;
      Sector_Number  : Unsigned_64 := 0;
      Cluster_Index  : Natural := 0;
   begin
      Get_FAT16_Table_Entry_Sector_Number
        (Filesystem_Info, Cluster, Sector_Number, Result);
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
         FAT_Entry := Unsigned_32 (FAT16_Table (Cluster_Index));
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

end Filesystems.FAT.FAT16;
