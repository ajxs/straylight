-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Devices.Console;
with Devices.UART;
with Filesystems.Block_Cache;
with Filesystems.FAT;
with Filesystems.Root;
with Filesystems.UStar;
with Filesystems.Node_Cache; use Filesystems.Node_Cache;
with Filesystems.Paths;
with Logging;                use Logging;
with Memory.Kernel;          use Memory.Kernel;
with Hart_State;             use Hart_State;

package body Filesystems is
   procedure Find_Unused_File_Handle_Entry
     (File_Handle_Array : in out Process_File_Handle_Array;
      File_Handle_Index : out Positive;
      Result            : out Function_Result) is
   begin
      for Index in File_Handle_Array'Range loop
         if not File_Handle_Array (Index).Entry_Used then
            File_Handle_Index := Index;
            Result := Success;
            return;
         end if;
      end loop;

      Log_Error ("No unused file handle entries");
      Result := No_Free_Entries;
   end Find_Unused_File_Handle_Entry;

   ----------------------------------------------------------------------------
   --  Finds the node with the given name in a directory: first the node
   --  cache is checked to see if the file is already cached, then the
   --  directory's filesystem is searched.
   --  Passing a null Parent_Node means to search for the root directory.
   --  Returns a null node, without an error, if the specified file
   --  doesn't exist in the directory.
   ----------------------------------------------------------------------------
   procedure Find_Child_Node
     (Process     : in out Process_Control_Block_T;
      Filesystem  : Filesystem_Access;
      Parent_Node : Filesystem_Node_Access;
      Child_Name  : Filesystem_Path_T;
      Node        : out Filesystem_Node_Access;
      Result      : out Function_Result) is
   begin
      --  The root directory is the only node with no parent.
      Parent_Index : constant Filesystem_Node_Index_T :=
        (if Parent_Node = null then 0 else Parent_Node.all.Index);

      --  Check if the file is already cached.
      Find_Filesystem_Node_In_Cache
        (Filesystem, Parent_Index, Child_Name, Node, Result);
      if Result /= Not_Found then
         --  The node was found in the cache, or the search failed.
         return;
      end if;

      case Filesystem.all.Filesystem_Type is
         when Filesystem_Type_UStar =>
            Filesystems.UStar.Find_File
              (Filesystem, Process, Child_Name, Parent_Node, Node, Result);

         when Filesystem_Type_Root  =>
            Filesystems.Root.Find_File
              (Filesystem, Child_Name, Parent_Index, Node, Result);

         when Filesystem_Type_FAT   =>
            Filesystems.FAT.Find_File
              (Filesystem, Process, Child_Name, Parent_Node, Node, Result);

         when others                =>
            Log_Error
              ("Unsupported filesystem type: "
               & Filesystem.all.Filesystem_Type'Image);
            Node := null;
            Result := Not_Supported;
      end case;
   exception
      when Constraint_Error =>
         Log_Constraint_Error;
         Node := null;
         Result := Constraint_Exception;
   end Find_Child_Node;

   procedure Find_File
     (Process         : in out Process_Control_Block_T;
      Path            : Filesystem_Path_T;
      Filesystem_Node : out Filesystem_Node_Access;
      Result          : out Function_Result)
   is
      Current_Filesystem : Filesystem_Access := null;

      Filesystem_Node_Parent : Filesystem_Node_Access := null;

      Next_Token_Start_Index : Integer := 1;
      Next_Token_End_Index   : Integer := 1;
      Next_Token_Length      : Integer := 1;
   begin
      pragma
        Debug
          (Debug_Filesystems,
           Log_Debug ("Filesystems.Find_File: '" & Path & "'"));

      if Path (Path'First) /= Filesystem_Node_Separator then
         --  Relative path; start at process's current working directory.
         Log_Error ("Relative paths not yet implemented.");
         Filesystem_Node := null;
         Result := File_Not_Found;
         return;
      end if;

      --  An absolute path starts at the root directory. Its node is named
      --  "/" in the root filesystem, and is the only node with no parent.
      Current_Filesystem := System_Root_Filesystem;

      --  Find the root directory node in the root filesystem.
      Find_Child_Node
        (Process,
         Current_Filesystem,
         null,
         [1 => Filesystem_Node_Separator],
         Filesystem_Node,
         Result);
      if Is_Error (Result) then
         return;
      end if;

      if Filesystem_Node = null then
         Log_Error ("Root directory node not found");
         Result := File_Not_Found;
         return;
      end if;

      --  Begin searching with the root filesystem node as the parent.
      Filesystem_Node_Parent := Filesystem_Node;

      loop
         Paths.Get_Next_Path_Component
           (Path,
            Next_Token_Start_Index,
            Next_Token_End_Index,
            Next_Token_Length);

         exit when Next_Token_Length = 0;

         --  Ensure we can traverse the parent node.
         if not Can_Filesystem_Node_Contain_Child_Nodes
                  (Filesystem_Node_Parent.all)
         then
            Filesystem_Node := null;
            exit;
         end if;

         --  Create a new string for the next path token.
         --  This is ideal because string slicing in Ada keeps the original
         --  string indexes, which can be more complicated to work with.
         Next_Path_Token :
           constant Filesystem_Path_T (1 .. Next_Token_Length) :=
             Path (Next_Token_Start_Index .. Next_Token_End_Index - 1);

         pragma
           Debug
             (Debug_Filesystems,
              Log_Debug ("Current path token: '" & Next_Path_Token & "'"));

         Find_Child_Node
           (Process,
            Current_Filesystem,
            Filesystem_Node_Parent,
            Next_Path_Token,
            Filesystem_Node,
            Result);
         if Is_Error (Result) then
            return;
         end if;

         if Filesystem_Node = null then
            pragma
              Debug
                (Debug_Filesystems,
                 Log_Debug
                   ("Filesystem node not found for token: '"
                    & Next_Path_Token
                    & "'"));
            exit;
         end if;

         if Filesystem_Node.all.Node_Type
           = Filesystem_Node_Type_Mounted_Filesystem
         then
            pragma
              Debug (Debug_Filesystems, Log_Debug ("Filesystem node found."));

            Current_Filesystem := Filesystem_Node.all.Mounted_Filesystem;
            if Current_Filesystem = null then
               Log_Error ("Mounted filesystem has null filesystem pointer");
               Filesystem_Node := null;
               exit;
            end if;
         end if;

         Next_Token_Start_Index := Next_Token_End_Index;
         Filesystem_Node_Parent := Filesystem_Node;
      end loop;

      if Filesystem_Node = null then
         Result := File_Not_Found;
         return;
      end if;

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Constraint_Error;
         Result := Constraint_Exception;
   end Find_File;

   procedure Create_File_Handle_For_Filesystem_Node_Unlocked
     (Process         : in out Process_Control_Block_T;
      Filesystem_Node : Filesystem_Node_Access;
      File_Open_Flags : File_Open_Flags_T;
      File_Handle     : out Process_File_Handle_Access;
      Result          : out Function_Result)
   is
      File_Handle_Index : Positive := 1;
   begin
      Find_Unused_File_Handle_Entry (Open_Files, File_Handle_Index, Result);
      if Is_Error (Result) then
         return;
      end if;

      File_Handle := Open_Files (File_Handle_Index)'Access;

      File_Handle.all.File_Handle_Id := Process.Next_File_Handle_Id;
      Process.Next_File_Handle_Id := Process.Next_File_Handle_Id + 1;

      File_Handle.all.Entry_Used := True;
      File_Handle.all.File := Filesystem_Node;
      File_Handle.all.Position := 0;
      File_Handle.all.File_Open_Flags := File_Open_Flags;
      File_Handle.all.Process_Id := Process.Process_Id;

      Filesystem_Node.all.Handle_Count := Filesystem_Node.all.Handle_Count + 1;

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Constraint_Error;
         Result := Constraint_Exception;
   end Create_File_Handle_For_Filesystem_Node_Unlocked;

   procedure Create_File_Handle_For_Filesystem_Node
     (Process         : in out Process_Control_Block_T;
      Filesystem_Node : Filesystem_Node_Access;
      File_Open_Flags : File_Open_Flags_T;
      File_Handle     : out Process_File_Handle_Access;
      Result          : out Function_Result) is
   begin
      Acquire_Spinlock (Open_Files_Spinlock);
      Acquire_Spinlock (Process.Spinlock);

      Create_File_Handle_For_Filesystem_Node_Unlocked
        (Process, Filesystem_Node, File_Open_Flags, File_Handle, Result);

      Release_Spinlock (Process.Spinlock);
      Release_Spinlock (Open_Files_Spinlock);
   end Create_File_Handle_For_Filesystem_Node;

   function Does_File_Handle_Pointer_Point_To_Regular_File
     (File_Handle : Process_File_Handle_Access) return Boolean is
   begin
      return
        File_Handle /= null
        and then File_Handle.all.File /= null
        and then
          File_Handle.all.File.all.Node_Type
          = Filesystem_Node_Type_Regular_File;
   exception
      when Constraint_Error =>
         Log_Constraint_Error;
         return False;
   end Does_File_Handle_Pointer_Point_To_Regular_File;

   procedure Open_File
     (Process         : in out Process_Control_Block_T;
      Path            : Filesystem_Path_T;
      File_Open_Flags : File_Open_Flags_T;
      File_Handle     : out Process_File_Handle_Access;
      Result          : out Function_Result)
   is
      Filesystem_Node : Filesystem_Node_Access := null;

      procedure Print_File_Found_Status (Found : Boolean) is
      begin
         Log_Debug
           ((if Found then "File found: '" else "File not found: '")
            & Path
            & "'");
      exception
         when Constraint_Error =>
            null;
      end Print_File_Found_Status;

      procedure Log_Open_File is
      begin
         Log_Debug ("Filesystems.Open_File: '" & Path & "'");
      exception
         when Constraint_Error =>
            null;
      end Log_Open_File;
   begin
      pragma Debug (Debug_Filesystems, Log_Open_File);

      Find_File (Process, Path, Filesystem_Node, Result);
      if Result = File_Not_Found then
         pragma Debug (Debug_Filesystems, Print_File_Found_Status (False));

         --  If the open mode flags indicate that the missing file should be
         --  created, create it and return the new node, else exit.
         if not File_Open_Flags.Creation_Flags.Create_If_Not_Exist then
            pragma
              Debug
                (Debug_Filesystems,
                 Log_Debug
                   ("Not creating file because creation flag not set."));
            File_Handle := null;
            return;
         end if;

         pragma
           Debug (Debug_Filesystems, Log_Debug ("Creating missing file."));

         Create_File (Process, Path, Filesystem_Node, Result);
         if Is_Error (Result) then
            File_Handle := null;
            return;
         end if;
      elsif Is_Error (Result) then
         File_Handle := null;
         return;
      end if;

      pragma Debug (Debug_Filesystems, Print_File_Found_Status (True));

      Create_File_Handle_For_Filesystem_Node
        (Process, Filesystem_Node, File_Open_Flags, File_Handle, Result);
      if Is_Error (Result) then
         File_Handle := null;
         return;
      end if;

      --  Only a regular file will be truncated.
      if File_Open_Flags.Creation_Flags.Truncate_Existing
        and then Does_File_Handle_Pointer_Point_To_Regular_File (File_Handle)
      then
         Truncate_File (Process, File_Handle, 0, Result);
         if Is_Error (Result) then
            File_Handle := null;
            return;
         end if;
      end if;

      Result := Success;
   end Open_File;

   procedure Read_File_Node_Type_Device
     (Process        : in out Process_Control_Block_T;
      File_Handle    : Process_File_Handle_Access;
      Buffer_Address : Virtual_Address_T;
      Bytes_To_Read  : Natural;
      Bytes_Read     : out Natural;
      Result         : out Function_Result)
   is
      --  This overlay is declared here rather than after 'begin' so that it
      --  is frozen, and its address set, before any statement that uses it.
      --  When declared after 'begin', GNAT freezes it at its first use, which
      --  is inside the first case alternative, leaving the address unset in
      --  every other alternative.
      Data_To_Read : Storage_Array (1 .. Storage_Count (Bytes_To_Read))
      with Import, Alignment => 1, Address => Buffer_Address;
   begin
      Device : Device_T renames File_Handle.all.File.all.Mounted_Device.all;

      case Device.Driver_Type is
         when Driver_Type_UART    =>
            Devices.UART.Read_Bytes
              (Device.Common,
               Device.UART,
               Process,
               Data_To_Read,
               Storage_Count (Bytes_Read),
               Result);

         when Driver_Type_Console =>
            Devices.Console.Read_Bytes
              (Device.Common,
               Device.Console,
               Process,
               Data_To_Read,
               Storage_Count (Bytes_Read),
               Result);

         when others              =>
            Log_Error ("Unsupported driver type: " & Device.Driver_Type'Image);
            Bytes_Read := 0;
            Result := Not_Supported;
      end case;
   exception
      when Constraint_Error =>
         Log_Constraint_Error;
         Bytes_Read := 0;
         Result := Constraint_Exception;
   end Read_File_Node_Type_Device;

   procedure Read_File_Node_Type_Regular_File
     (Process        : in out Process_Control_Block_T;
      File_Handle    : Process_File_Handle_Access;
      Buffer_Address : Virtual_Address_T;
      Bytes_To_Read  : Natural;
      Bytes_Read     : out Natural;
      Result         : out Function_Result) is
   begin
      Remaining_Bytes_In_File : constant Natural :=
        Natural
          (File_Handle.all.File.all.File_Size - File_Handle.all.Position);

      Real_Bytes_To_Read : constant Natural :=
        Natural'Min (Bytes_To_Read, Remaining_Bytes_In_File);

      if Real_Bytes_To_Read = 0 then
         Bytes_Read := 0;
         Result := Success;
         return;
      end if;

      case File_Handle.all.File.all.Parent_Filesystem.all.Filesystem_Type is
         when Filesystem_Type_UStar =>
            Filesystems.UStar.Read_File
              (File_Handle.all.File.all.Parent_Filesystem,
               Process,
               File_Handle.all.File,
               Buffer_Address,
               File_Handle.all.Position,
               Real_Bytes_To_Read,
               Bytes_Read,
               Result);

         when Filesystem_Type_FAT   =>
            Filesystems.FAT.Read_File
              (File_Handle.all.File.all.Parent_Filesystem,
               Process,
               File_Handle.all.File,
               Buffer_Address,
               File_Handle.all.Position,
               Real_Bytes_To_Read,
               Bytes_Read,
               Result);

         when others                =>
            Log_Error
              ("Unsupported filesystem type: "
               & File_Handle.all.File.all.Parent_Filesystem.all
                   .Filesystem_Type'Image);
            Result := Not_Supported;
      end case;

      if Is_Error (Result) then
         Bytes_Read := 0;
         return;
      end if;

      File_Handle.all.Position :=
        File_Handle.all.Position + Unsigned_64 (Bytes_Read);

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Constraint_Error;
         Result := Constraint_Exception;
   end Read_File_Node_Type_Regular_File;

   procedure Read_File
     (Process        : in out Process_Control_Block_T;
      File_Handle    : Process_File_Handle_Access;
      Buffer_Address : Virtual_Address_T;
      Bytes_To_Read  : Natural;
      Bytes_Read     : out Natural;
      Result         : out Function_Result) is
   begin
      pragma
        Debug
          (Debug_Filesystems,
           Log_Debug
             ("Filesystems.Read_File: "
              & ASCII.LF
              & "  Bytes_To_Read: "
              & Bytes_To_Read'Image
              & ASCII.LF
              & "  File_Position: "
              & File_Handle.all.Position'Image));

      if File_Handle.all.File_Open_Flags.Access_Mode = Write_Only then
         Bytes_Read := 0;
         Result := Invalid_File_Permissions;
         return;
      end if;

      --  Note: In future it might make sense to move this check to _after_
      --  the file type dispatching, to ensure that the file type is validated
      --  before the no-op return.
      if Bytes_To_Read = 0 then
         Bytes_Read := 0;
         Result := Success;
         return;
      end if;

      if Bytes_To_Read > Maximum_File_Read_Size then
         Bytes_Read := 0;
         Result := Invalid_File_Size;
         return;
      end if;

      case File_Handle.all.File.all.Node_Type is
         when Filesystem_Node_Type_Regular_File =>
            Read_File_Node_Type_Regular_File
              (Process,
               File_Handle,
               Buffer_Address,
               Bytes_To_Read,
               Bytes_Read,
               Result);

         when Filesystem_Node_Type_Device       =>
            Read_File_Node_Type_Device
              (Process,
               File_Handle,
               Buffer_Address,
               Bytes_To_Read,
               Bytes_Read,
               Result);

         when others                            =>
            Log_Error
              ("Unsupported node type for reading: "
               & File_Handle.all.File.all.Node_Type'Image);
            Bytes_Read := 0;
            Result := Not_Supported;
      end case;

      pragma
        Debug
          (Debug_Filesystems,
           Log_Debug
             ("Filesystems.Read_File: " & Bytes_Read'Image & " bytes read."));
   exception
      when Constraint_Error =>
         Log_Constraint_Error;
         Bytes_Read := 0;
         Result := Constraint_Exception;
   end Read_File;

   procedure Seek_File
     (File_Handle : Process_File_Handle_Access;
      New_Offset  : Unsigned_64;
      Result      : out Function_Result) is
   begin
      --  Ensure that we can't seek past the last valid index into the file.
      File_Handle.all.Position :=
        Unsigned_64'Min
          ((if File_Handle.all.File.all.File_Size = 0
            then 0
            else File_Handle.all.File.all.File_Size),
           New_Offset);

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Constraint_Error;
         Result := Constraint_Exception;
   end Seek_File;

   procedure Read_Next_Directory_Entry
     (File_Handle : Process_File_Handle_Access;
      Entry_Name  : out Filesystem_Node_Name_T;
      Entry_Type  : out Filesystem_Node_Type_T;
      Result      : out Function_Result) is
   begin
      Entry_Name := Null_Filesystem_Node_Name;
      Entry_Type := Filesystem_Node_Type_None;

      Directory_Node : constant Filesystem_Node_Access := File_Handle.all.File;

      if not Can_Filesystem_Node_Contain_Child_Nodes (Directory_Node.all) then
         Log_Error ("Node isn't a directory");
         Result := Invalid_File_Type;
         return;
      end if;

      --  If the file handle refers to a mount point, the file entries being
      --  read are located in the root directory of the mounted filesystem.
      --  Otherwise, the entries are in a normal directory of the parent
      --  filesystem.
      Directory_Filesystem : constant Filesystem_Access :=
        (if Directory_Node.all.Node_Type
           = Filesystem_Node_Type_Mounted_Filesystem
         then Directory_Node.all.Mounted_Filesystem
         else Directory_Node.all.Parent_Filesystem);

      case Directory_Filesystem.all.Filesystem_Type is
         when Filesystem_Type_Root =>
            Filesystems.Root.Read_Next_Directory_Entry
              (Directory_Filesystem,
               Directory_Node.all.Index,
               File_Handle.all.Position,
               Entry_Name,
               Entry_Type,
               Result);

         when others               =>
            Log_Error
              ("Unsupported filesystem type: "
               & Directory_Filesystem.all.Filesystem_Type'Image);
            Result := Not_Supported;
      end case;
   exception
      when Constraint_Error =>
         Log_Constraint_Error;
         Result := Constraint_Exception;
   end Read_Next_Directory_Entry;

   procedure Set_Filesystem_Node_Name
     (Node      : in out Filesystem_Node_T;
      Node_Name : Filesystem_Path_T;
      Result    : out Function_Result) is
   begin
      if Node_Name'Length > Filesystem_Node_Name_Max_Byte_Length then
         Log_Error
           ("Filesystem node name too long: " & Node_Name'Length'Image);
         Result := Invalid_Argument;
         return;
      end if;

      Node.Filename.Byte_Length := Node_Name'Length;
      for Index in Node_Name'Range loop
         Node.Filename.Value (Index) := Node_Name (Index);
      end loop;

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Constraint_Error;
         Result := Constraint_Exception;
   end Set_Filesystem_Node_Name;

   function Is_Valid_Filesystem_Pointer
     (Filesystem : Filesystem_Access) return Boolean is
   begin
      return
        Filesystem /= null
        and then Filesystem.all.Device /= null
        and then
          Filesystem.all.Device.all.Driver_Type
          in Driver_Type_Ramdisk | Driver_Type_Virtio_Block;
   exception
      when Constraint_Error =>
         Log_Constraint_Error;
         return False;
   end Is_Valid_Filesystem_Pointer;

   function Sector_To_Block
     (Sector_Number : Sector_Index_T; Sector_Size : Natural)
      return Block_Index_T is
   begin
      return
        Block_Index_T ((Natural (Sector_Number) * Sector_Size) / Block_Size);
   exception
      when Constraint_Error =>
         Log_Constraint_Error;
         return 0;
   end Sector_To_Block;

   function Get_Sectors_Per_Block (Sector_Size : Natural) return Natural is
   begin
      return Block_Size / Sector_Size;
   exception
      when Constraint_Error =>
         Log_Constraint_Error;
         return 0;
   end Get_Sectors_Per_Block;

   procedure Initialise_Block_Cache is
      Result            : Function_Result := Unset;
      Allocation_Result : Virtual_Physical_Address_Pair_T;
      System_Block_Cache renames Filesystems.Block_Cache.System_Block_Cache;
   begin
      pragma
        Debug (Debug_Filesystems, Log_Debug ("Initialising block cache..."));

      System_Block_Cache.Spinlock.Lock_Id := Lock_Id_Block_Cache;

      --  Assign unique Lock IDs to the block cache spinlocks.
      for I in System_Block_Cache.Entries'Range loop
         System_Block_Cache.Entries (I).Sleeplock.Spinlock.Lock_Id :=
           Lock_Id_Block_Cache_Entry_Prefix + Lock_Id_T (I);
      end loop;

      --  Allocate memory for the block cache entries.
      --  Each block is conveniently the same size as a page.
      Allocate_Pages
        (System_Block_Cache.Entries'Last, Allocation_Result, Result);
      if Is_Error (Result) then
         --  Error already printed.
         Panic;
      end if;

      System_Block_Cache.Data_Address_Physical :=
        Allocation_Result.Physical_Address;
      System_Block_Cache.Data_Address_Virtual :=
        Allocation_Result.Virtual_Address;
   end Initialise_Block_Cache;

   procedure Allocate_Filesystem_Node
     (New_Node : out Filesystem_Node_Access; Result : out Function_Result)
   is
      Allocated_Address : Virtual_Address_T := Null_Address;
   begin
      Allocate_Kernel_Memory
        (Filesystem_Node_T'Size / 8, Allocated_Address, Result);
      if Is_Error (Result) then
         New_Node := null;
         return;
      end if;

      New_Node :=
        Convert_Address_To_Filesystem_Node_Access (Allocated_Address);

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Constraint_Error;
         New_Node := null;
         Result := Constraint_Exception;
   end Allocate_Filesystem_Node;

   procedure Find_File_Handle
     (Process_Id     : Process_Id_T;
      File_Handle_Id : Process_File_Handle_Id_T;
      File_Handle    : out Process_File_Handle_Access;
      Result         : out Function_Result) is
   begin
      for File_Handle_Entry of Open_Files loop
         if File_Handle_Entry.Entry_Used
           and then File_Handle_Entry.File_Handle_Id = File_Handle_Id
           and then File_Handle_Entry.Process_Id = Process_Id
         then
            File_Handle := File_Handle_Entry'Access;
            Result := Success;
            return;
         end if;
      end loop;

      Result := Not_Found;
   end Find_File_Handle;

   procedure Close_File_Unlocked
     (File_Handle : Process_File_Handle_Access; Result : out Function_Result)
   is
   begin
      File_Handle.all.Entry_Used := False;

      File_Handle.all.File.all.Handle_Count :=
        File_Handle.all.File.all.Handle_Count - 1;

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Constraint_Error;
         Result := Constraint_Exception;
   end Close_File_Unlocked;

   procedure Close_File
     (File_Handle : Process_File_Handle_Access; Result : out Function_Result)
   is
   begin
      Acquire_Spinlock (Open_Files_Spinlock);
      Close_File_Unlocked (File_Handle, Result);
      Release_Spinlock (Open_Files_Spinlock);
   end Close_File;

   procedure Write_File_Node_Type_Device
     (Process        : in out Process_Control_Block_T;
      File_Handle    : Process_File_Handle_Access;
      Buffer_Address : Virtual_Address_T;
      Bytes_To_Write : Natural;
      Bytes_Written  : out Natural;
      Result         : out Function_Result)
   is
      pragma Unreferenced (Process);
   begin
      Bytes_Written := 0;

      Device : Device_T renames File_Handle.all.File.all.Mounted_Device.all;

      case Device.Driver_Type is
         when Driver_Type_UART    =>
            Data_To_Write : Storage_Array (1 .. Storage_Count (Bytes_To_Write))
            with Import, Alignment => 1, Address => Buffer_Address;

            Devices.UART.Put_Bytes (Device.Common, Device.UART, Data_To_Write);
            Bytes_Written := Bytes_To_Write;
            Result := Success;

         when Driver_Type_Console =>
            Data_To_Write : Storage_Array (1 .. Storage_Count (Bytes_To_Write))
            with Import, Alignment => 1, Address => Buffer_Address;

            Devices.Console.Put_Bytes
              (Device.Common, Device.Console, Data_To_Write);
            Bytes_Written := Bytes_To_Write;
            Result := Success;

         when others              =>
            Log_Error ("Unsupported driver type: " & Device.Driver_Type'Image);
            Result := Not_Supported;
      end case;
   exception
      when Constraint_Error =>
         Log_Constraint_Error;
         Bytes_Written := 0;
         Result := Constraint_Exception;
   end Write_File_Node_Type_Device;

   procedure Write_File_Node_Type_Regular_File
     (Process        : in out Process_Control_Block_T;
      File_Handle    : Process_File_Handle_Access;
      Buffer_Address : Virtual_Address_T;
      Bytes_To_Write : Natural;
      Bytes_Written  : out Natural;
      Result         : out Function_Result) is
   begin
      Bytes_Written := 0;

      --  If the file is opened in append mode, seek to the end of the file.
      if File_Handle.all.File_Open_Flags.Status_Flags.Is_Append_Mode then
         Seek_File (File_Handle, File_Handle.all.File.all.File_Size, Result);
         if Is_Error (Result) then
            return;
         end if;
      end if;

      case File_Handle.all.File.all.Parent_Filesystem.all.Filesystem_Type is
         when Filesystem_Type_FAT =>
            Filesystems.FAT.Write_File
              (File_Handle.all.File.all.Parent_Filesystem,
               Process,
               File_Handle.all.File,
               Buffer_Address,
               File_Handle.all.Position,
               Bytes_To_Write,
               Bytes_Written,
               Result);

         when others              =>
            Result := Not_Supported;
      end case;

      if Is_Error (Result) then
         Bytes_Written := 0;
         return;
      end if;

      File_Handle.all.Position :=
        File_Handle.all.Position + Unsigned_64 (Bytes_Written);

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Constraint_Error;
         Bytes_Written := 0;
         Result := Constraint_Exception;
   end Write_File_Node_Type_Regular_File;

   procedure Write_File
     (Process        : in out Process_Control_Block_T;
      File_Handle    : Process_File_Handle_Access;
      Buffer_Address : Virtual_Address_T;
      Bytes_To_Write : Natural;
      Bytes_Written  : out Natural;
      Result         : out Function_Result) is
   begin
      pragma
        Debug
          (Debug_Filesystems,
           Log_Debug
             ("Filesystems.Write_File: "
              & ASCII.LF
              & "  Bytes_To_Write: "
              & Bytes_To_Write'Image
              & ASCII.LF
              & "  File_Position:  "
              & File_Handle.all.Position'Image));

      if File_Handle.all.File_Open_Flags.Access_Mode = Read_Only then
         Bytes_Written := 0;
         Result := Invalid_File_Permissions;
         return;
      end if;

      --  Note: In future it might make sense to move this check to _after_
      --  the file type dispatching, to ensure that the file type is validated
      --  before the no-op return.
      if Bytes_To_Write = 0 then
         Bytes_Written := 0;
         Result := Success;
         return;
      end if;

      if Bytes_To_Write > Maximum_File_Write_Size then
         Bytes_Written := 0;
         Result := Invalid_File_Size;
         return;
      end if;

      case File_Handle.all.File.all.Node_Type is
         when Filesystem_Node_Type_Regular_File =>
            Write_File_Node_Type_Regular_File
              (Process,
               File_Handle,
               Buffer_Address,
               Bytes_To_Write,
               Bytes_Written,
               Result);

         when Filesystem_Node_Type_Device       =>
            Write_File_Node_Type_Device
              (Process,
               File_Handle,
               Buffer_Address,
               Bytes_To_Write,
               Bytes_Written,
               Result);

         when others                            =>
            Log_Error
              ("Unsupported node type for writing: "
               & File_Handle.all.File.all.Node_Type'Image);
            Bytes_Written := 0;
            Result := Not_Supported;
      end case;

   exception
      when Constraint_Error =>
         Log_Constraint_Error;
         Bytes_Written := 0;
         Result := Constraint_Exception;
   end Write_File;

   procedure Validate_Read_Start_Offset_And_Get_Actual_Bytes_To_Read
     (Filesystem_Node      : Filesystem_Node_Access;
      Start_Offset         : Unsigned_64;
      Bytes_To_Read        : Natural;
      Actual_Bytes_To_Read : out Natural;
      Result               : out Function_Result) is
   begin
      --  Validate that the start offset is within the file size.
      if Start_Offset >= Filesystem_Node.all.File_Size then
         Actual_Bytes_To_Read := 0;
         Result := Invalid_Argument;
         return;
      end if;

      --  Truncate the read if it would exceed the file size.
      Actual_Bytes_To_Read :=
        Natural'Min
          (Natural (Filesystem_Node.all.File_Size - Start_Offset),
           Bytes_To_Read);

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Constraint_Error;
         Actual_Bytes_To_Read := 0;
         Result := Constraint_Exception;
   end Validate_Read_Start_Offset_And_Get_Actual_Bytes_To_Read;

   procedure Validate_Filesystem_And_Node
     (Filesystem      : Filesystem_Access;
      Filesystem_Node : Filesystem_Node_Access;
      Filesystem_Type : Filesystem_Type_T;
      Result          : out Function_Result) is
   begin
      if not Is_Valid_Filesystem_Pointer (Filesystem)
        or else Filesystem.all.Filesystem_Type /= Filesystem_Type
      then
         Log_Error ("Invalid filesystem type");
         Result := Invalid_Argument;
         return;
      end if;

      if Filesystem_Node = null then
         Log_Error ("Filesystem node is null");
         Result := Invalid_Argument;
         return;
      end if;

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Constraint_Error;
         Result := Constraint_Exception;
   end Validate_Filesystem_And_Node;

   procedure Create_File
     (Process  : in out Process_Control_Block_T;
      Path     : Filesystem_Path_T;
      New_Node : out Filesystem_Node_Access;
      Result   : out Function_Result)
   is
      --  The parent directory is Path (Path'First .. Parent_Path_Last), and
      --  the new file's name is Path (Child_Name_First .. Path'Last).
      Parent_Path_Last : Natural := 0;
      Child_Name_First : Natural := 0;

      Parent_File_Node : Filesystem_Node_Access := null;
   begin
      Paths.Split_Path (Path, Parent_Path_Last, Child_Name_First, Result);
      if Is_Error (Result) then
         New_Node := null;
         return;
      end if;

      Parent_File_Path : constant Filesystem_Path_T :=
        Path (Path'First .. Parent_Path_Last);

      Child_File_Name :
        constant Filesystem_Path_T (1 .. Path'Last - Child_Name_First + 1) :=
          Path (Child_Name_First .. Path'Last);

      pragma
        Debug
          (Debug_Filesystems,
           Log_Debug
             ("Filesystems.Create_File: "
              & ASCII.LF
              & "  Parent path: '"
              & Parent_File_Path
              & "'"
              & ASCII.LF
              & "  Child name: '"
              & Child_File_Name
              & "'"));

      Find_File (Process, Parent_File_Path, Parent_File_Node, Result);
      if Result = File_Not_Found then
         pragma
           Debug
             (Debug_Filesystems,
              Log_Debug
                ("Filesystems.Create_File: File not found: '"
                 & Parent_File_Path
                 & "'"));
         New_Node := null;
         return;
      elsif Is_Error (Result) then
         New_Node := null;
         return;
      end if;

      --  Test whether the parent node is one that can actually contain
      --  child nodes (i.e. is a directory or mounted filesystem).
      --  Note that the parent file node pointer should never be null. This
      --  check is simply there for additional safety.
      if Parent_File_Node = null
        or else
          not Can_Filesystem_Node_Contain_Child_Nodes (Parent_File_Node.all)
      then
         pragma
           Debug
             (Debug_Filesystems,
              Log_Debug
                ("Filesystems.Create_File: Node cannot contain child nodes: '"
                 & Parent_File_Path
                 & "'"));

         Result := Invalid_Filename;
         return;
      end if;

      --  If the parent node is a mounted filesystem, then use its filesystem
      --  pointer as the parent filesystem. Otherwise, use the parent node's
      --  parent filesystem.
      Parent_Filesystem : constant Filesystem_Access :=
        (if Parent_File_Node.all.Node_Type
           = Filesystem_Node_Type_Mounted_Filesystem
         then Parent_File_Node.all.Mounted_Filesystem
         else Parent_File_Node.all.Parent_Filesystem);

      case Parent_Filesystem.all.Filesystem_Type is
         when Filesystem_Type_FAT =>
            Filesystems.FAT.Create_File
              (Parent_Filesystem,
               Process,
               Child_File_Name,
               Parent_File_Node,
               New_Node,
               Result);

         when others              =>
            Log_Error
              ("Unsupported filesystem type: "
               & Parent_Filesystem.all.Filesystem_Type'Image);
            Result := Not_Supported;
      end case;

      if Is_Error (Result) then
         return;
      end if;

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Constraint_Error;
         Result := Constraint_Exception;
   end Create_File;

   procedure Get_Sector_Block_Number_And_Offset
     (Sector_Number              : Sector_Index_T;
      Sector_Size                : Natural;
      Block_Number               : out Block_Index_T;
      Sector_Offset_Within_Block : out Storage_Count;
      Result                     : out Function_Result) is
   begin
      Block_Number :=
        Block_Index_T ((Natural (Sector_Number) * Sector_Size) / Block_Size);

      Sector_Offset_Within_Block :=
        Storage_Count
          ((Integer (Sector_Number) mod Get_Sectors_Per_Block (Sector_Size))
           * Sector_Size);

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Constraint_Error;
         Block_Number := 0;
         Sector_Offset_Within_Block := 0;
         Result := Constraint_Exception;
   end Get_Sector_Block_Number_And_Offset;

   procedure Truncate_File
     (Process     : in out Process_Control_Block_T;
      File_Handle : Process_File_Handle_Access;
      New_Size    : Storage_Count;
      Result      : out Function_Result) is
   begin
      if File_Handle.all.File_Open_Flags.Access_Mode = Read_Only then
         Result := Invalid_File_Permissions;
         return;
      end if;

      if File_Handle.all.File.all.Node_Type
        /= Filesystem_Node_Type_Regular_File
      then
         Result := Invalid_File_Type;
         return;
      end if;

      if Unsigned_64 (New_Size) = File_Handle.all.File.all.File_Size then
         Result := Success;
         return;
      elsif New_Size > Maximum_File_Size then
         Result := Invalid_File_Size;
         return;
      end if;

      case File_Handle.all.File.all.Parent_Filesystem.all.Filesystem_Type is
         when Filesystem_Type_FAT =>
            Filesystems.FAT.Truncate_File
              (Process, File_Handle.all.File, New_Size, Result);

         when others              =>
            Log_Error
              ("Unsupported filesystem type: "
               & File_Handle.all.File.all.Parent_Filesystem.all
                   .Filesystem_Type'Image);
            Result := Not_Supported;
      end case;

      if Is_Error (Result) then
         return;
      end if;

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Constraint_Error;
         Result := Constraint_Exception;
   end Truncate_File;

end Filesystems;
