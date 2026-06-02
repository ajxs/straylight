-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Filesystems.Block_Cache;
with Filesystems.FAT;
with Filesystems.Root;
with Filesystems.UStar;
with Filesystems.Node_Cache; use Filesystems.Node_Cache;
with Memory.Allocators;      use Memory.Allocators;
with Memory.Kernel;          use Memory.Kernel;
with Hart_State;             use Hart_State;

package body Filesystems is
   function Compare_Node_Name_With_Wide_String
     (Name1 : Wide_String; Name1_Length : Integer; Name2 : Wide_String)
      return Boolean is
   begin
      if Name1_Length /= Name2'Length then
         return False;
      end if;

      for Index in Name2'Range loop
         if Name1 (Index) /= Name2 (Index) then
            return False;
         end if;
      end loop;

      return True;
   exception
      when Constraint_Error =>
         Log_Error
           ("Constraint_Error: Compare_Node_Name_With_Wide_String",
            Logging_Tags);
         return False;
   end Compare_Node_Name_With_Wide_String;

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

   procedure Get_Next_Path_Component
     (Path                   : Filesystem_Path_T;
      Start_Index            : in out Integer;
      End_Index              : out Integer;
      Next_Token_Byte_Length : out Integer) is
   begin
      Next_Token_Byte_Length := 0;

      --  Skip any leading slashes.
      while Start_Index <= Path'Last loop
         exit when Path (Start_Index) /= Filesystem_Node_Separator;
         Start_Index := Start_Index + 1;
      end loop;

      End_Index := Start_Index;

      while End_Index <= Path'Last loop
         exit when Path (End_Index) = Filesystem_Node_Separator;

         End_Index := End_Index + 1;
      end loop;

      Next_Token_Byte_Length := End_Index - Start_Index;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Get_Next_Path_Component", Logging_Tags);
   end Get_Next_Path_Component;

   function Can_Filesystem_Node_Contain_Child_Nodes
     (Node : Filesystem_Node_Access) return Boolean is
   begin
      return
        Node /= null
        and then
          (Node.all.Node_Type = Filesystem_Node_Type_Directory
           or else
             Node.all.Node_Type = Filesystem_Node_Type_Mounted_Filesystem);
   exception
      when Constraint_Error =>
         Log_Error
           ("Constraint_Error: Can_Filesystem_Node_Contain_Child_Nodes");
         return False;
   end Can_Filesystem_Node_Contain_Child_Nodes;

   procedure Find_File
     (Process         : in out Process_Control_Block_T;
      Path            : Filesystem_Path_T;
      Filesystem_Node : out Filesystem_Node_Access;
      Result          : out Function_Result)
   is
      Current_Filesystem : Filesystem_Access := null;

      Filesystem_Node_Parent : Filesystem_Node_Access := null;

      Filesystem_Node_Parent_Index : Unsigned_64 := 0;

      Next_Token_Start_Index : Integer := 1;
      Next_Token_End_Index   : Integer := 1;
      Next_Token_Length      : Integer := 1;
   begin
      Log_Debug ("Filesystems.Find_File: '" & Path & "'", Logging_Tags);

      if Path (Path'First) = Filesystem_Node_Separator then
         --  Absolute path; start at root filesystem.
         Current_Filesystem := System_Root_Filesystem;
         --  The root node index of the in-memory root filesystem is 1.
         Filesystem_Node_Parent_Index := 1;
      else
         --  Relative path; start at process's current working directory.
         Log_Error ("Relative paths not yet implemented.");
         Result := File_Not_Found;
         return;
      end if;

      loop
         Get_Next_Path_Component
           (Path,
            Next_Token_Start_Index,
            Next_Token_End_Index,
            Next_Token_Length);

         exit when Next_Token_Length = 0;

         if Filesystem_Node_Parent /= null then
            --  Ensure we can traverse the parent node.
            if not Can_Filesystem_Node_Contain_Child_Nodes
                     (Filesystem_Node_Parent)
            then
               Log_Debug ("Unable to traverse node.", Logging_Tags);
               Filesystem_Node := null;
               exit;
            end if;
         end if;

         --  Create a new string for the next path token.
         --  This is ideal because string slicing in Ada keeps the original
         --  string indexes, which can be more complicated to work with.
         Next_Path_Token :
           constant Filesystem_Path_T (1 .. Next_Token_Length) :=
             Path (Next_Token_Start_Index .. Next_Token_End_Index - 1);

         Log_Debug
           ("Current path token: '" & Next_Path_Token & "'", Logging_Tags);

         Find_Filesystem_Node_In_Cache
           (Current_Filesystem,
            Filesystem_Node_Parent_Index,
            Next_Path_Token,
            Filesystem_Node,
            Result);
         if Is_Error (Result) then
            return;
         end if;

         if Result = Cache_Entry_Not_Found then
            case Current_Filesystem.all.Filesystem_Type is
               when Filesystem_Type_UStar =>
                  Filesystems.UStar.Find_File
                    (Current_Filesystem,
                     Process,
                     Next_Path_Token,
                     Filesystem_Node_Parent,
                     Filesystem_Node,
                     Result);

               when Filesystem_Type_Root  =>
                  Filesystems.Root.Find_File
                    (Current_Filesystem,
                     Next_Path_Token,
                     Filesystem_Node_Parent_Index,
                     Filesystem_Node,
                     Result);

               when Filesystem_Type_FAT   =>
                  Filesystems.FAT.Find_File
                    (Current_Filesystem,
                     Process,
                     Next_Path_Token,
                     Filesystem_Node_Parent,
                     Filesystem_Node,
                     Result);

               when others                =>
                  Log_Error
                    ("Unsupported filesystem type: "
                     & Current_Filesystem.all.Filesystem_Type'Image);
                  Result := File_Not_Found;
            end case;

            if Is_Error (Result) then
               return;
            end if;
         end if;

         if Filesystem_Node = null then
            Log_Debug
              ("Filesystem node not found for token: '"
               & Path (Next_Token_Start_Index .. Next_Token_End_Index - 1)
               & "'",
               Logging_Tags);
            exit;
         end if;

         if Filesystem_Node.all.Node_Type
           = Filesystem_Node_Type_Mounted_Filesystem
         then
            Log_Debug ("Filesystem node found.", Logging_Tags);
            Current_Filesystem := Filesystem_Node.all.Mounted_Filesystem;
            if Current_Filesystem = null then
               Log_Error ("Mounted filesystem has null filesystem pointer");
               Filesystem_Node := null;
               exit;
            end if;
         end if;

         Next_Token_Start_Index := Next_Token_End_Index;
         Filesystem_Node_Parent := Filesystem_Node;
         Filesystem_Node_Parent_Index := Filesystem_Node.all.Index;
      end loop;

      if Filesystem_Node = null then
         Result := File_Not_Found;
         return;
      end if;

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Find_File");
         Result := Constraint_Exception;
   end Find_File;

   procedure Create_File_Handle_For_Filesystem_Node_Unlocked
     (Process         : in out Process_Control_Block_T;
      Filesystem_Node : Filesystem_Node_Access;
      Mode            : File_Open_Mode_T;
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
      File_Handle.all.Mode := Mode;
      File_Handle.all.Process_Id := Process.Process_Id;

      Filesystem_Node.all.Handle_Count := Filesystem_Node.all.Handle_Count + 1;

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error
           ("Constraint_Error: "
            & "Create_File_Handle_For_Filesystem_Node_Unlocked");
         Result := Constraint_Exception;
   end Create_File_Handle_For_Filesystem_Node_Unlocked;

   procedure Create_File_Handle_For_Filesystem_Node
     (Process         : in out Process_Control_Block_T;
      Filesystem_Node : Filesystem_Node_Access;
      Mode            : File_Open_Mode_T;
      File_Handle     : out Process_File_Handle_Access;
      Result          : out Function_Result) is
   begin
      Acquire_Spinlock (Open_Files_Spinlock);
      Acquire_Spinlock (Process.Spinlock);

      Create_File_Handle_For_Filesystem_Node_Unlocked
        (Process, Filesystem_Node, Mode, File_Handle, Result);

      Release_Spinlock (Process.Spinlock);
      Release_Spinlock (Open_Files_Spinlock);
   end Create_File_Handle_For_Filesystem_Node;

   procedure Open_File
     (Process     : in out Process_Control_Block_T;
      Path        : Filesystem_Path_T;
      Mode        : File_Open_Mode_T;
      File_Handle : out Process_File_Handle_Access;
      Result      : out Function_Result)
   is
      Filesystem_Node : Filesystem_Node_Access := null;
   begin
      Log_Debug ("Filesystems.Open_File: '" & Path & "'", Logging_Tags);

      Find_File (Process, Path, Filesystem_Node, Result);
      if Is_Error (Result) then
         File_Handle := null;
         return;
      elsif Result = File_Not_Found then
         Log_Debug ("File not found: '" & Path & "'", Logging_Tags);

         if not Mode.Create then
            File_Handle := null;
            return;
         end if;

         Log_Debug ("Creating missing file: '" & Path & "'", Logging_Tags);
         Create_File (Process, Path, Result);
         if Is_Error (Result) then
            File_Handle := null;
            return;
         end if;

         Find_File (Process, Path, Filesystem_Node, Result);
         if Is_Error (Result) then
            File_Handle := null;
            return;
         elsif Result = File_Not_Found then
            Log_Error ("File not found after creation: '" & Path & "'");
            File_Handle := null;
            return;
         end if;
      end if;

      Log_Debug ("File found: '" & Path & "'", Logging_Tags);

      Create_File_Handle_For_Filesystem_Node
        (Process, Filesystem_Node, Mode, File_Handle, Result);
      if Is_Error (Result) then
         File_Handle := null;
         return;
      end if;

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Open_File");
         Result := Constraint_Exception;
   end Open_File;

   procedure Read_File_Node_Type_File
     (Process        : in out Process_Control_Block_T;
      File_Handle    : Process_File_Handle_Access;
      Buffer_Address : Virtual_Address_T;
      Bytes_To_Read  : Natural;
      Bytes_Read     : out Natural;
      Result         : out Function_Result)
   is
      Real_Bytes_To_Read      : Natural := 0;
      Remaining_Bytes_In_File : Natural := 0;
   begin
      Remaining_Bytes_In_File :=
        Natural (File_Handle.all.File.all.Size - File_Handle.all.Position);

      if Bytes_To_Read > Remaining_Bytes_In_File then
         Real_Bytes_To_Read := Remaining_Bytes_In_File;
      else
         Real_Bytes_To_Read := Bytes_To_Read;
      end if;

      if Real_Bytes_To_Read = 0 then
         Log_Debug ("Filesystems.Read_File: No bytes to read.", Logging_Tags);
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
         Log_Error ("Constraint_Error: Read_File_Node_Type_File");
         Result := Constraint_Exception;
   end Read_File_Node_Type_File;

   procedure Read_File
     (Process        : in out Process_Control_Block_T;
      File_Handle    : Process_File_Handle_Access;
      Buffer_Address : Virtual_Address_T;
      Bytes_To_Read  : Natural;
      Bytes_Read     : out Natural;
      Result         : out Function_Result) is
   begin
      Log_Debug
        ("Filesystems.Read_File: "
         & ASCII.LF
         & "  Bytes_To_Read: "
         & Bytes_To_Read'Image
         & ASCII.LF
         & "  File_Position: "
         & File_Handle.all.Position'Image,
         Logging_Tags);

      if Bytes_To_Read > Maximum_File_Read_Size or else Bytes_To_Read = 0 then
         Log_Error
           ("Filesystems.Read_File: Invalid Bytes_To_Read: "
            & Bytes_To_Read'Image);
         Bytes_Read := 0;
         Result := Invalid_Argument;
         return;
      end if;

      case File_Handle.all.File.all.Node_Type is
         when Filesystem_Node_Type_File =>
            Read_File_Node_Type_File
              (Process,
               File_Handle,
               Buffer_Address,
               Bytes_To_Read,
               Bytes_Read,
               Result);

         when others                    =>
            Log_Error
              ("Unsupported node type for reading: "
               & File_Handle.all.File.all.Node_Type'Image);
            Bytes_Read := 0;
            Result := Function_Results.Not_Supported;
      end case;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Read_File");
         Bytes_Read := 0;
         Result := Constraint_Exception;
   end Read_File;

   procedure Seek_File
     (File_Handle : Process_File_Handle_Access;
      New_Offset  : Unsigned_64;
      Result      : out Function_Result)
   is
      Effective_New_Offset : Unsigned_64 := New_Offset;
   begin
      if New_Offset > File_Handle.all.File.all.Size then
         if File_Handle.all.File.all.Size = 0 then
            Log_Debug
              ("Seek_File: File size is 0, setting offset to 0", Logging_Tags);
            Effective_New_Offset := 0;
         else
            Log_Debug
              ("Seek_File: New offset is beyond end of file", Logging_Tags);
            Effective_New_Offset := File_Handle.all.File.all.Size - 1;
         end if;
      end if;

      File_Handle.all.Position := Effective_New_Offset;
      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Seek_File", Logging_Tags);
         Result := Constraint_Exception;
   end Seek_File;

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
         Log_Error ("Constraint_Error: Set_Filesystem_Node_Name");
         Result := Constraint_Exception;
   end Set_Filesystem_Node_Name;

   function Is_Valid_Filesystem_Pointer
     (Filesystem : Filesystem_Access) return Boolean is
   begin
      return
        Filesystem /= null
        and then Filesystem.all.Device /= null
        and then Filesystem.all.Device.all.Device_Class = Device_Class_Storage;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Is_Valid_Filesystem_Pointer");
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
         Log_Error ("Constraint_Error: Sector_To_Block");
         return 0;
   end Sector_To_Block;

   function Get_Sectors_Per_Block (Sector_Size : Natural) return Natural is
   begin
      return Block_Size / Sector_Size;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Get_Sectors_Per_Block");
         return 0;
   end Get_Sectors_Per_Block;

   function Get_Sector_Offset_Within_Block
     (Sector_Number : Sector_Index_T; Sector_Size : Natural)
      return Storage_Offset is
   begin
      return
        Storage_Offset
          ((Integer (Sector_Number) mod Get_Sectors_Per_Block (Sector_Size))
           * Sector_Size);
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Get_Sector_Offset_Within_Block");
         return 0;
   end Get_Sector_Offset_Within_Block;

   procedure Initialise_Block_Cache is
      Result            : Function_Result := Unset;
      Allocation_Result : Memory_Allocation_Result;
      System_Block_Cache renames Filesystems.Block_Cache.System_Block_Cache;
   begin
      Log_Debug ("Initialising block cache...", Logging_Tags);

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

      Log_Debug
        ("Initialised block cache:"
         & ASCII.LF
         & "  Data Physical Address: "
         & System_Block_Cache.Data_Address_Physical'Image
         & ASCII.LF
         & "  Data Virtual Address:  "
         & System_Block_Cache.Data_Address_Virtual'Image,
         Logging_Tags);
   exception
      when Constraint_Error =>
         Panic ("Constraint_Error: Initialise_Block_Cache");
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
         Log_Error ("Constraint_Error: Allocate_Filesystem_Node");
         New_Node := null;
         Result := Constraint_Exception;
   end Allocate_Filesystem_Node;

   procedure Find_File_Handle
     (Process_Id     : Process_Id_T;
      File_Handle_Id : Unsigned_64;
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
      Log_Debug ("Filesystems.Close_File_Unlocked", Logging_Tags);

      File_Handle.all.Entry_Used := False;

      File_Handle.all.File.all.Handle_Count :=
        File_Handle.all.File.all.Handle_Count - 1;

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Close_File_Unlocked", Logging_Tags);
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

   procedure Write_File_Node_Type_File
     (Process        : in out Process_Control_Block_T;
      File_Handle    : Process_File_Handle_Access;
      Buffer_Address : Virtual_Address_T;
      Bytes_To_Write : Natural;
      Bytes_Written  : out Natural;
      Result         : out Function_Result) is
   begin
      Bytes_Written := 0;

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
            Log_Error
              ("Unsupported filesystem type: "
               & File_Handle.all.File.all.Parent_Filesystem.all
                   .Filesystem_Type'Image);
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
         Log_Error
           ("Constraint_Error: Write_File_Node_Type_File", Logging_Tags);
         Bytes_Written := 0;
         Result := Constraint_Exception;
   end Write_File_Node_Type_File;

   procedure Write_File
     (Process        : in out Process_Control_Block_T;
      File_Handle    : Process_File_Handle_Access;
      Buffer_Address : Virtual_Address_T;
      Bytes_To_Write : Natural;
      Bytes_Written  : out Natural;
      Result         : out Function_Result) is
   begin
      Log_Debug
        ("Filesystems.Write_File: "
         & ASCII.LF
         & "  Bytes_To_Write: "
         & Bytes_To_Write'Image
         & ASCII.LF
         & "  File_Position:  "
         & File_Handle.all.Position'Image,
         Logging_Tags);

      if Bytes_To_Write > Maximum_File_Write_Size or else Bytes_To_Write = 0
      then
         Log_Error
           ("Filesystems.Write_File: Invalid Bytes_To_Write: "
            & Bytes_To_Write'Image);
         Bytes_Written := 0;
         Result := Invalid_Argument;
         return;
      end if;

      case File_Handle.all.File.all.Node_Type is
         when Filesystem_Node_Type_File =>
            Write_File_Node_Type_File
              (Process,
               File_Handle,
               Buffer_Address,
               Bytes_To_Write,
               Bytes_Written,
               Result);

         when others                    =>
            Log_Error
              ("Unsupported node type for writing: "
               & File_Handle.all.File.all.Node_Type'Image);
            Bytes_Written := 0;
            Result := Not_Supported;
      end case;

   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Write_File", Logging_Tags);
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
      --  Validate that the read doesn't exceed the file size.
      if Start_Offset >= Filesystem_Node.all.Size then
         Log_Error
           ("Read offset exceeds file size: "
            & Start_Offset'Image
            & " >= "
            & Filesystem_Node.all.Size'Image,
            Logging_Tags);

         Actual_Bytes_To_Read := 0;
         Result := Invalid_Argument;
         return;
      end if;

      --  Truncate the read if it would exceed the file size.
      if Start_Offset + Unsigned_64 (Bytes_To_Read) > Filesystem_Node.all.Size
      then
         Actual_Bytes_To_Read :=
           Natural (Filesystem_Node.all.Size - Start_Offset);

         Log_Debug
           ("Truncating read from "
            & Bytes_To_Read'Image
            & " to "
            & Actual_Bytes_To_Read'Image
            & " bytes to stay within file size",
            Logging_Tags);
      else
         Actual_Bytes_To_Read := Bytes_To_Read;
      end if;

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error
           ("Constraint_Error: "
            & "Validate_Read_Start_Offset_And_Get_Actual_Bytes_To_Read",
            Logging_Tags);
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
         Log_Error ("Invalid filesystem type", Logging_Tags);
         Result := Invalid_Argument;
         return;
      end if;

      if Filesystem_Node = null then
         Log_Error ("Filesystem node is null", Logging_Tags);
         Result := Invalid_Argument;
         return;
      end if;

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error
           ("Constraint_Error: Validate_Filesystem_And_Node", Logging_Tags);
         Result := Constraint_Exception;
   end Validate_Filesystem_And_Node;

   procedure Create_File
     (Process : in out Process_Control_Block_T;
      Path    : Filesystem_Path_T;
      Result  : out Function_Result)
   is
      Parent_Path_Length : Integer := 0;

      Parent_Filesystem : Filesystem_Access := null;
      Parent_File_Node  : Filesystem_Node_Access := null;
      New_File_Node     : Filesystem_Node_Access := null;
   begin
      for I in reverse Path'Range loop
         if Path (I) = Filesystem_Node_Separator then
            Parent_Path_Length := I - Path'First;
            exit;
         end if;
      end loop;

      Parent_File_Path :
        constant Filesystem_Path_T
                   (Path'First .. Path'First + Parent_Path_Length - 1) :=
          Path (Path'First .. Path'First + Parent_Path_Length - 1);

      Child_File_Name :
        constant Filesystem_Path_T (1 .. Path'Last - Parent_Path_Length - 1) :=
          Path (Path'First + Parent_Path_Length + 1 .. Path'Last);

      Log_Debug
        ("Filesystems.Create_File: "
         & ASCII.LF
         & "  Parent path: '"
         & Parent_File_Path
         & "'"
         & ASCII.LF
         & "  Child name: '"
         & Child_File_Name
         & "'",
         Logging_Tags);

      Find_File (Process, Parent_File_Path, Parent_File_Node, Result);
      if Is_Error (Result) then
         return;
      elsif Result = File_Not_Found then
         Log_Debug
           ("Filesystems.Create_File: File not found: '"
            & Parent_File_Path
            & "'",
            Logging_Tags);
         return;
      end if;

      --  Test whether the parent node is one that can actually contain
      --  child nodes (i.e. is a directory or mounted filesystem).
      if not Can_Filesystem_Node_Contain_Child_Nodes (Parent_File_Node) then
         Log_Debug
           ("Filesystems.Create_File: Node cannot contain child nodes: '"
            & Parent_File_Path
            & "'",
            Logging_Tags);

         Result := Invalid_Filename;
         return;
      end if;

      --  If the parent node is a mounted filesystem, then use its filesystem
      --  pointer as the parent filesystem. Otherwise, use the parent node's
      --  parent filesystem.
      Parent_Filesystem :=
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
               New_File_Node,
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
         Log_Error ("Constraint_Error: Create_File", Logging_Tags);
         Result := Constraint_Exception;
   end Create_File;

end Filesystems;
