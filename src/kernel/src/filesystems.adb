-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Filesystems.Block_Cache;
with Filesystems.FAT;
with Filesystems.Root;
with Filesystems.UStar;
with Memory.Allocators; use Memory.Allocators;
with Memory.Kernel;     use Memory.Kernel;
with RISCV;
with System_State;      use System_State;

package body Filesystems is
   procedure Add_Filesystem_Node_To_Cache
     (Node : Filesystem_Node_Access; Result : out Function_Result)
   is
      Cache renames Filesystem_Node_Cache;
   begin
      Starting_Index : constant Positive := Cache.Next_Entry_Index;

      loop
         if Can_Filesystem_Cache_Entry_Be_Overwritten
              (Cache.Entries (Cache.Next_Entry_Index))
         then
            Log_Debug
              ("Setting filesystem cache entry at index: "
               & Cache.Next_Entry_Index'Image,
               Logging_Tags);

            Cache.Entries (Cache.Next_Entry_Index).Node := Node;
            Cache.Entries (Cache.Next_Entry_Index).Last_Access :=
              RISCV.Get_System_Time;

            Result := Success;
            return;
         end if;

         Cache.Next_Entry_Index := Cache.Next_Entry_Index + 1;
         if Cache.Next_Entry_Index > Cache.Entries'Last then
            Cache.Next_Entry_Index := Cache.Entries'First;
         end if;

         exit when Cache.Next_Entry_Index = Starting_Index;
      end loop;

      Result := Cache_Exhausted;
   exception
      when Constraint_Error =>
         Log_Error
           ("Constraint_Error: Add_Filesystem_Node_To_Cache", Logging_Tags);
         Result := Constraint_Exception;
   end Add_Filesystem_Node_To_Cache;

   function Can_Filesystem_Cache_Entry_Be_Overwritten
     (Cache_Entry : Filesystem_Node_Cache_Entry_T) return Boolean is
   begin
      --  @TODO: Implement cache eviction policy.
      return Cache_Entry.Node = null and then Cache_Entry.Handle_Count = 0;
   end Can_Filesystem_Cache_Entry_Be_Overwritten;

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

   procedure Create_Filesystem_Node_Cache_Entry
     (Parent_Filesystem : Filesystem_Access;
      Filename          : Wide_String;
      New_Node          : out Filesystem_Node_Access;
      Result            : out Function_Result;
      Index             : Filesystem_Node_Index_T := 0;
      Parent_Index      : Filesystem_Node_Index_T := 0;
      Data_Location     : Unsigned_64 := 0;
      Size              : Unsigned_64 := 0;
      Node_Type         : Filesystem_Node_Type_T := Filesystem_Node_Type_File;
      Filesystem        : Filesystem_Access := null)
   is
      Cache renames Filesystem_Node_Cache;
      Cache_Index : Natural := 0;
   begin
      Find_Free_Cache_Entry (Cache_Index, Result);
      if Is_Error (Result) then
         New_Node := null;
         return;
      end if;

      Allocate_Filesystem_Node (New_Node, Result);
      if Is_Error (Result) then
         New_Node := null;
         return;
      end if;

      New_Node.all :=
        (Index             => Index,
         Parent_Index      => Parent_Index,
         Filename          => [others => Wide_Character'Val (0)],
         Filename_Length   => 0,
         Data_Location     => Data_Location,
         Size              => Size,
         Node_Type         => Node_Type,
         Parent_Filesystem => Parent_Filesystem,
         Filesystem        => Filesystem);

      Set_Filesystem_Node_Name (New_Node.all, Filename, Result);
      if Is_Error (Result) then
         New_Node := null;
         return;
      end if;

      Cache.Entries (Cache_Index).Node := New_Node;
      Cache.Entries (Cache_Index).Last_Access := RISCV.Get_System_Time;
      Cache.Entries (Cache_Index).Handle_Count := 0;

      Log_Debug
        ("Added new filesystem node to cache at index: " & Cache_Index'Image,
         Logging_Tags);

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Create_Filesystem_Node_Cache_Entry");
         Result := Constraint_Exception;
   end Create_Filesystem_Node_Cache_Entry;

   procedure Find_Free_Cache_Entry
     (Cache_Index : out Natural; Result : out Function_Result) is
   begin
      for Index in Filesystem_Node_Cache.Entries'Range loop
         if Can_Filesystem_Cache_Entry_Be_Overwritten
              (Filesystem_Node_Cache.Entries (Index))
         then
            Cache_Index := Index;
            Result := Success;
            return;
         end if;
      end loop;

      Log_Error ("No free cache entry found");
      Result := Cache_Exhausted;
   end Find_Free_Cache_Entry;

   procedure Find_Filesystem_Node_In_Cache
     (Filesystem   : Filesystem_Access;
      Parent_Index : Unsigned_64;
      Filename     : Wide_String;
      Node         : out Filesystem_Node_Access;
      Result       : out Function_Result)
   is
      Cache_Index : Natural := 0;
   begin
      Search_For_Filesystem_Node_In_Cache
        (Filesystem, Parent_Index, Filename, Cache_Index, Result);
      if Is_Error (Result) or else Cache_Index = 0 then
         Node := null;
         return;
      end if;

      Filesystem_Node_Cache.Entries (Cache_Index).Last_Access :=
        RISCV.Get_System_Time;

      Node := Filesystem_Node_Cache.Entries (Cache_Index).Node;
      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error
           ("Constraint_Error: Find_Filesystem_Node_In_Cache", Logging_Tags);
         Result := Constraint_Exception;
   end Find_Filesystem_Node_In_Cache;

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
     (Path              : Wide_String;
      Start_Index       : in out Integer;
      End_Index         : out Integer;
      Next_Token_Length : out Integer) is
   begin
      Next_Token_Length := 0;

      --  Skip leading slashes.
      while Start_Index <= Path'Last loop
         exit when Path (Start_Index) /= '/';
         Start_Index := Start_Index + 1;
      end loop;

      End_Index := Start_Index;

      while End_Index <= Path'Last loop
         exit when Path (End_Index) = '/';

         End_Index := End_Index + 1;
      end loop;

      Next_Token_Length := End_Index - Start_Index;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Get_Next_Path_Component", Logging_Tags);
   end Get_Next_Path_Component;

   function Is_Searchable_Filesystem_Node
     (Node : Filesystem_Node_Access) return Boolean is
   begin
      return
        Node /= null
        and then (Node.all.Node_Type = Filesystem_Node_Type_Directory
                  or else Node.all.Node_Type
                          = Filesystem_Node_Type_Mounted_Filesystem);
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Is_Searchable_Filesystem_Node");
         return False;
   end Is_Searchable_Filesystem_Node;

   procedure Find_File
     (Process         : in out Process_Control_Block_T;
      Path            : Wide_String;
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
      if Path (Path'First) = '/' then
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
            if not Is_Searchable_Filesystem_Node (Filesystem_Node_Parent) then
               Log_Debug ("Unable to traverse node.", Logging_Tags);
               Filesystem_Node := null;
               exit;
            end if;
         end if;

         declare
            --  Create a new Wide_String for the next path token.
            --  This is ideal because string slicing in Ada keeps the original
            --  string indexes, which can be more complicated to work with.
            Next_Path_Token : constant Wide_String (1 .. Next_Token_Length) :=
              Path (Next_Token_Start_Index .. Next_Token_End_Index - 1);
         begin
            Log_Debug_Wide
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

            if Filesystem_Node /= null then
               Log_Debug_Wide ("Found node in system cache.", Logging_Tags);
            else
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
         end;

         if Filesystem_Node = null then
            exit;
         end if;

         if Filesystem_Node.all.Node_Type
           = Filesystem_Node_Type_Mounted_Filesystem
         then
            Log_Debug ("Filesystem node found.", Logging_Tags);
            Current_Filesystem := Filesystem_Node.all.Filesystem;
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

   procedure Open_File
     (Process     : in out Process_Control_Block_T;
      Path        : Wide_String;
      Mode        : File_Open_Mode_T;
      File_Handle : out Process_File_Handle_Access;
      Result      : out Function_Result)
   is
      Filesystem_Node : Filesystem_Node_Access := null;

      File_Handle_Index : Positive := 1;
   begin
      Log_Debug_Wide
        ("System_State.Filesystem: Open_File: '" & Path & "'", Logging_Tags);

      Find_File (Process, Path, Filesystem_Node, Result);
      if Is_Error (Result) then
         File_Handle := null;
         return;
      elsif Result = File_Not_Found then
         Log_Debug ("File not found", Logging_Tags);
         File_Handle := null;
         return;
      end if;

      Log_Debug ("File found", Logging_Tags);

      Find_Unused_File_Handle_Entry (Open_Files, File_Handle_Index, Result);
      if Is_Error (Result) then
         return;
      end if;

      File_Handle := Open_Files (File_Handle_Index)'Access;

      File_Handle.all.File_Handle_Id := Unsigned_64 (File_Handle_Index);
      File_Handle.all.Entry_Used := True;
      File_Handle.all.File := Filesystem_Node;
      File_Handle.all.Position := 0;
      File_Handle.all.Mode := Mode;
      File_Handle.all.Process_Id := Process.Process_Id;

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Open_File");
         Result := Constraint_Exception;
   end Open_File;

   procedure Read_File
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
      Log_Debug
        ("Read_File: "
         & ASCII.LF
         & "  Bytes_To_Read: "
         & Bytes_To_Read'Image
         & ASCII.LF
         & "  File_Position: "
         & File_Handle.all.Position'Image,
         Logging_Tags);

      if Bytes_To_Read > Maximum_File_Read_Size or else Bytes_To_Read = 0 then
         Log_Error
           ("Read_File: Requested read size too large: "
            & Bytes_To_Read'Image);
         Bytes_Read := 0;
         Result := Invalid_Argument;
         return;
      end if;

      Remaining_Bytes_In_File :=
        Natural (File_Handle.all.File.all.Size - File_Handle.all.Position);

      if Bytes_To_Read > Remaining_Bytes_In_File then
         Real_Bytes_To_Read := Remaining_Bytes_In_File;
      else
         Real_Bytes_To_Read := Bytes_To_Read;
      end if;

      if Real_Bytes_To_Read = 0 then
         Log_Debug ("No bytes to read.", Logging_Tags);
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
            Result := Invalid_Argument;
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
         Log_Error ("Constraint_Error: Read_File");
         Result := Constraint_Exception;
   end Read_File;

   procedure Search_For_Filesystem_Node_In_Cache
     (Filesystem   : Filesystem_Access;
      Parent_Index : Unsigned_64;
      Filename     : Wide_String;
      Cache_Index  : out Natural;
      Result       : out Function_Result)
   is
      Cache renames Filesystem_Node_Cache;
   begin
      Cache_Index := 0;

      for Index in Cache.Entries'Range loop
         if Cache.Entries (Index).Node /= null then
            if Filesystem = Cache.Entries (Index).Node.all.Parent_Filesystem
              and then Parent_Index
                       = Cache.Entries (Index).Node.all.Parent_Index
              and then Compare_Node_Name_With_Wide_String
                         (Cache.Entries (Index).Node.all.Filename,
                          Cache.Entries (Index).Node.all.Filename_Length,
                          Filename)
            then
               Cache_Index := Index;
               exit;
            end if;
         end if;
      end loop;

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error
           ("Constraint_Error: Find_Filesystem_Node_In_Cache", Logging_Tags);
         Result := Constraint_Exception;
   end Search_For_Filesystem_Node_In_Cache;

   procedure Seek_File
     (File_Handle : Process_File_Handle_Access;
      New_Offset  : Unsigned_64;
      Result      : out Function_Result) is
   begin
      if New_Offset > File_Handle.all.File.all.Size then
         Log_Error
           ("Seek_File: New offset is beyond end of file", Logging_Tags);
         Result := Invalid_Argument;
         return;
      end if;

      File_Handle.all.Position := New_Offset;
      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Seek_File", Logging_Tags);
         Result := Constraint_Exception;
   end Seek_File;

   procedure Set_Filesystem_Node_Name
     (Node      : in out Filesystem_Node_T;
      Node_Name : Wide_String;
      Result    : out Function_Result) is
   begin
      if Node_Name'Length > Filesystem_Node_Path_T'Last then
         Log_Error
           ("Filesystem node name too long: " & Node_Name'Length'Image);
         Result := Invalid_Argument;
         return;
      end if;

      Node.Filename_Length := Node_Name'Length;
      for Index in Node_Name'Range loop
         Node.Filename (Index) := Node_Name (Index);
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
     (Sector_Number : Unsigned_64; Sector_Size : Natural) return Unsigned_64 is
   begin
      return
        Unsigned_64 ((Natural (Sector_Number) * Sector_Size) / Block_Size);
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Sector_To_Block");
         return 0;
   end Sector_To_Block;

   function Sectors_Per_Block (Sector_Size : Natural) return Natural is
   begin
      return Block_Size / Sector_Size;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Sectors_Per_Block");
         return 0;
   end Sectors_Per_Block;

   function Get_Sector_Offset_Within_Block
     (Sector_Number : Unsigned_64; Sector_Size : Natural) return Storage_Offset
   is
   begin
      return
        Storage_Offset
          ((Integer (Sector_Number) mod Sectors_Per_Block (Sector_Size))
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

      Log_Debug ("Initialised block cache.", Logging_Tags);
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

end Filesystems;
