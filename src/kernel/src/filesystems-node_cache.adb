with Memory.Kernel; use Memory.Kernel;
with RISCV;

package body Filesystems.Node_Cache is
   procedure Add_Filesystem_Node_To_Cache_Unlocked
     (Node : Filesystem_Node_Access; Result : out Function_Result)
   is
      Cache_Index : Natural := 0;
   begin
      Find_Free_Cache_Entry (Cache_Index, Result);
      if Is_Error (Result) then
         return;
      end if;

      Node_Cache_Entries (Cache_Index).Node := Node;
      Node_Cache_Entries (Cache_Index).Last_Access := RISCV.Get_System_Time;

      Result := Success;

   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Add_Filesystem_Node_To_Cache_Unlocked");
         Result := Constraint_Exception;
   end Add_Filesystem_Node_To_Cache_Unlocked;

   procedure Add_Filesystem_Node_To_Cache
     (Node : Filesystem_Node_Access; Result : out Function_Result) is
   begin
      Acquire_Spinlock (Node_Cache_Spinlock);
      Add_Filesystem_Node_To_Cache_Unlocked (Node, Result);
      Release_Spinlock (Node_Cache_Spinlock);
   end Add_Filesystem_Node_To_Cache;

   procedure Create_Filesystem_Node_Cache_Entry_Unlocked
     (Parent_Filesystem  : Filesystem_Access;
      Filename           : Filesystem_Path_T;
      New_Node           : out Filesystem_Node_Access;
      Result             : out Function_Result;
      Index              : Filesystem_Node_Index_T;
      Parent_Index       : Filesystem_Node_Index_T;
      Data_Location      : Unsigned_64;
      File_Size          : Unsigned_64;
      Node_Type          : Filesystem_Node_Type_T;
      Mounted_Device     : Device_Access;
      Mounted_Filesystem : Filesystem_Access)
   is
      Cache_Index        : Natural := 0;
      Free_Memory_Result : Function_Result := Unset;
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
        (Index              => Index,
         Parent_Index       => Parent_Index,
         Filename           => Null_Filesystem_Node_Name,
         Data_Location      => Data_Location,
         File_Size          => File_Size,
         Node_Type          => Node_Type,
         Parent_Filesystem  => Parent_Filesystem,
         Mounted_Device     => Mounted_Device,
         Mounted_Filesystem => Mounted_Filesystem,
         Handle_Count       => 0);

      Set_Filesystem_Node_Name (New_Node.all, Filename, Result);
      if Is_Error (Result) then
         Free_Kernel_Memory (New_Node'Address, Free_Memory_Result);
         New_Node := null;
         return;
      end if;

      Node_Cache_Entries (Cache_Index).Node := New_Node;
      Node_Cache_Entries (Cache_Index).Last_Access := RISCV.Get_System_Time;

      Log_Debug
        ("Added new filesystem node to cache at index: " & Cache_Index'Image,
         Logging_Tags_Node_Cache);

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error
           ("Constraint_Error: Create_Filesystem_Node_Cache_Entry_Unlocked");
         Result := Constraint_Exception;
   end Create_Filesystem_Node_Cache_Entry_Unlocked;

   procedure Create_Filesystem_Node_Cache_Entry
     (Parent_Filesystem  : Filesystem_Access;
      Filename           : Filesystem_Path_T;
      New_Node           : out Filesystem_Node_Access;
      Result             : out Function_Result;
      Index              : Filesystem_Node_Index_T := 0;
      Parent_Index       : Filesystem_Node_Index_T := 0;
      Data_Location      : Unsigned_64 := 0;
      File_Size          : Unsigned_64 := 0;
      Node_Type          : Filesystem_Node_Type_T :=
        Filesystem_Node_Type_Regular_File;
      Mounted_Device     : Device_Access := null;
      Mounted_Filesystem : Filesystem_Access := null) is
   begin
      Acquire_Spinlock (Node_Cache_Spinlock);
      Create_Filesystem_Node_Cache_Entry_Unlocked
        (Parent_Filesystem,
         Filename,
         New_Node,
         Result,
         Index,
         Parent_Index,
         Data_Location,
         File_Size,
         Node_Type,
         Mounted_Device,
         Mounted_Filesystem);
      Release_Spinlock (Node_Cache_Spinlock);
   end Create_Filesystem_Node_Cache_Entry;

   --  Assumes that the caller already holds the cache spinlock.
   procedure Find_Free_Cache_Entry
     (Cache_Index : out Natural; Result : out Function_Result) is
   begin
      Cache_Index := 0;

      for Index in Node_Cache_Entries'Range loop
         if Node_Cache_Entries (Index).Node = null then
            Cache_Index := Index;
            Result := Success;
            return;
         end if;
      end loop;

      for Index in Node_Cache_Entries'Range loop
         if Can_Filesystem_Cache_Entry_Be_Overwritten
              (Node_Cache_Entries (Index))
         then
            Cache_Index := Index;
            Result := Success;
            return;
         end if;
      end loop;

      Log_Error ("No free cache entry found");
      Result := Cache_Exhausted;
   end Find_Free_Cache_Entry;

   procedure Find_Filesystem_Node_In_Cache_Unlocked
     (Filesystem   : Filesystem_Access;
      Parent_Index : Unsigned_64;
      Filename     : Filesystem_Path_T;
      Node         : out Filesystem_Node_Access;
      Result       : out Function_Result)
   is
      Cache_Index : Natural := 0;
   begin
      Log_Debug
        ("Searching for filesystem node in cache with name: '"
         & Filename
         & "'",
         Logging_Tags_Node_Cache);

      Search_For_Filesystem_Node_In_Cache
        (Filesystem, Parent_Index, Filename, Cache_Index, Result);
      if Is_Error (Result) or else Result = Cache_Entry_Not_Found then
         Node := null;
         return;
      end if;

      Node_Cache_Entries (Cache_Index).Last_Access := RISCV.Get_System_Time;

      Node := Node_Cache_Entries (Cache_Index).Node;

      Log_Debug
        ("Found node in system cache: '"
         & Node.all.Filename.Value (1 .. Node.all.Filename.Byte_Length)
         & "'",
         Logging_Tags_Node_Cache);

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error
           ("Constraint_Error: Find_Filesystem_Node_In_Cache_Unlocked");
         Result := Constraint_Exception;
   end Find_Filesystem_Node_In_Cache_Unlocked;

   procedure Find_Filesystem_Node_In_Cache
     (Filesystem   : Filesystem_Access;
      Parent_Index : Unsigned_64;
      Filename     : Filesystem_Path_T;
      Node         : out Filesystem_Node_Access;
      Result       : out Function_Result) is
   begin
      Acquire_Spinlock (Node_Cache_Spinlock);
      Find_Filesystem_Node_In_Cache_Unlocked
        (Filesystem, Parent_Index, Filename, Node, Result);
      Release_Spinlock (Node_Cache_Spinlock);
   end Find_Filesystem_Node_In_Cache;

   function Can_Filesystem_Cache_Entry_Be_Overwritten
     (Cache_Entry : Filesystem_Node_Cache_Entry_T) return Boolean is
   begin
      if Cache_Entry.Node = null then
         return True;
      end if;

      return Cache_Entry.Node.all.Handle_Count = 0;
   exception
      when Constraint_Error =>
         Log_Error
           ("Constraint_Error: Can_Filesystem_Cache_Entry_Be_Overwritten");
         return False;
   end Can_Filesystem_Cache_Entry_Be_Overwritten;

   procedure Search_For_Filesystem_Node_In_Cache
     (Filesystem   : Filesystem_Access;
      Parent_Index : Unsigned_64;
      Filename     : Filesystem_Path_T;
      Cache_Index  : out Natural;
      Result       : out Function_Result) is
   begin
      Cache_Index := 0;

      for Index in Node_Cache_Entries'Range loop
         if Node_Cache_Entries (Index).Node /= null then
            if Filesystem
              = Node_Cache_Entries (Index).Node.all.Parent_Filesystem
              and then
                Parent_Index = Node_Cache_Entries (Index).Node.all.Parent_Index
              and then
                Does_Node_Name_Match_Path_Name
                  (Node_Cache_Entries (Index).Node.all.Filename, Filename)
            then
               Cache_Index := Index;
               Result := Success;
               return;
            end if;
         end if;
      end loop;

      Result := Cache_Entry_Not_Found;
   exception
      when Constraint_Error =>
         Log_Error
           ("Constraint_Error: Search_For_Filesystem_Node_In_Cache",
            Logging_Tags_Node_Cache);
         Result := Constraint_Exception;
   end Search_For_Filesystem_Node_In_Cache;

end Filesystems.Node_Cache;
