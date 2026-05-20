private package Filesystems.Node_Cache is
   pragma Preelaborate;

   procedure Find_Filesystem_Node_In_Cache
     (Filesystem   : Filesystem_Access;
      Parent_Index : Unsigned_64;
      Filename     : Filesystem_Path_T;
      Node         : out Filesystem_Node_Access;
      Result       : out Function_Result);

   procedure Create_Filesystem_Node_Cache_Entry
     (Parent_Filesystem  : Filesystem_Access;
      Filename           : Filesystem_Path_T;
      New_Node           : out Filesystem_Node_Access;
      Result             : out Function_Result;
      Index              : Filesystem_Node_Index_T := 0;
      Parent_Index       : Filesystem_Node_Index_T := 0;
      Data_Location      : Unsigned_64 := 0;
      Size               : Unsigned_64 := 0;
      Node_Type          : Filesystem_Node_Type_T := Filesystem_Node_Type_File;
      Mounted_Device     : Device_Access := null;
      Mounted_Filesystem : Filesystem_Access := null);

   procedure Add_Filesystem_Node_To_Cache
     (Node : Filesystem_Node_Access; Result : out Function_Result);

private
   type Filesystem_Node_Cache_Entry_T is record
      Node        : Filesystem_Node_Access := null;
      Last_Access : Unsigned_64 := 0;
   end record;

   type Filesystem_Node_Cache_Entry_Array_T is
     array (1 .. 256) of Filesystem_Node_Cache_Entry_T;

   type Filesystem_Node_Cache_T is record
      Entries  : Filesystem_Node_Cache_Entry_Array_T;
      Spinlock : Spinlock_T;
   end record;

   Filesystem_Node_Cache : Filesystem_Node_Cache_T :=
     (Entries  => [others => (Node => null, Last_Access => 0)],
      Spinlock =>
        (Locked        => 0,
         Time_Acquired => 0,
         Hart_Id       => No_Hart_Id,
         Lock_Id       => Lock_Id_FS_Node_Cache));

   procedure Find_Free_Cache_Entry
     (Cache_Index : out Natural; Result : out Function_Result);

   function Can_Filesystem_Cache_Entry_Be_Overwritten
     (Cache_Entry : Filesystem_Node_Cache_Entry_T) return Boolean;

   --  This is the internal method used to iterate over the cache, looking
   --  for a matching node. It assumes the caller has already acquired the
   --  cache spinlock.
   procedure Search_For_Filesystem_Node_In_Cache
     (Filesystem   : Filesystem_Access;
      Parent_Index : Unsigned_64;
      Filename     : Filesystem_Path_T;
      Cache_Index  : out Natural;
      Result       : out Function_Result);

end Filesystems.Node_Cache;
