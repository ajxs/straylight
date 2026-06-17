-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Ada.Unchecked_Conversion;
with Interfaces;              use Interfaces;
with Locks;                   use Locks;
with System;                  use System;
with System.Storage_Elements; use System.Storage_Elements;

with Devices;          use Devices;
with Function_Results; use Function_Results;
with Logging;          use Logging;
with Memory;           use Memory;
with Processes;        use Processes;
with Utilities;        use Utilities;

package Filesystems
  with Preelaborate
is

   Block_Size : constant := 16#1000#;

   Filesystem_Node_Name_Max_Byte_Length : constant Integer := 256;

   --  This type is used to represent the *kernel's* internal representation
   --  of a storage device sector. It's independent of any particular device's
   --  specific implementation details.
   --  This abstraction supports a single, consistent type used to index
   --  sectors across all devices, regardless of the device's actual internal
   --  addressing scheme, block size, etc.
   subtype Sector_Index_T is Unsigned_64;

   type Filesystem_Type_T is
     (Filesystem_Type_None,
      Filesystem_Type_FAT,
      Filesystem_Type_Root,
      Filesystem_Type_UStar);

   type Filesystem_T
     (Filesystem_Type : Filesystem_Type_T := Filesystem_Type_None)
   is record
      Device : access Device_T := null;

      --  Some filesystems require addition runtime metadata for operation.
      --  These fields can be used to point to an arbitrary metadata structure,
      --  which can be used at runtime.
      Filesystem_Meta_Info_Address : Virtual_Address_T;
      Filesystem_Meta_Info_Size    : Integer := 0;
   end record;

   type Mounted_Filesystem_Array is array (1 .. 16) of aliased Filesystem_T;

   type Filesystem_Access is access all Filesystem_T;

   type Filesystem_Node_Type_T is
     (Filesystem_Node_Type_None,
      Filesystem_Node_Type_Regular_File,
      Filesystem_Node_Type_Device,
      Filesystem_Node_Type_Directory,
      Filesystem_Node_Type_Mounted_Filesystem);

   --  This type is an alias for the type used internally to represent paths
   --  within the system. This type is used for all function arguments
   --  representing a filesystem path or node name.
   subtype Filesystem_Path_T is String;

   --  A filesystem node's name is stored as a fixed-size array of
   --  UTF-8 encoded bytes, with a length field indicating how
   --  many bytes are actually used.
   type Filesystem_Node_Name_T is
     new Fixed_Length_String_T (Filesystem_Node_Name_Max_Byte_Length);

   Null_Filesystem_Node_Name : constant Filesystem_Node_Name_T :=
     (Value       => [others => Character'Val (0)],
      Max_Length  => Filesystem_Node_Name_Max_Byte_Length,
      Byte_Length => 0);

   --  Unique index of an individual node within its filesystem.
   --  This type is a generic identifier, which is only meaningful within the
   --  context of the filesystem the node belongs to.
   --  It is used to uniquely identify files, and instruct the OS on how
   --  to locate them physically within the filesystem.
   subtype Filesystem_Node_Index_T is Unsigned_64;

   type Filesystem_Node_T is record
      --  @TODO: In the future it might be worthwhile allocating the names of
      --  filesystem nodes on the heap, to avoid allocating the full maximum
      --  path length for every node in memory.
      Filename : Filesystem_Node_Name_T := Null_Filesystem_Node_Name;

      --  The index uniquely identifies this node within its filesystem.
      Index             : Filesystem_Node_Index_T := 0;
      Parent_Index      : Filesystem_Node_Index_T := 0;
      Parent_Filesystem : Filesystem_Access := null;

      --  Note that a mounted filesystem node type is used to represent the
      --  root node of a mounted filesystem.
      --  @TODO: In the future it might make more sense to have separate nodes
      --  for the mount point and the root of the mounted filesystem.
      Node_Type : Filesystem_Node_Type_T := Filesystem_Node_Type_Regular_File;

      Mounted_Filesystem : Filesystem_Access := null;
      Mounted_Device     : Device_Access := null;

      --  This is a generic location identifier, which is only meaningful
      --  within the context of the filesystem it belongs to.
      --  It is used to instruct the filesystem on where to start reading the
      --  file's data.
      Data_Location : Unsigned_64 := 0;
      File_Size     : Unsigned_64 := 0;

      Handle_Count : Natural := 0;
   end record;

   type Filesystem_Node_Access is access all Filesystem_Node_T
   with Convention => C;

   type File_Open_Mode_T is record
      Read   : Boolean := False;
      Write  : Boolean := False;
      Create : Boolean := False;
   end record
   with Size => 64;
   for File_Open_Mode_T use
     record
       Read   at 0 range 0 .. 0;
       Write  at 0 range 1 .. 1;
       Create at 0 range 2 .. 2;
     end record;

   function Validate_File_Open_Mode (Mode : File_Open_Mode_T) return Boolean
   is (Mode.Read or else Mode.Write);

   type Process_File_Handle_T is record
      Entry_Used     : Boolean := False;
      --  The file handle ID is used to reference this handle from user mode.
      --  It's unique to the process, not globally unique across the system.
      --  A file handle is identified by the process ID and the file handle
      --  ID together.
      File_Handle_Id : Unsigned_64 := 0;
      Process_Id     : Process_Id_T := 0;
      File           : Filesystem_Node_Access := null;
      Mode           : File_Open_Mode_T;
      Position       : Unsigned_64 := 0;
   end record;

   type Process_File_Handle_Access is access all Process_File_Handle_T;

   procedure Open_File
     (Process     : in out Process_Control_Block_T;
      Path        : Filesystem_Path_T;
      Mode        : File_Open_Mode_T;
      File_Handle : out Process_File_Handle_Access;
      Result      : out Function_Result);

   procedure Read_File
     (Process        : in out Process_Control_Block_T;
      File_Handle    : Process_File_Handle_Access;
      Buffer_Address : Virtual_Address_T;
      Bytes_To_Read  : Natural;
      Bytes_Read     : out Natural;
      Result         : out Function_Result);

   procedure Write_File
     (Process        : in out Process_Control_Block_T;
      File_Handle    : Process_File_Handle_Access;
      Buffer_Address : Virtual_Address_T;
      Bytes_To_Write : Natural;
      Bytes_Written  : out Natural;
      Result         : out Function_Result);

   procedure Create_File
     (Process  : in out Process_Control_Block_T;
      Path     : Filesystem_Path_T;
      New_Node : out Filesystem_Node_Access;
      Result   : out Function_Result);

   procedure Truncate_File
     (Process     : in out Process_Control_Block_T;
      File_Handle : Process_File_Handle_Access;
      New_Size    : Unsigned_64;
      Result      : out Function_Result);

   procedure Close_File
     (File_Handle : Process_File_Handle_Access; Result : out Function_Result);

   procedure Seek_File
     (File_Handle : Process_File_Handle_Access;
      New_Offset  : Unsigned_64;
      Result      : out Function_Result);

   --  This needs to stay in this package to avoid issues re: strict aliasing.
   --  This automatically suppresses the aliasing optimisations.
   --  refer to:
   --  https://gcc.gnu.org/onlinedocs/gcc-9.4.0/
   --    gnat_ugn/Optimization-and-Strict-Aliasing.html
   function Convert_Address_To_Filesystem_Node_Access is new
     Ada.Unchecked_Conversion (Virtual_Address_T, Filesystem_Node_Access);

   procedure Initialise_Block_Cache;

   System_Root_Filesystem : Filesystem_Access := null;
   Mounted_Filesystems    : Mounted_Filesystem_Array;

   procedure Find_File_Handle
     (Process_Id     : Process_Id_T;
      File_Handle_Id : Unsigned_64;
      File_Handle    : out Process_File_Handle_Access;
      Result         : out Function_Result);

private
   Logging_Tags : constant Log_Tags := [Log_Tag_Filesystems];

   Filesystem_Node_Separator : constant Character := '/';

   subtype Block_Index_T is Unsigned_64;

   type Process_File_Handle_Array is
     array (1 .. 64) of aliased Process_File_Handle_T;

   Open_Files : Process_File_Handle_Array;

   Open_Files_Spinlock : Spinlock_T :=
     (Locked        => 0,
      Time_Acquired => 0,
      Hart_Id       => No_Hart_Id,
      Lock_Id       => Lock_Id_Open_Files);

   Maximum_File_Read_Size  : constant := 16#60_000#;
   Maximum_File_Write_Size : constant := 16#60_000#;

   procedure Read_File_Node_Type_File
     (Process        : in out Process_Control_Block_T;
      File_Handle    : Process_File_Handle_Access;
      Buffer_Address : Virtual_Address_T;
      Bytes_To_Read  : Natural;
      Bytes_Read     : out Natural;
      Result         : out Function_Result);

   procedure Write_File_Node_Type_File
     (Process        : in out Process_Control_Block_T;
      File_Handle    : Process_File_Handle_Access;
      Buffer_Address : Virtual_Address_T;
      Bytes_To_Write : Natural;
      Bytes_Written  : out Natural;
      Result         : out Function_Result);

   procedure Create_File_Handle_For_Filesystem_Node
     (Process         : in out Process_Control_Block_T;
      Filesystem_Node : Filesystem_Node_Access;
      Mode            : File_Open_Mode_T;
      File_Handle     : out Process_File_Handle_Access;
      Result          : out Function_Result);

   function Compare_Node_Name_With_Wide_String
     (Name1 : Wide_String; Name1_Length : Integer; Name2 : Wide_String)
      return Boolean;

   function Does_Node_Name_Match_Path_Name
     (Node_Name        : Filesystem_Node_Name_T;
      Path             : Filesystem_Path_T;
      Case_Insensitive : Boolean := False) return Boolean
   is (Compare_Fixed_Length_String_With_String
         (Node_Name, Path, Case_Insensitive));

   procedure Get_Next_Path_Component
     (Path                   : Filesystem_Path_T;
      Start_Index            : in out Integer;
      End_Index              : out Integer;
      Next_Token_Byte_Length : out Integer);

   function Is_Searchable_Device_Node
     (Node : Filesystem_Node_Access) return Boolean
   is (Node /= null
       and then Node.all.Node_Type = Filesystem_Node_Type_Mounted_Filesystem);

   function Can_Filesystem_Node_Contain_Child_Nodes
     (Node : Filesystem_Node_Access) return Boolean;

   procedure Find_File
     (Process         : in out Process_Control_Block_T;
      Path            : Filesystem_Path_T;
      Filesystem_Node : out Filesystem_Node_Access;
      Result          : out Function_Result);

   function Is_Valid_Filesystem_Pointer
     (Filesystem : Filesystem_Access) return Boolean;

   procedure Set_Filesystem_Node_Name
     (Node      : in out Filesystem_Node_T;
      Node_Name : Filesystem_Path_T;
      Result    : out Function_Result);

   function Sector_To_Block
     (Sector_Number : Sector_Index_T; Sector_Size : Natural)
      return Block_Index_T;

   function Get_Sectors_Per_Block (Sector_Size : Natural) return Natural;

   function Get_Sector_Offset_Within_Block
     (Sector_Number : Sector_Index_T; Sector_Size : Natural)
      return Storage_Offset;

   procedure Find_Unused_File_Handle_Entry
     (File_Handle_Array : in out Process_File_Handle_Array;
      File_Handle_Index : out Positive;
      Result            : out Function_Result);

   procedure Allocate_Filesystem_Node
     (New_Node : out Filesystem_Node_Access; Result : out Function_Result);

   procedure Validate_Read_Start_Offset_And_Get_Actual_Bytes_To_Read
     (Filesystem_Node      : Filesystem_Node_Access;
      Start_Offset         : Unsigned_64;
      Bytes_To_Read        : Natural;
      Actual_Bytes_To_Read : out Natural;
      Result               : out Function_Result);

   procedure Validate_Filesystem_And_Node
     (Filesystem      : Filesystem_Access;
      Filesystem_Node : Filesystem_Node_Access;
      Filesystem_Type : Filesystem_Type_T;
      Result          : out Function_Result);

   procedure Get_Sector_Block_Number_And_Offset
     (Sector_Number              : Sector_Index_T;
      Sector_Size                : Natural;
      Block_Number               : out Block_Index_T;
      Sector_Offset_Within_Block : out Storage_Offset;
      Result                     : out Function_Result);

end Filesystems;
