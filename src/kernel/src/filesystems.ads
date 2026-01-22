-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Ada.Unchecked_Conversion;
with Interfaces;              use Interfaces;
with System;                  use System;
with System.Storage_Elements; use System.Storage_Elements;

with Devices;          use Devices;
with Function_Results; use Function_Results;
with Logging;          use Logging;
with Memory;           use Memory;
with Processes;        use Processes;

package Filesystems is
   pragma Preelaborate;

   Block_Size : constant := 16#1000#;

   Filesystem_Node_Max_Byte_Length : constant Integer := 256;

   type Filesystem_Type_T is
     (Filesystem_Type_None,
      Filesystem_Type_FAT,
      Filesystem_Type_Root,
      Filesystem_Type_UStar);

   type Filesystem_T
     (Filesystem_Type : Filesystem_Type_T := Filesystem_Type_None)
   is record
      Device : access Device_T := null;

      Filesystem_Meta_Info_Address : Virtual_Address_T;
      Filesystem_Meta_Info_Size    : Integer := 0;
   end record;

   type Mounted_Filesystem_Array is array (1 .. 16) of aliased Filesystem_T;

   type Filesystem_Access is access all Filesystem_T;

   type Filesystem_Node_Type_T is
     (Filesystem_Node_Type_None,
      Filesystem_Node_Type_File,
      Filesystem_Node_Type_Directory,
      Filesystem_Node_Type_Mounted_Filesystem);

   --  This type is an alias for the type used internally to represent paths
   --  within the system. This type is used for all function arguments
   --  representing a filesystem path or node name.
   subtype Filesystem_Path_T is String;

   --  A filesystem node's name is stored as a fixed-size array of
   --  UTF-8 encoded bytes, with a separate length field indicating how
   --  many bytes are actually used.
   subtype Filesystem_Node_Name_T is
     Filesystem_Path_T (1 .. Filesystem_Node_Max_Byte_Length);

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
      Filename             : Filesystem_Node_Name_T :=
        [others => Character'Val (0)];
      Filename_Byte_Length : Integer := 0;

      --  The index uniquely identifies this node within its filesystem.
      Index             : Filesystem_Node_Index_T := 0;
      Parent_Index      : Filesystem_Node_Index_T := 0;
      Parent_Filesystem : Filesystem_Access := null;
      Filesystem        : Filesystem_Access := null;
      Node_Type         : Filesystem_Node_Type_T := Filesystem_Node_Type_File;

      --  This is a generic location identifier, which is only meaningful
      --  within the context of the filesystem it belongs to.
      --  It is used to instruct the filesystem on where to start reading the
      --  file's data.
      Data_Location : Unsigned_64 := 0;
      Size          : Unsigned_64 := 0;
   end record;

   type Filesystem_Node_Access is access all Filesystem_Node_T
   with Convention => C;

   type File_Open_Mode_T is (File_Open_Mode_Read, File_Open_Mode_Write)
   with Size => 64;
   for File_Open_Mode_T use
     (File_Open_Mode_Read => 0, File_Open_Mode_Write => 1);

   type Process_File_Handle_T is record
      --  The file handle ID is used to reference this handle from user mode.
      File_Handle_Id : Unsigned_64 := 0;
      Entry_Used     : Boolean := False;
      File           : Filesystem_Node_Access := null;
      Mode           : File_Open_Mode_T := File_Open_Mode_Read;
      Process_Id     : Process_Id_T := 0;
      Position       : Unsigned_64 := 0;
   end record;

   type Process_File_Handle_Array is
     array (1 .. 64) of aliased Process_File_Handle_T;

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

   procedure Add_Filesystem_Node_To_Cache
     (Node : Filesystem_Node_Access; Result : out Function_Result);

   procedure Find_Filesystem_Node_In_Cache
     (Filesystem   : Filesystem_Access;
      Parent_Index : Unsigned_64;
      Filename     : Filesystem_Path_T;
      Node         : out Filesystem_Node_Access;
      Result       : out Function_Result);

   procedure Initialise_Block_Cache;

   System_Root_Filesystem : Filesystem_Access := null;
   Mounted_Filesystems    : Mounted_Filesystem_Array;

   Open_Files : Process_File_Handle_Array;

   procedure Find_File_Handle
     (Process_Id     : Process_Id_T;
      File_Handle_Id : Unsigned_64;
      File_Handle    : out Process_File_Handle_Access;
      Result         : out Function_Result);

private
   Logging_Tags : constant Log_Tags := [Log_Tag_Filesystems];

   type Filesystem_Node_Cache_Entry_T is record
      Node        : Filesystem_Node_Access := null;
      Last_Access : Unsigned_64 := 0;

      --  The number of processes currently using this node.
      Handle_Count : Natural := 0;
   end record;

   type Filesystem_Node_Cache_Entry_Array_T is
     array (1 .. 256) of Filesystem_Node_Cache_Entry_T;

   type Filesystem_Node_Cache_T is record
      Entries          : Filesystem_Node_Cache_Entry_Array_T;
      Next_Entry_Index : Positive := 1;
   end record;

   Filesystem_Node_Cache : Filesystem_Node_Cache_T;

   Maximum_File_Read_Size : constant := 16#60_000#;

   function Compare_Node_Name_With_Wide_String
     (Name1 : Wide_String; Name1_Length : Integer; Name2 : Wide_String)
      return Boolean;

   function Does_Node_Name_Match_Path_Name
     (Node_Name             : Filesystem_Node_Name_T;
      Node_Name_Byte_Length : Integer;
      Path                  : Filesystem_Path_T) return Boolean;

   procedure Get_Next_Path_Component
     (Path                   : Filesystem_Path_T;
      Start_Index            : in out Integer;
      End_Index              : out Integer;
      Next_Token_Byte_Length : out Integer);

   function Is_Searchable_Device_Node
     (Node : Filesystem_Node_Access) return Boolean
   is (Node /= null
       and then Node.all.Node_Type = Filesystem_Node_Type_Mounted_Filesystem);

   function Is_Searchable_Filesystem_Node
     (Node : Filesystem_Node_Access) return Boolean;

   procedure Search_For_Filesystem_Node_In_Cache
     (Filesystem   : Filesystem_Access;
      Parent_Index : Unsigned_64;
      Filename     : Filesystem_Path_T;
      Cache_Index  : out Natural;
      Result       : out Function_Result);

   procedure Find_Free_Cache_Entry
     (Cache_Index : out Natural; Result : out Function_Result);

   procedure Find_File
     (Process         : in out Process_Control_Block_T;
      Path            : Filesystem_Path_T;
      Filesystem_Node : out Filesystem_Node_Access;
      Result          : out Function_Result);

   function Is_Valid_Filesystem_Pointer
     (Filesystem : Filesystem_Access) return Boolean;

   function Can_Filesystem_Cache_Entry_Be_Overwritten
     (Cache_Entry : Filesystem_Node_Cache_Entry_T) return Boolean;

   procedure Set_Filesystem_Node_Name
     (Node      : in out Filesystem_Node_T;
      Node_Name : Filesystem_Path_T;
      Result    : out Function_Result);

   procedure Create_Filesystem_Node_Cache_Entry
     (Parent_Filesystem : Filesystem_Access;
      Filename          : Filesystem_Path_T;
      New_Node          : out Filesystem_Node_Access;
      Result            : out Function_Result;
      Index             : Filesystem_Node_Index_T := 0;
      Parent_Index      : Filesystem_Node_Index_T := 0;
      Data_Location     : Unsigned_64 := 0;
      Size              : Unsigned_64 := 0;
      Node_Type         : Filesystem_Node_Type_T := Filesystem_Node_Type_File;
      Filesystem        : Filesystem_Access := null);

   function Sector_To_Block
     (Sector_Number : Unsigned_64; Sector_Size : Natural) return Unsigned_64;

   function Sectors_Per_Block (Sector_Size : Natural) return Natural;

   function Get_Sector_Offset_Within_Block
     (Sector_Number : Unsigned_64; Sector_Size : Natural)
      return Storage_Offset;

   procedure Find_Unused_File_Handle_Entry
     (File_Handle_Array : in out Process_File_Handle_Array;
      File_Handle_Index : out Positive;
      Result            : out Function_Result);

   procedure Allocate_Filesystem_Node
     (New_Node : out Filesystem_Node_Access; Result : out Function_Result);

end Filesystems;
