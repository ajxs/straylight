-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Ada.Unchecked_Conversion;
with Interfaces;              use Interfaces;
with System;                  use System;
with System.Storage_Elements; use System.Storage_Elements;

with Addresses;        use Addresses;
with Devices;          use Devices;
with Function_Results; use Function_Results;
with Logging;          use Logging;
with Processes;        use Processes;

package Filesystems is
   pragma Preelaborate;

   Block_Size : constant := 16#1000#;

   type Filesystem_Type_T is
     (Filesystem_Type_None,
      Filesystem_Type_FAT,
      Filesystem_Type_Root,
      Filesystem_Type_UStar);

   type Filesystem_T
     (Filesystem_Type : Filesystem_Type_T := Filesystem_Type_None)
   is record
      Device : access Device_T := null;

      Filesystem_Meta_Info_Address : Virtual_Address_T := Null_Address;
      Filesystem_Meta_Info_Size    : Integer := 0;
   end record;

   type Mounted_Filesystem_Array is array (1 .. 16) of aliased Filesystem_T;

   type Filesystem_Access is access all Filesystem_T;

   type Filesystem_Node_Type_T is
     (Filesystem_Node_Type_None,
      Filesystem_Node_Type_File,
      Filesystem_Node_Type_Directory,
      Filesystem_Node_Type_Mounted_Filesystem);

   subtype Filesystem_Node_Path_T is Wide_String (1 .. 256);

   subtype Filesystem_Node_Index_T is Unsigned_64;

   type Filesystem_Node_T is record
      --  @TODO: This will need to change in the future.
      Filename        : Filesystem_Node_Path_T :=
        [others => Wide_Character'Val (0)];
      Filename_Length : Integer := 0;

      --  Unique index of this node within the filesystem.
      --  This is a generic identifier, which is only meaningful within the
      --  context of the filesystem it belongs to.
      --  It is used to uniquely identify files, and instruct the OS on how
      --  to find them.
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
      Path        : Wide_String;
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
     (Cache  : in out Filesystem_Node_Cache_T;
      Node   : Filesystem_Node_Access;
      Result : out Function_Result);

   procedure Find_Filesystem_Node_In_Cache
     (Cache        : in out Filesystem_Node_Cache_T;
      Filesystem   : Filesystem_Access;
      Parent_Index : Unsigned_64;
      Filename     : Wide_String;
      Node         : out Filesystem_Node_Access;
      Result       : out Function_Result);

private
   Logging_Tags : constant Log_Tags := [Log_Tag_Filesystems];

   Maximum_File_Read_Size : constant := 16#4000#;

   function Compare_Node_Name_With_Wide_String
     (Name1 : Wide_String; Name1_Length : Integer; Name2 : Wide_String)
      return Boolean;

   procedure Get_Next_Path_Component
     (Path              : Wide_String;
      Start_Index       : in out Integer;
      End_Index         : out Integer;
      Next_Token_Length : out Integer);

   function Is_Searchable_Device_Node
     (Node : Filesystem_Node_Access) return Boolean
   is (Node /= null
       and then Node.all.Node_Type = Filesystem_Node_Type_Mounted_Filesystem);

   function Is_Searchable_Filesystem_Node
     (Node : Filesystem_Node_Access) return Boolean;

   procedure Search_For_Filesystem_Node_In_Cache
     (Cache        : in out Filesystem_Node_Cache_T;
      Filesystem   : Filesystem_Access;
      Parent_Index : Unsigned_64;
      Filename     : Wide_String;
      Cache_Index  : out Natural;
      Result       : out Function_Result);

   procedure Find_Free_Cache_Entry
     (Cache       : in out Filesystem_Node_Cache_T;
      Cache_Index : out Natural;
      Result      : out Function_Result);

   procedure Find_File
     (Process         : in out Process_Control_Block_T;
      Path            : Wide_String;
      Filesystem_Node : out Filesystem_Node_Access;
      Result          : out Function_Result);

   function Is_Valid_Filesystem_Pointer
     (Filesystem : Filesystem_Access) return Boolean;

   function Can_Filesystem_Cache_Entry_Be_Overwritten
     (Cache_Entry : Filesystem_Node_Cache_Entry_T) return Boolean;

   procedure Set_Filesystem_Node_Name
     (Node      : in out Filesystem_Node_T;
      Node_Name : Wide_String;
      Result    : out Function_Result);

   procedure Create_Filesystem_Node_Cache_Entry
     (Cache             : in out Filesystem_Node_Cache_T;
      Parent_Filesystem : Filesystem_Access;
      Filename          : Wide_String;
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

end Filesystems;
