-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

package Filesystems.UStar is
   pragma Preelaborate;

   procedure Find_File
     (Filesystem      : Filesystem_Access;
      Reading_Process : in out Process_Control_Block_T;
      Filename        : Filesystem_Path_T;
      Parent_Node     : Filesystem_Node_Access;
      Filesystem_Node : out Filesystem_Node_Access;
      Result          : out Function_Result);

   procedure Read_File
     (Filesystem      : Filesystem_Access;
      Reading_Process : in out Process_Control_Block_T;
      Filesystem_Node : Filesystem_Node_Access;
      Buffer_Address  : Virtual_Address_T;
      Start_Offset    : Unsigned_64;
      Bytes_To_Read   : Natural;
      Bytes_Read      : out Natural;
      Result          : out Function_Result);

private
   Logging_Tags_UStar : constant Log_Tags := [Log_Tag_Filesystems_UStar];

   type Tar_File_Header is record
      Name     : String (1 .. 100);
      Mode     : String (1 .. 8);
      Uid      : String (1 .. 8);
      Gid      : String (1 .. 8);
      Size     : String (1 .. 12);
      Mtime    : String (1 .. 12);
      Checksum : String (1 .. 8);
      Typeflag : Character;
      Linkname : String (1 .. 100);
      Magic    : String (1 .. 6);
      Version  : String (1 .. 2);
      Uname    : String (1 .. 32);
      Gname    : String (1 .. 32);
      Devmajor : String (1 .. 8);
      Devminor : String (1 .. 8);
      Prefix   : String (1 .. 155);
      Padding  : String (1 .. 12);
   end record
   with Convention => C, Size => 4096, Pack;

   function Octal_To_Unsigned_64 (Octal_String : String) return Unsigned_64;

   procedure Print_File_Header (Header : Tar_File_Header);

   function Compare_Filename_Against_UStar_Filename
     (Filename : Filesystem_Path_T; UStar_Filename : String) return Boolean;

   function Get_UStar_String_Length (Str : String) return Integer;

   function Is_Valid_Record (Header : Tar_File_Header) return Boolean
   is (Header.Magic (1 .. 5) = "ustar");

   function Get_Filesystem_Node_Type_From_UStar_Typeflag
     (Typeflag : Character) return Filesystem_Node_Type_T;

end Filesystems.UStar;
