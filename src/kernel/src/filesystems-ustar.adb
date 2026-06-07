-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Filesystems.Block_Cache; use Filesystems.Block_Cache;
with Filesystems.Node_Cache;  use Filesystems.Node_Cache;

package body Filesystems.UStar is
   procedure Find_File
     (Filesystem      : Filesystem_Access;
      Reading_Process : in out Process_Control_Block_T;
      Filename        : Filesystem_Path_T;
      Parent_Node     : Filesystem_Node_Access;
      Found_Node      : out Filesystem_Node_Access;
      Result          : out Function_Result)
   is
      Sector_Number : Sector_Index_T := 0;

      Block_Number               : Block_Index_T := 0;
      Block_Address              : Virtual_Address_T := Null_Address;
      Sector_Offset_Within_Block : Storage_Offset := 0;

      File_Sector_Size : Unsigned_64 := 0;
   begin
      --  Note that the parent node isn't meaningful for a UStar filesystem,
      --  since it doesn't have a hierarchical structure, but it's still
      --  validated it to ensure that the caller isn't passing in an invalid
      --  node pointer.
      --  This should also never be null, because the UStar filesystem should
      --  always be mounted, and have a parent in the root filesystem.
      Validate_Filesystem_And_Node
        (Filesystem, Parent_Node, Filesystem_Type_UStar, Result);
      if Is_Error (Result) then
         Found_Node := null;
         return;
      end if;

      Log_Debug
        ("Filesystems.UStar.Find_File: Searching for file",
         Logging_Tags_UStar);

      loop
         Get_Sector_Block_Number_And_Offset
           (Sector_Number,
            Ustar_Sector_Size,
            Block_Number,
            Sector_Offset_Within_Block,
            Result);
         if Is_Error (Result) then
            return;
         end if;

         Read_Block_From_Filesystem
           (Filesystem, Reading_Process, Block_Number, Block_Address, Result);
         if Is_Error (Result) then
            return;
         end if;

         declare
            Header : constant Tar_File_Header
            with
              Import,
              Alignment => 1,
              Address   => Block_Address + Sector_Offset_Within_Block;

            Filename_Length : constant Natural :=
              Get_UStar_String_Length (Header.Name);
         begin
            if not Is_Valid_Record (Header) then
               Log_Debug
                 ("Filesystems.UStar.Find_File: Invalid record reached",
                  Logging_Tags_UStar);

               Release_Block (Filesystem, Block_Number, Result);

               exit;
            end if;

            File_Size : constant Unsigned_64 :=
              Octal_To_Unsigned_64 (Header.Size);

            --  The number of sectors occupied by the file's data.
            File_Sector_Size :=
              (File_Size + Ustar_Sector_Size - 1) / Ustar_Sector_Size;

            Log_Debug
              ("Filesystems.UStar.Find_File: Checking file: '"
               & Header.Name (1 .. Filename_Length)
               & "'",
               Logging_Tags_UStar);

            if Filename = Header.Name (1 .. Filename_Length) then
               --  If we have a match, create a filesystem node cache entry.
               Create_Filesystem_Node_Cache_Entry
                 (Filesystem,
                  Filename,
                  Found_Node,
                  Result,
                  Index         => Sector_Number,
                  Data_Location => Sector_Number + 1,
                  Size          => File_Size,
                  Parent_Index  => Parent_Node.all.Index);
               if Is_Error (Result) then
                  Found_Node := null;

                  Release_Block (Filesystem, Block_Number, Result);

                  return;
               end if;

               Found_Node.all.Node_Type :=
                 Get_Filesystem_Node_Type_From_UStar_Typeflag
                   (Header.Typeflag);

               Release_Block (Filesystem, Block_Number, Result);
               Result := Success;
               return;
            end if;
         end;

         Release_Block (Filesystem, Block_Number, Result);

         --  Each file's data is contained right after the header sector,
         --  so skip over the data sectors to get to the next header.
         Sector_Number := Sector_Number + File_Sector_Size + 1;
      end loop;

      Found_Node := null;
      Result := File_Not_Found;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Filesystems.UStar.Find_File");
         Found_Node := null;
         Result := Constraint_Exception;
   end Find_File;

   function Get_UStar_String_Length (Str : USTAR_Header_Name) return Integer is
      Length : Integer := 0;
   begin
      for I in Str'Range loop
         exit when Str (I) = ASCII.NUL;

         Length := Length + 1;
      end loop;

      return Length;
   exception
      when Constraint_Error =>
         return 0;
   end Get_UStar_String_Length;

   function Octal_To_Unsigned_64 (Octal_String : String) return Unsigned_64 is
      Result : Unsigned_64 := 0;
      Digit  : Unsigned_64;
   begin
      for I in Octal_String'Range loop
         --  UStar numeric fields are NUL- or space-terminated.
         exit when Octal_String (I) = ASCII.NUL or else Octal_String (I) = ' ';

         --  Any non-octal character indicates a corrupt header field.
         if Octal_String (I) not in '0' .. '7' then
            return 0;
         end if;

         Digit := Character'Pos (Octal_String (I)) - Character'Pos ('0');

         --  Note that a UStar Octal string is at most 12 characters long,
         --  so we know that the result of the conversion will not overflow
         --  the Unsigned_64 type.
         Result := Result * 8 + Digit;
      end loop;

      return Result;
   end Octal_To_Unsigned_64;

   procedure Print_File_Header (Header : Tar_File_Header) is
   begin
      Log_Debug
        ("UStar File Header:"
         & ASCII.LF
         & "  Name:     "
         & Header.Name
         & ASCII.LF
         & "  Mode:     "
         & Header.Mode
         & ASCII.LF
         & "  Uid:      "
         & Header.Uid
         & ASCII.LF
         & "  Gid:      "
         & Header.Gid
         & ASCII.LF
         & "  Size:     "
         & Header.Size
         & ASCII.LF
         & "  Mtime:    "
         & Header.Mtime
         & ASCII.LF
         & "  Checksum: "
         & Header.Checksum
         & ASCII.LF
         & "  Typeflag: "
         & Header.Typeflag
         & ASCII.LF
         & "  Linkname: "
         & Header.Linkname
         & ASCII.LF
         & "  Magic:    "
         & Header.Magic
         & ASCII.LF
         & "  Version:  "
         & Header.Version
         & ASCII.LF
         & "  Uname:    "
         & Header.Uname
         & ASCII.LF
         & "  Gname:    "
         & Header.Gname
         & ASCII.LF
         & "  Devmajor: "
         & Header.Devmajor
         & ASCII.LF
         & "  Devminor: "
         & Header.Devminor
         & ASCII.LF
         & "  Prefix:   "
         & Header.Prefix,
         Logging_Tags_UStar);
   end Print_File_Header;

   procedure Read_File
     (Filesystem      : Filesystem_Access;
      Reading_Process : in out Process_Control_Block_T;
      Filesystem_Node : Filesystem_Node_Access;
      Buffer_Address  : Virtual_Address_T;
      Start_Offset    : Unsigned_64;
      Bytes_To_Read   : Natural;
      Bytes_Read      : out Natural;
      Result          : out Function_Result)
   is
      Sector_Number              : Sector_Index_T := 0;
      Block_Address              : Virtual_Address_T := Null_Address;
      Block_Number               : Block_Index_T := 0;
      Sector_Offset_Within_Block : Storage_Offset := 0;

      Current_Offset     : Unsigned_64 := Start_Offset;
      Bytes_Left_To_Read : Natural := 0;

      Actual_Bytes_To_Read : Natural := Bytes_To_Read;
   begin
      Bytes_Read := 0;

      Validate_Filesystem_And_Node
        (Filesystem, Filesystem_Node, Filesystem_Type_UStar, Result);
      if Is_Error (Result) then
         return;
      end if;

      Log_Debug ("Filesystems.UStar.Read_File", Logging_Tags_UStar);

      Validate_Read_Start_Offset_And_Get_Actual_Bytes_To_Read
        (Filesystem_Node,
         Start_Offset,
         Bytes_To_Read,
         Actual_Bytes_To_Read,
         Result);
      if Is_Error (Result) then
         return;
      end if;

      Sector_Number :=
        Filesystem_Node.all.Data_Location
        + Current_Offset / Unsigned_64 (Ustar_Sector_Size);

      Bytes_Left_To_Read := Actual_Bytes_To_Read;

      loop
         exit when Bytes_Left_To_Read = 0;

         Get_Sector_Block_Number_And_Offset
           (Sector_Number,
            Ustar_Sector_Size,
            Block_Number,
            Sector_Offset_Within_Block,
            Result);
         if Is_Error (Result) then
            return;
         end if;

         Read_Block_From_Filesystem
           (Filesystem, Reading_Process, Block_Number, Block_Address, Result);
         if Is_Error (Result) then
            return;
         end if;

         Offset_Within_Block : constant Storage_Offset :=
           Sector_Offset_Within_Block
           + Storage_Offset
               (Current_Offset mod Unsigned_64 (Ustar_Sector_Size));

         --  Truncate the number of bytes to copy within the current block
         --  if it exceeds the block's boundaries.
         Bytes_To_Copy_From_Block : constant Natural :=
           Natural'Min
             (Bytes_Left_To_Read, Block_Size - Natural (Offset_Within_Block));

         Copy
           (Buffer_Address + Storage_Offset (Bytes_Read),
            Block_Address + Offset_Within_Block,
            Bytes_To_Copy_From_Block);

         Bytes_Read := Bytes_Read + Bytes_To_Copy_From_Block;

         Release_Block (Filesystem, Block_Number, Result);
         if Is_Error (Result) then
            return;
         end if;

         Current_Offset := @ + Unsigned_64 (Bytes_To_Copy_From_Block);
         Bytes_Left_To_Read := @ - Bytes_To_Copy_From_Block;
         Sector_Number :=
           Filesystem_Node.all.Data_Location
           + Current_Offset / Unsigned_64 (Ustar_Sector_Size);

      end loop;

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Filesystems.UStar.Read_File");
         Bytes_Read := 0;
         Result := Constraint_Exception;
   end Read_File;

end Filesystems.UStar;
