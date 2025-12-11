-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Filesystems.Block_Cache; use Filesystems.Block_Cache;
with Memory;                  use Memory;
with System_State;            use System_State;
with Utilities;               use Utilities;

package body Filesystems.UStar is
   function Compare_Filename_Against_UStar_Filename
     (Filename : Wide_String; UStar_Filename : String) return Boolean is
   begin
      if Filename'Length /= UStar_Filename'Length then
         return False;
      end if;

      for I in 1 .. UStar_Filename'Length loop
         if Convert_Wide_Char_To_ASCII (Filename (I)) /= UStar_Filename (I)
         then
            return False;
         end if;
      end loop;

      return True;
   exception
      when Constraint_Error =>
         Log_Error
           ("Constraint_Error: Compare_Filename_Against_UStar_Filename");
         return False;
   end Compare_Filename_Against_UStar_Filename;

   procedure Find_File
     (Filesystem      : Filesystem_Access;
      Reading_Process : in out Process_Control_Block_T;
      Filename        : Wide_String;
      Parent_Node     : Filesystem_Node_Access;
      Filesystem_Node : out Filesystem_Node_Access;
      Result          : out Function_Result)
   is
      Current_Sector : Unsigned_64 := 0;

      Sector_Address   : Virtual_Address_T := Null_Address;
      File_Sector_Size : Unsigned_64 := 0;
   begin
      Log_Debug
        ("Filesystems.UStar.Find_File: Searching for file",
         Logging_Tags_UStar);

      if not Is_Valid_Filesystem_Pointer (Filesystem)
        or else Filesystem.all.Filesystem_Type /= Filesystem_Type_UStar
      then
         Log_Error ("Invalid UStar filesystem", Logging_Tags_UStar);
         Filesystem_Node := null;
         Result := Invalid_Argument;
         return;
      end if;

      loop
         Read_Sector_From_Filesystem
           (Filesystem,
            Reading_Process,
            Current_Sector,
            512,
            Sector_Address,
            Result);
         if Is_Error (Result) then
            return;
         end if;

         declare
            Header : constant Tar_File_Header
            with Import, Alignment => 1, Address => Sector_Address;

            Filename_Length : constant Natural :=
              Get_UStar_String_Length (Header.Name);
         begin
            if not Is_Valid_Record (Header) then
               Log_Debug
                 ("Filesystems.UStar.Find_File: Invalid record reached",
                  Logging_Tags_UStar);
               exit;
            end if;

            --  The number of sectors occupied by the file's data.
            File_Sector_Size :=
              (Octal_To_Unsigned_64 (Header.Size) + 511) / 512;

            Log_Debug
              ("Filesystems.UStar.Find_File: Checking file: '"
               & Header.Name (1 .. Filename_Length)
               & "'",
               Logging_Tags_UStar);

            if Compare_Filename_Against_UStar_Filename
                 (Filename, Header.Name (1 .. Filename_Length))
            then
               --  If we have a match, create a filesystem node cache entry.
               Create_Filesystem_Node_Cache_Entry
                 (Current_System_State.Filesystem_Node_Cache,
                  Filesystem,
                  Filename,
                  Filesystem_Node,
                  Result,
                  Index         => Current_Sector,
                  Data_Location => Current_Sector + 1,
                  Size          => Octal_To_Unsigned_64 (Header.Size),
                  Parent_Index  => Parent_Node.all.Index);
               if Is_Error (Result) then
                  Filesystem_Node := null;
                  return;
               end if;

               Filesystem_Node.all.Node_Type :=
                 Get_Filesystem_Node_Type_From_UStar_Typeflag
                   (Header.Typeflag);

               Release_Sector (Filesystem, Current_Sector, 512, Result);
               Result := Success;
               return;
            end if;
         end;

         Release_Sector (Filesystem, Current_Sector, 512, Result);

         --  Each file's data is contained right after the header sector,
         --  so skip over the data sectors to get to the next header.
         Current_Sector := Current_Sector + File_Sector_Size + 1;
      end loop;

      Filesystem_Node := null;
      Result := File_Not_Found;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Filesystems.UStar.Find_File");
         Filesystem_Node := null;
         Result := Constraint_Exception;
   end Find_File;

   function Get_Filesystem_Node_Type_From_UStar_Typeflag
     (Typeflag : Character) return Filesystem_Node_Type_T is
   begin
      case Typeflag is
         when '0' | ASCII.NUL =>
            return Filesystem_Node_Type_File;

         when '5'             =>
            return Filesystem_Node_Type_Directory;

         when others          =>
            return Filesystem_Node_Type_None;
      end case;
   end Get_Filesystem_Node_Type_From_UStar_Typeflag;

   function Get_UStar_String_Length (Str : String) return Integer is
      Length : Integer := 0;
   begin
      for I in 1 .. 100 loop
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
   begin
      for I in Octal_String'Range loop
         exit when Octal_String (I) = ASCII.NUL or else Octal_String (I) = ' ';

         Result :=
           Result * 8 + Character'Pos (Octal_String (I)) - Character'Pos ('0');
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
      Sector_Address : Virtual_Address_T := Null_Address;

      Current_Offset            : Unsigned_64 := Start_Offset;
      Offset_Within_Sector      : Natural := 0;
      Bytes_To_Copy_From_Sector : Natural := 0;
      Bytes_Left_To_Read        : Natural := 0;

      Sector_Size : constant Natural := 512;
   begin
      if not Is_Valid_Filesystem_Pointer (Filesystem)
        or else Filesystem.all.Filesystem_Type /= Filesystem_Type_UStar
      then
         Log_Error ("Invalid UStar filesystem", Logging_Tags_UStar);
         Bytes_Read := 0;
         Result := Invalid_Argument;
         return;
      end if;

      Log_Debug ("Filesystems.UStar.Read_File", Logging_Tags_UStar);

      Current_Read_Sector : constant Unsigned_64 :=
        Filesystem_Node.all.Data_Location
        + Current_Offset / Unsigned_64 (Sector_Size);

      Bytes_Left_To_Read := Bytes_To_Read;

      loop
         exit when Bytes_Left_To_Read = 0;

         Offset_Within_Sector :=
           Natural (Current_Offset mod Unsigned_64 (Sector_Size));

         --  Truncate the number of bytes to copy within this sector
         --  if it exceeds the sector size.
         if Offset_Within_Sector + Bytes_Left_To_Read > Sector_Size then
            Bytes_To_Copy_From_Sector := Sector_Size - Offset_Within_Sector;
         else
            Bytes_To_Copy_From_Sector := Bytes_Left_To_Read;
         end if;

         Read_Sector_From_Filesystem
           (Filesystem,
            Reading_Process,
            Current_Read_Sector,
            Sector_Size,
            Sector_Address,
            Result);
         if Is_Error (Result) then
            return;
         end if;

         Copy
           (Buffer_Address + Storage_Offset (Bytes_Read),
            Sector_Address + Storage_Offset (Offset_Within_Sector),
            Bytes_To_Copy_From_Sector);

         Release_Sector (Filesystem, Current_Read_Sector, Sector_Size, Result);
         if Is_Error (Result) then
            return;
         end if;

         Bytes_Read := Bytes_Read + Bytes_To_Copy_From_Sector;
         Current_Offset :=
           Current_Offset + Unsigned_64 (Bytes_To_Copy_From_Sector);
         Bytes_Left_To_Read := Bytes_Left_To_Read - Bytes_To_Copy_From_Sector;
      end loop;

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Filesystems.UStar.Read_File");
         Bytes_Read := 0;
         Result := Constraint_Exception;
   end Read_File;

end Filesystems.UStar;
