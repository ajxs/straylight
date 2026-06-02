package body Filesystems.FAT.DOS_Filenames is
   function Is_Valid_DOS_Filename_Character (C : Character) return Boolean
   is (C in 'A' .. 'Z'
       or else C in '0' .. '9'
       or else
         C
         in '!'
          | '#'
          | '$'
          | '%'
          | '&'
          | '''
          | '('
          | ')'
          | '-'
          | '@'
          | '^'
          | '_'
          | '`'
          | '{'
          | '}'
          | '~');

   procedure Convert_Path_String_To_DOS_Format
     (Source              : String;
      Destination         : in out String;
      Conversion_Is_Lossy : in out Boolean;
      Chars_Written       : out Integer;
      Result              : out Function_Result)
   is
      Dest_Index  : Integer := Destination'First;
      Upper_Bound : Integer := 0;
   begin
      Chars_Written := 0;

      if Source'Length > Destination'Length then
         Conversion_Is_Lossy := True;
         Upper_Bound := Destination'Length;
      else
         Upper_Bound := Source'Length;
      end if;

      for I in Source'First .. Source'First + Upper_Bound - 1 loop
         Uppercase_Character : constant Character := To_Upper (Source (I));

         if Is_Valid_DOS_Filename_Character (Uppercase_Character) then
            Destination (Dest_Index) := Uppercase_Character;
         else
            Destination (Dest_Index) := '_';
            Conversion_Is_Lossy := True;
         end if;

         Dest_Index := Dest_Index + 1;
         Chars_Written := @ + 1;
      end loop;

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error
           ("Constraint_Error: Convert_Path_String_To_DOS_Format",
            Logging_Tags_FAT);
         Result := Constraint_Exception;
   end Convert_Path_String_To_DOS_Format;

   function Find_Last_Dot_Index (Node_Name_String : String) return Integer is
   begin
      for I in reverse Node_Name_String'First .. Node_Name_String'Last loop
         if Node_Name_String (I) = '.' then
            return I;
         end if;
      end loop;

      return 0;
   end Find_Last_Dot_Index;

   procedure Create_DOS_Filename
     (Filename            : String;
      DOS_Filename        : out FAT_DOS_File_Name_T;
      DOS_Extension       : out FAT_DOS_File_Ext_T;
      Conversion_Is_Lossy : out Boolean;
      Result              : out Function_Result;
      Version             : Integer := 1)
   is
      Start_Index         : Integer := Filename'First;
      DOS_Filename_Length : Integer := 0;

      --  Cap max version string length so that there's a guarantee that at
      --  least 3 characters will be available for the filename stem.
      Max_Version_String_Length : constant Integer := 5;
   begin
      Conversion_Is_Lossy := False;

      if Filename'Length = 0 then
         Result := Success;
         return;
      end if;

      --  Convert the version number to a string, and check if it can fit in
      --  the DOS filename. This number will be appended to the end of the
      --  filename stem in the case that the conversion is lossy.
      --  Do this early so we can avoid unnecessary
      --  processing if the version number is too large.
      Version_Number_String : String := Integer'Image (Version);
      if Version_Number_String'Length > Max_Version_String_Length then
         Log_Error
           ("Version number is too large to fit in the DOS filename.",
            Logging_Tags_FAT);
         Result := Constraint_Exception;
         return;
      end if;

      Version_Number_String (Version_Number_String'First) := '~';

      --  Initialize the DOS filename.
      DOS_Filename := [others => ' '];
      DOS_Extension := [others => ' '];

      --  Skip leading spaces and dots, as these are not valid at the start of
      --  a DOS filename.
      while Start_Index <= Filename'Last
        and then Filename (Start_Index) in ' ' | '.'
      loop
         Start_Index := @ + 1;
         Conversion_Is_Lossy := True;
      end loop;

      Last_Dot_Index : constant Integer :=
        Find_Last_Dot_Index (Filename (Start_Index .. Filename'Last));

      Filename_Stem : constant String :=
        Filename
          (Start_Index
           ..
             (if Last_Dot_Index > 0
              then Last_Dot_Index - 1
              else Filename'Last));

      if Filename_Stem'Length = 0 then
         DOS_Filename (1) := '_';
         Result := Success;
         return;
      end if;

      --  Copy the filename stem.
      Convert_Path_String_To_DOS_Format
        (Filename_Stem,
         DOS_Filename,
         Conversion_Is_Lossy,
         DOS_Filename_Length,
         Result);

      --  If the name conversion was lossy, append the version number to the
      --  end of the filename stem.
      if Conversion_Is_Lossy then
         declare
            Char_Pos : Integer :=
              (if (DOS_Filename_Length + Version_Number_String'Length) > 8
               then 8
               else DOS_Filename_Length + Version_Number_String'Length);
         begin
            for I in reverse Version_Number_String'Range loop
               DOS_Filename (Char_Pos) := Version_Number_String (I);
               Char_Pos := Char_Pos - 1;
            end loop;
         end;

      end if;

      --  Copy the extension.
      if Last_Dot_Index > 0 then
         Extension_String : constant String :=
           Filename (Last_Dot_Index + 1 .. Filename'Last);

         Convert_Path_String_To_DOS_Format
           (Extension_String,
            DOS_Extension,
            Conversion_Is_Lossy,
            DOS_Filename_Length,
            Result);
      end if;

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Create_DOS_Filename", Logging_Tags_FAT);
         Result := Constraint_Exception;
   end Create_DOS_Filename;

   function Get_DOS_Filename_Checksum
     (DOS_Filename : FAT_DOS_File_Name_T; DOS_Extension : FAT_DOS_File_Ext_T)
      return Unsigned_8
   is
      Result : Unsigned_8 := 0;
   begin
      Combined_Name : constant String := DOS_Filename & DOS_Extension;

      for I in Combined_Name'Range loop
         Result :=
           (if (Result and 1) /= 0 then 16#80# else 0)
           + Shift_Right (Result, 1)
           + Character'Pos (Combined_Name (I));
      end loop;

      return Result;
   end Get_DOS_Filename_Checksum;

end Filesystems.FAT.DOS_Filenames;
