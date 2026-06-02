private package Filesystems.FAT.DOS_Filenames
  with Preelaborate
is
   procedure Create_DOS_Filename
     (Filename            : String;
      DOS_Filename        : out FAT_DOS_File_Name_T;
      DOS_Extension       : out FAT_DOS_File_Ext_T;
      --  Whether the conversion required a character substitution that
      --  could potentially lead to a loss of uniqueness in the filename.
      --  In this case, a version number suffix is appended.
      Conversion_Is_Lossy : out Boolean;
      Result              : out Function_Result;
      Version             : Integer := 1);

   function Get_DOS_Filename_Checksum
     (DOS_Filename : FAT_DOS_File_Name_T; DOS_Extension : FAT_DOS_File_Ext_T)
      return Unsigned_8;

end Filesystems.FAT.DOS_Filenames;
