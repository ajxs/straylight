-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with System;

package Filesystems.FAT
  with Preelaborate
is
   --  Note the difference in nomenclature: Find_File locates a 'file' by its
   --  'filename', whereas the higher-level filesystem methods operate on the
   --  full file 'path'.
   procedure Find_File
     (Filesystem      : Filesystem_Access;
      Reading_Process : in out Process_Control_Block_T;
      Filename        : Filesystem_Path_T;
      Parent_Node     : Filesystem_Node_Access;
      Found_Node      : out Filesystem_Node_Access;
      Result          : out Function_Result);

   procedure Create_File
     (Filesystem      : Filesystem_Access;
      Reading_Process : in out Process_Control_Block_T;
      Filename        : Filesystem_Path_T;
      Parent_Node     : Filesystem_Node_Access;
      New_Node        : out Filesystem_Node_Access;
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

   procedure Write_File
     (Filesystem      : Filesystem_Access;
      Writing_Process : in out Process_Control_Block_T;
      Filesystem_Node : Filesystem_Node_Access;
      Buffer_Address  : Virtual_Address_T;
      Start_Offset    : Unsigned_64;
      Bytes_To_Write  : Natural;
      Bytes_Written   : out Natural;
      Result          : out Function_Result);

private
   Logging_Tags_FAT : constant Log_Tags := [Log_Tag_Filesystems_FAT];

   No_Free_Clusters : constant Function_Result := -1234_0000;

   Cluster_Marker_EOC_FAT12 : constant := 16#0FF8#;
   Cluster_Marker_EOC_FAT16 : constant := 16#FFF8#;
   Cluster_Marker_EOC_FAT32 : constant := 16#0FFFFFF8#;

   Cluster_Marker_Bad_FAT12 : constant := 16#0FF7#;
   Cluster_Marker_Bad_FAT16 : constant := 16#FFF7#;
   Cluster_Marker_Bad_FAT32 : constant := 16#0FFFFFF7#;

   type FAT_Type_T is
     (FAT_Type_FAT12, FAT_Type_FAT16, FAT_Type_FAT32, FAT_Type_ExFAT);

   --  This structure contains all of the relevant FAT filesystem metadata
   --  that we will need to perform filesystem operations.
   --  It is populated by the Populate_Filesystem_Meta_Info procedure,
   --  which is called by the file operations methods.
   --  The field types of this standard are chosen to reflect the list of
   --  valid values for each field according to the FAT32 v1.03 specification.
   type FAT_Filesystem_Info_T is record
      FAT_Type                        : FAT_Type_T := FAT_Type_FAT12;
      Bytes_Per_Sector                : Positive;
      Sectors_Per_Cluster             : Positive;
      FAT_Table_Count                 : Positive;
      Sectors_In_FAT_Table            : Positive;
      Total_Sector_Count              : Positive;
      Total_Clusters                  : Positive;
      Total_Sectors_In_All_FAT_Tables : Positive;
      Sectors_In_Root_Directory       : Natural := 0;
      First_FAT_Sector                : Sector_Index_T := 0;
      First_Data_Sector               : Sector_Index_T := 0;
      FAT12_16_Root_Directory_Sector  : Sector_Index_T := 0;
   end record;

   ----------------------------------------------------------------------------
   --  The filesystem's boot jump bytes.
   ----------------------------------------------------------------------------
   type Boot_Jump_Bytes_T is array (1 .. 3) of Unsigned_8;

   ----------------------------------------------------------------------------
   --  The BIOS parameter block.
   --  Represents a DOS 3.31 BPB type.
   ----------------------------------------------------------------------------
   type BIOS_Parameter_Block_T is record
      Bytes_Per_Sector      : Unsigned_16;
      Sectors_Per_Cluster   : Unsigned_8;
      Reserved_Sector_Count : Unsigned_16;
      Table_Count           : Unsigned_8;
      Root_Entry_Count      : Unsigned_16;
      Total_Sector_Count    : Unsigned_16;
      Media_Type            : Unsigned_8;
      Table_Size            : Unsigned_16;
      Sectors_Per_Track     : Unsigned_16;
      Head_Side_Count       : Unsigned_16;
      Hidden_Sector_Count   : Unsigned_32;
      Large_Sector_Count    : Unsigned_32;
   end record
   with Size => 200, Scalar_Storage_Order => System.Low_Order_First;
   for BIOS_Parameter_Block_T use
     record
       Bytes_Per_Sector      at 0 range 0 .. 15;
       Sectors_Per_Cluster   at 0 range 16 .. 23;
       Reserved_Sector_Count at 0 range 24 .. 39;
       Table_Count           at 0 range 40 .. 47;
       Root_Entry_Count      at 0 range 48 .. 63;
       Total_Sector_Count    at 0 range 64 .. 79;
       Media_Type            at 0 range 80 .. 87;
       Table_Size            at 0 range 88 .. 103;
       Sectors_Per_Track     at 0 range 104 .. 119;
       Head_Side_Count       at 0 range 120 .. 135;
       Hidden_Sector_Count   at 0 range 136 .. 167;
       Large_Sector_Count    at 0 range 168 .. 199;
     end record;

   ----------------------------------------------------------------------------
   --  Extended BIOS parameter block.
   --  Used in FAT12/FAT16 filesystems.
   ----------------------------------------------------------------------------
   type EBPB_T is record
      Physical_Drive_Number   : Unsigned_8;
      Reserved                : Unsigned_8;
      Extended_Boot_Signature : Unsigned_8;
      Volume_Id               : Unsigned_32;
      Volume_Label            : String (1 .. 11);
      File_System_Type        : String (1 .. 8);
   end record
   with Size => 208, Scalar_Storage_Order => System.Low_Order_First;
   for EBPB_T use
     record
       Physical_Drive_Number   at 0 range 00 .. 07;
       Reserved                at 0 range 08 .. 15;
       Extended_Boot_Signature at 0 range 16 .. 23;
       Volume_Id               at 0 range 24 .. 55;
       Volume_Label            at 0 range 56 .. 143;
       File_System_Type        at 0 range 144 .. 207;
     end record;

   ----------------------------------------------------------------------------
   --  FAT32 Extended BIOS Parameter Block type.
   ----------------------------------------------------------------------------
   type EBPB_FAT32_T is record
      Table_Size       : Unsigned_32;
      Drive_Desc       : Unsigned_16;
      Version          : Unsigned_16;
      Root_Cluster     : Unsigned_32;
      Info_Sector      : Unsigned_16;
      Backup_BS_Sector : Unsigned_16;
      Reserved         : String (1 .. 12);
      Drive_Number     : Unsigned_8;
      Reserved_1       : Unsigned_8;
      Boot_Signature   : Unsigned_8;
      Volume_ID        : Unsigned_32;
      Volume_Label     : String (1 .. 11);
      File_System_Type : String (1 .. 8);
   end record
   with Size => 432, Scalar_Storage_Order => System.Low_Order_First;
   for EBPB_FAT32_T use
     record
       Table_Size       at 0 range 0 .. 31;
       Drive_Desc       at 0 range 32 .. 47;
       Version          at 0 range 48 .. 63;
       Root_Cluster     at 0 range 64 .. 95;
       Info_Sector      at 0 range 96 .. 111;
       Backup_BS_Sector at 0 range 112 .. 127;
       Reserved         at 0 range 128 .. 223;
       Drive_Number     at 0 range 224 .. 231;
       Reserved_1       at 0 range 232 .. 239;
       Boot_Signature   at 0 range 240 .. 247;
       Volume_ID        at 0 range 248 .. 279;
       Volume_Label     at 0 range 280 .. 367;
       File_System_Type at 0 range 368 .. 431;
     end record;

   ----------------------------------------------------------------------------
   --  This buffer type represents the space reserved in the FAT boot sector
   --  for the Extended BIOS Parameter Block.
   --  This reserved buffer type is used since we will not know ahead of time
   --  what FAT type we are dealing with, and will need to be able to convert
   --  between the different EBPB types.
   ----------------------------------------------------------------------------
   type EBPB_Reserved_Space_Buffer_T is array (0 .. 53) of Unsigned_8;

   ----------------------------------------------------------------------------
   --  Reserved Space Buffer for Boot Sector.
   ----------------------------------------------------------------------------
   type Boot_Sector_Reserved_Space_Buffer is array (0 .. 421) of Unsigned_8;

   ----------------------------------------------------------------------------
   --  Type representing the boot sector in a FAT formatted device.
   --  This contains the BIOS Parameter Block typed as a blank buffer which
   --  can be cast to the relevant type depending on the FAT version.
   ----------------------------------------------------------------------------
   type Boot_Sector_T is record
      Boot_Jump           : Boot_Jump_Bytes_T;
      OEM_Name            : String (1 .. 8);
      BPB                 : BIOS_Parameter_Block_T;
      EBPB_Reserved_Space : EBPB_Reserved_Space_Buffer_T;
      Reserved            : Boot_Sector_Reserved_Space_Buffer;
   end record
   with Size => 4096;
   for Boot_Sector_T use
     record
       Boot_Jump           at 0 range 0 .. 23;
       OEM_Name            at 0 range 24 .. 87;
       BPB                 at 0 range 88 .. 287;
       EBPB_Reserved_Space at 0 range 288 .. 719;
       Reserved            at 0 range 720 .. 4095;
     end record;

   ----------------------------------------------------------------------------
   --  File name entry attributes type.
   ----------------------------------------------------------------------------
   type Directory_Entry_Attributes_T is record
      Read_Only    : Boolean;
      Hidden       : Boolean;
      System_Entry : Boolean;
      Volume_Label : Boolean;
      Directory    : Boolean;
      Archive      : Boolean;
      Device       : Boolean;
      Reserved     : Boolean;
   end record
   with Size => 8;
   for Directory_Entry_Attributes_T use
     record
       Read_Only    at 0 range 0 .. 0;
       Hidden       at 0 range 1 .. 1;
       System_Entry at 0 range 2 .. 2;
       Volume_Label at 0 range 3 .. 3;
       Directory    at 0 range 4 .. 4;
       Archive      at 0 range 5 .. 5;
       Device       at 0 range 6 .. 6;
       Reserved     at 0 range 7 .. 7;
     end record;

   subtype FAT_DOS_File_Name_T is String (1 .. 8);
   subtype FAT_DOS_File_Ext_T is String (1 .. 3);

   ----------------------------------------------------------------------------
   --  DOS 8.3 Directory Entry
   ----------------------------------------------------------------------------
   type FAT_Directory_Entry_T is record
      File_Name          : FAT_DOS_File_Name_T;
      File_Ext           : FAT_DOS_File_Ext_T;
      Attributes         : Directory_Entry_Attributes_T;
      Reserved           : Unsigned_8;
      Creation_Seconds   : Unsigned_8;
      Creation_Time      : Unsigned_16;
      Creation_Date      : Unsigned_16;
      Last_Accessed_Date : Unsigned_16;
      First_Cluster_High : Unsigned_16;
      Last_Modified_Time : Unsigned_16;
      Last_Modified_Date : Unsigned_16;
      First_Cluster_Low  : Unsigned_16;
      File_Size          : Unsigned_32;
   end record
   with Size => 256, Scalar_Storage_Order => System.Low_Order_First;
   for FAT_Directory_Entry_T use
     record
       File_Name          at 0 range 0 .. 63;
       File_Ext           at 0 range 64 .. 87;
       Attributes         at 0 range 88 .. 95;
       Reserved           at 0 range 96 .. 103;
       Creation_Seconds   at 0 range 104 .. 111;
       Creation_Time      at 0 range 112 .. 127;
       Creation_Date      at 0 range 128 .. 143;
       Last_Accessed_Date at 0 range 144 .. 159;
       First_Cluster_High at 0 range 160 .. 175;
       Last_Modified_Time at 0 range 176 .. 191;
       Last_Modified_Date at 0 range 192 .. 207;
       First_Cluster_Low  at 0 range 208 .. 223;
       File_Size          at 0 range 224 .. 255;
     end record;

   type Directory_Index_T is
     array (Natural range 0 .. <>) of FAT_Directory_Entry_T
   with Convention => C, Pack;

   ----------------------------------------------------------------------------
   --  An entry into a FAT32 formatted table.
   ----------------------------------------------------------------------------
   type FAT32_Table_Entry_T is new Unsigned_32;

   ----------------------------------------------------------------------------
   --  The file allocation table in a FAT32 formatted device.
   ----------------------------------------------------------------------------
   type FAT32_Table_T is array (Natural range <>) of FAT32_Table_Entry_T
   with Pack;

   ----------------------------------------------------------------------------
   --  The Long File Name Sequence file name sequence number.
   ----------------------------------------------------------------------------
   type File_Name_Number_T is mod 2 ** 5;

   ----------------------------------------------------------------------------
   --  LFN Directory Entry sequence type.
   --  Stores the sequence number and attributes for a LFN directory entry.
   ----------------------------------------------------------------------------
   type Long_File_Name_Sequence is record
      Number     : File_Name_Number_T := 0;
      Reserved   : Boolean := False;
      Last_Entry : Boolean := False;
      Reserved_2 : Boolean := False;
   end record
   with Size => 8;
   for Long_File_Name_Sequence use
     record
       Number     at 0 range 0 .. 4;
       Reserved   at 0 range 5 .. 5;
       Last_Entry at 0 range 6 .. 6;
       Reserved_2 at 0 range 7 .. 7;
     end record;

   ----------------------------------------------------------------------------
   --  LFN Directory Entry type.
   ----------------------------------------------------------------------------
   type Long_File_Name_Directory_Entry is record
      Sequence      : Long_File_Name_Sequence;
      Name_1        : Wide_String (1 .. 5);
      Attributes    : Directory_Entry_Attributes_T;
      Entry_Type    : Unsigned_8;
      Checksum      : Unsigned_8;
      Name_2        : Wide_String (1 .. 6);
      First_Cluster : Unsigned_16;
      Name_3        : Wide_String (1 .. 2);
   end record
   with Size => 256;
   for Long_File_Name_Directory_Entry use
     record
       Sequence      at 0 range 0 .. 7;
       Name_1        at 0 range 8 .. 87;
       Attributes    at 0 range 88 .. 95;
       Entry_Type    at 0 range 96 .. 103;
       Checksum      at 0 range 104 .. 111;
       Name_2        at 0 range 112 .. 207;
       First_Cluster at 0 range 208 .. 223;
       Name_3        at 0 range 224 .. 255;
     end record;

   type LFN_Directory_Index_T is
     array (Natural range 0 .. <>) of Long_File_Name_Directory_Entry
   with Convention => C, Pack;

   function Is_Last_Directory_Entry
     (Dir_Entry : FAT_Directory_Entry_T) return Boolean
   is (Dir_Entry.File_Name (1) = ASCII.NUL)
   with Pure_Function, Inline;

   function Is_Unused_Directory_Entry
     (Dir_Entry : FAT_Directory_Entry_T) return Boolean
   is (Character'Pos (Dir_Entry.File_Name (1)) = 16#E5#)
   with Pure_Function, Inline;

   function Is_Free_Directory_Entry
     (Dir_Entry : FAT_Directory_Entry_T) return Boolean
   is (Is_Last_Directory_Entry (Dir_Entry)
       or else Is_Unused_Directory_Entry (Dir_Entry))
   with Pure_Function, Inline;

   function Is_LFN_Directory_Entry
     (Dir_Entry : FAT_Directory_Entry_T) return Boolean
   is (Dir_Entry.Attributes.Read_Only
       and then Dir_Entry.Attributes.Hidden
       and then Dir_Entry.Attributes.System_Entry
       and then Dir_Entry.Attributes.Volume_Label)
   with Pure_Function, Inline;

   function Get_Filesystem_Type (Total_Clusters : Natural) return FAT_Type_T
   is (if Total_Clusters < 4085
       then FAT_Type_FAT12
       elsif Total_Clusters < 65525
       then FAT_Type_FAT16
       elsif Total_Clusters < 268_435_445
       then FAT_Type_FAT32
       else FAT_Type_ExFAT)
   with Pure_Function, Inline;

   function Is_Cluster_End_Of_Chain
     (Cluster : Unsigned_32; FAT_Type : FAT_Type_T) return Boolean
   is (if FAT_Type = FAT_Type_FAT12
       then Cluster >= Cluster_Marker_EOC_FAT12
       elsif FAT_Type = FAT_Type_FAT16
       then Cluster >= Cluster_Marker_EOC_FAT16
       elsif FAT_Type = FAT_Type_FAT32
       then Cluster >= Cluster_Marker_EOC_FAT32
       else True)
   with Pure_Function, Inline;

   function Is_Cluster_Bad
     (Cluster : Unsigned_32; FAT_Type : FAT_Type_T) return Boolean
   is (if FAT_Type = FAT_Type_FAT12
       then Cluster = Cluster_Marker_Bad_FAT12
       elsif FAT_Type = FAT_Type_FAT16
       then Cluster = Cluster_Marker_Bad_FAT16
       elsif FAT_Type = FAT_Type_FAT32
       then Cluster = Cluster_Marker_Bad_FAT32
       else False)
   with Pure_Function, Inline;

   function Is_Cluster_Free (Cluster : Unsigned_32) return Boolean
   is (Cluster = 0)
   with Pure_Function, Inline;

   function Get_EOC_Marker (FAT_Type : FAT_Type_T) return Unsigned_32
   is (if FAT_Type = FAT_Type_FAT12
       then Cluster_Marker_EOC_FAT12
       elsif FAT_Type = FAT_Type_FAT16
       then Cluster_Marker_EOC_FAT16
       else Cluster_Marker_EOC_FAT32)
   with Pure_Function, Inline;

   function Get_First_Sector_Of_Cluster
     (Cluster             : Unsigned_32;
      Sectors_Per_Cluster : Natural;
      First_Data_Sector   : Unsigned_64) return Sector_Index_T
   is (Sector_Index_T (Cluster - 2)
       * Sector_Index_T (Sectors_Per_Cluster)
       + Sector_Index_T (First_Data_Sector))
   with Inline, Pure_Function;

   procedure Parse_Boot_Sector
     (Boot_Sector     : Boot_Sector_T;
      Filesystem_Info : out FAT_Filesystem_Info_T;
      Result          : out Function_Result);

   procedure Search_FAT_Directory_For_File
     (Filesystem                   : Filesystem_Access;
      Filesystem_Info              : FAT_Filesystem_Info_T;
      Directory                    : Directory_Index_T;
      Filename                     : Filesystem_Path_T;
      Parent_Node                  : Filesystem_Node_Access;
      Directory_Starting_Sector    : Sector_Index_T;
      Entry_Long_Filename          : in out Wide_String;
      Entry_Long_Filename_Length   : in out Natural;
      Entry_Long_Filename_Checksum : in out Unsigned_8;
      Checksum_Valid               : in out Boolean;
      Filesystem_Node              : out Filesystem_Node_Access;
      Last_Entry_Reached           : out Boolean;
      Result                       : out Function_Result);

   procedure Parse_LFN_Directory_Entry
     (Dir_Entry       : FAT_Directory_Entry_T;
      Filename        : in out Wide_String;
      Filename_Length : in out Natural;
      Checksum        : out Unsigned_8;
      Is_Last_Entry   : out Boolean;
      Result          : out Function_Result);

   procedure Parse_DOS_Directory_Entry
     (Dir_Entry       : FAT_Directory_Entry_T;
      Filename        : out Wide_String;
      Filename_Length : out Natural;
      Checksum        : out Unsigned_8;
      Result          : out Function_Result);

   procedure Print_FAT_Filesystem_Info
     (FAT_Filesystem_Info : FAT_Filesystem_Info_T);

   procedure Read_FAT_Entry
     (Filesystem      : Filesystem_Access;
      Reading_Process : in out Process_Control_Block_T;
      Filesystem_Info : FAT_Filesystem_Info_T;
      Cluster         : Unsigned_32;
      FAT_Entry       : out Unsigned_32;
      Result          : out Function_Result);

   procedure Write_FAT_Entry
     (Filesystem      : Filesystem_Access;
      Writing_Process : in out Process_Control_Block_T;
      Filesystem_Info : FAT_Filesystem_Info_T;
      Cluster         : Unsigned_32;
      FAT_Entry       : Unsigned_32;
      Result          : out Function_Result);

   procedure Find_Free_Cluster
     (Filesystem      : Filesystem_Access;
      Writing_Process : in out Process_Control_Block_T;
      Filesystem_Info : FAT_Filesystem_Info_T;
      Free_Cluster    : out Unsigned_32;
      Result          : out Function_Result);

   --  Combine the two 16-bit halves of the cluster.
   function Get_First_Cluster_Of_Dir_Entry
     (Dir_Entry : FAT_Directory_Entry_T) return Unsigned_32
   is (Unsigned_32 (Dir_Entry.First_Cluster_Low)
       or Shift_Left (Unsigned_32 (Dir_Entry.First_Cluster_High), 16));

   function Get_Directory_Entry_Node_Index
     (Directory_Entry_Sector : Unsigned_32; Index_Within_Sector : Natural)
      return Filesystem_Node_Index_T
   is (Shift_Left (Unsigned_64 (Directory_Entry_Sector), 32)
       + Unsigned_64 (Index_Within_Sector));

   function Get_Node_Type_From_Directory_Entry
     (Dir_Entry : FAT_Directory_Entry_T) return Filesystem_Node_Type_T
   is (if Dir_Entry.Attributes.Directory
       then Filesystem_Node_Type_Directory
       else Filesystem_Node_Type_File);

   procedure Read_File_Data
     (Filesystem      : Filesystem_Access;
      Filesystem_Info : FAT_Filesystem_Info_T;
      Reading_Process : in out Process_Control_Block_T;
      Filesystem_Node : Filesystem_Node_Access;
      Buffer_Address  : Virtual_Address_T;
      Start_Offset    : Unsigned_64;
      Bytes_To_Read   : Natural;
      Bytes_Read      : out Natural;
      Result          : out Function_Result);

   procedure Validate_FAT_Filesystem
     (Boot_Sector : Boot_Sector_T; Result : out Function_Result);

   ----------------------------------------------------------------------------
   --  'Reads' a UCS-2 formatted filename from a FAT directory entry into a
   --  UTF-8 encoded filesystem node name.
   ----------------------------------------------------------------------------
   procedure Read_FAT_Filename_Into_Filesystem_Node_Name
     (FAT_Filename         : Wide_String;
      FAT_Filename_Length  : Natural;
      Filesystem_Node_Name : out Filesystem_Node_Name_T;
      Result               : out Function_Result);

   procedure Get_Directory_Entry_Sector_And_Index
     (Filesystem_Node     : Filesystem_Node_Access;
      Sector_Number       : out Sector_Index_T;
      Index_Within_Sector : out Natural;
      Result              : out Function_Result);

   procedure Get_Filesystem_Node_Directory_Entry
     (Filesystem      : Filesystem_Access;
      Calling_Process : in out Process_Control_Block_T;
      Filesystem_Info : FAT_Filesystem_Info_T;
      Filesystem_Node : Filesystem_Node_Access;
      Directory_Entry : out FAT_Directory_Entry_T;
      Result          : out Function_Result);

   procedure Write_Filesystem_Node_Directory_Entry
     (Filesystem              : Filesystem_Access;
      Calling_Process         : in out Process_Control_Block_T;
      Filesystem_Info         : FAT_Filesystem_Info_T;
      Filesystem_Node         : Filesystem_Node_Access;
      Updated_Directory_Entry : FAT_Directory_Entry_T;
      Result                  : out Function_Result);

   procedure Allocate_Cluster
     (Filesystem      : Filesystem_Access;
      Writing_Process : in out Process_Control_Block_T;
      Filesystem_Info : FAT_Filesystem_Info_T;
      New_Cluster     : out Unsigned_32;
      Result          : out Function_Result);

   procedure Extend_Cluster_Chain
     (Filesystem       : Filesystem_Access;
      Writing_Process  : in out Process_Control_Block_T;
      Filesystem_Info  : FAT_Filesystem_Info_T;
      Cluster          : Unsigned_32;
      New_Cluster      : out Unsigned_32;
      Result           : out Function_Result;
      Zero_New_Cluster : Boolean := False);

   procedure Write_File_Data
     (Filesystem      : Filesystem_Access;
      Filesystem_Info : FAT_Filesystem_Info_T;
      Writing_Process : in out Process_Control_Block_T;
      Filesystem_Node : Filesystem_Node_Access;
      Buffer_Address  : Virtual_Address_T;
      Start_Offset    : Unsigned_64;
      Bytes_To_Write  : Natural;
      Bytes_Written   : out Natural;
      Result          : out Function_Result);

end Filesystems.FAT;
