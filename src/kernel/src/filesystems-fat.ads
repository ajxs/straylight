-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with System;

package Filesystems.FAT is
   pragma Preelaborate;

   --  Note the difference in nomenclature: Find_File locates a 'file' by its
   --  'filename', whereas the higher-level filesystem methods operate on the
   --  full file 'path'.
   procedure Find_File
     (Filesystem      : Filesystem_Access;
      Reading_Process : in out Process_Control_Block_T;
      Filename        : Wide_String;
      Parent_Node     : Filesystem_Node_Access;
      Found_Node      : out Filesystem_Node_Access;
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
   Logging_Tags_FAT : constant Log_Tags := [Log_Tag_Filesystems_FAT];

   type FAT_Type_T is (FAT12, FAT16, FAT32, ExFAT);

   type FAT_Filesystem_Info_T is record
      FAT_Type                    : FAT_Type_T := FAT12;
      Root_Directory_Sector       : Unsigned_64 := 0;
      Root_Directory_Sector_Count : Natural := 0;
      Root_Directory_Buffer_Size  : Natural := 0;
      Bytes_Per_Sector            : Natural := 0;
      First_FAT_Sector            : Unsigned_64 := 0;
      First_Data_Sector           : Unsigned_64 := 0;
      FAT_Sector_Count            : Natural := 0;
      FAT_Buffer_Size             : Natural := 0;
      Sectors_Per_Cluster         : Natural := 0;
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
       Bytes_Per_Sector at 0 range 0 .. 15;
       Sectors_Per_Cluster at 0 range 16 .. 23;
       Reserved_Sector_Count at 0 range 24 .. 39;
       Table_Count at 0 range 40 .. 47;
       Root_Entry_Count at 0 range 48 .. 63;
       Total_Sector_Count at 0 range 64 .. 79;
       Media_Type at 0 range 80 .. 87;
       Table_Size at 0 range 88 .. 103;
       Sectors_Per_Track at 0 range 104 .. 119;
       Head_Side_Count at 0 range 120 .. 135;
       Hidden_Sector_Count at 0 range 136 .. 167;
       Large_Sector_Count at 0 range 168 .. 199;
     end record;

   ----------------------------------------------------------------------------
   --  Extended BIOS parameter block.
   --  Used in FAT12/FAT16 filesystems.
   ----------------------------------------------------------------------------
   type Extended_BIOS_Parameter_Block is record
      Physical_Drive_Number   : Unsigned_8;
      Reserved                : Unsigned_8;
      Extended_Boot_Signature : Unsigned_8;
      Volume_Id               : Unsigned_32;
      Volume_Label            : String (1 .. 11);
      File_System_Type        : String (1 .. 8);
   end record
   with Size => 208, Scalar_Storage_Order => System.Low_Order_First;
   for Extended_BIOS_Parameter_Block use
     record
       Physical_Drive_Number at 0 range 00 .. 07;
       Reserved at 0 range 08 .. 15;
       Extended_Boot_Signature at 0 range 16 .. 23;
       Volume_Id at 0 range 24 .. 55;
       Volume_Label at 0 range 56 .. 143;
       File_System_Type at 0 range 144 .. 207;
     end record;

   ----------------------------------------------------------------------------
   --  Reserved area in the FAT32 BPB.
   ----------------------------------------------------------------------------
   type FAT32_Reserved_Buffer is array (1 .. 12) of Character;

   ----------------------------------------------------------------------------
   --  FAT32 Extended BIOS Parameter Block type.
   ----------------------------------------------------------------------------
   type FAT32_Extended_BIOS_Parameter_Block is record
      Table_Size       : Unsigned_32;
      Drive_Desc       : Unsigned_16;
      Version          : Unsigned_16;
      Root_Cluster     : Unsigned_32;
      Info_Sector      : Unsigned_16;
      Backup_BS_Sector : Unsigned_16;
      Reserved         : FAT32_Reserved_Buffer;
      Drive_Number     : Unsigned_8;
      Reserved_1       : Unsigned_8;
      Boot_Signature   : Unsigned_8;
      Volume_ID        : Unsigned_32;
      Volume_Label     : String (1 .. 11);
      File_System_Type : String (1 .. 8);
   end record
   with Size => 432, Scalar_Storage_Order => System.Low_Order_First;
   for FAT32_Extended_BIOS_Parameter_Block use
     record
       Table_Size at 0 range 0 .. 31;
       Drive_Desc at 0 range 32 .. 47;
       Version at 0 range 48 .. 63;
       Root_Cluster at 0 range 64 .. 95;
       Info_Sector at 0 range 96 .. 111;
       Backup_BS_Sector at 0 range 112 .. 127;
       Reserved at 0 range 128 .. 223;
       Drive_Number at 0 range 224 .. 231;
       Reserved_1 at 0 range 232 .. 239;
       Boot_Signature at 0 range 240 .. 247;
       Volume_ID at 0 range 248 .. 279;
       Volume_Label at 0 range 280 .. 367;
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
       Boot_Jump at 0 range 0 .. 23;
       OEM_Name at 0 range 24 .. 87;
       BPB at 0 range 88 .. 287;
       EBPB_Reserved_Space at 0 range 288 .. 719;
       Reserved at 0 range 720 .. 4095;
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
       Read_Only at 0 range 0 .. 0;
       Hidden at 0 range 1 .. 1;
       System_Entry at 0 range 2 .. 2;
       Volume_Label at 0 range 3 .. 3;
       Directory at 0 range 4 .. 4;
       Archive at 0 range 5 .. 5;
       Device at 0 range 6 .. 6;
       Reserved at 0 range 7 .. 7;
     end record;

   ----------------------------------------------------------------------------
   --  DOS 8.3 Directory Entry
   ----------------------------------------------------------------------------
   type FAT_Directory_Entry_T is record
      File_Name          : String (1 .. 8);
      File_Ext           : String (1 .. 3);
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
       File_Name at 0 range 0 .. 63;
       File_Ext at 0 range 64 .. 87;
       Attributes at 0 range 88 .. 95;
       Reserved at 0 range 96 .. 103;
       Creation_Seconds at 0 range 104 .. 111;
       Creation_Time at 0 range 112 .. 127;
       Creation_Date at 0 range 128 .. 143;
       Last_Accessed_Date at 0 range 144 .. 159;
       First_Cluster_High at 0 range 160 .. 175;
       Last_Modified_Time at 0 range 176 .. 191;
       Last_Modified_Date at 0 range 192 .. 207;
       First_Cluster_Low at 0 range 208 .. 223;
       File_Size at 0 range 224 .. 255;
     end record;

   ----------------------------------------------------------------------------
   --  Directory Index array type.
   ----------------------------------------------------------------------------
   type Directory_Index_T is array (Natural range <>) of FAT_Directory_Entry_T
   with Convention => C, Pack;

   ----------------------------------------------------------------------------
   --  An entry in a FAT12 formatted table.
   ----------------------------------------------------------------------------
   type FAT12_Table_Entry_T is mod 2**12 with Size => 12;

   ----------------------------------------------------------------------------
   --  The file allocation table in a FAT12 formatted device.
   ----------------------------------------------------------------------------
   type FAT12_Table_T is array (Natural range <>) of FAT12_Table_Entry_T
   with Pack;

   ----------------------------------------------------------------------------
   --  An entry into a FAT16 formatted table.
   ----------------------------------------------------------------------------
   type FAT16_Table_Entry_T is new Unsigned_16;

   ----------------------------------------------------------------------------
   --  The file allocation table in a FAT16 formatted device.
   ----------------------------------------------------------------------------
   type FAT16_Table_T is array (Natural range <>) of FAT16_Table_Entry_T
   with Pack;

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
   type File_Name_Number_T is mod 2**5;

   ----------------------------------------------------------------------------
   --  LFN Directory Entry sequence type.
   --  Stores the sequence number and attributes for a LFN directory entry.
   ----------------------------------------------------------------------------
   type Long_File_Name_Sequence is record
      Number     : File_Name_Number_T;
      Empty_1    : Boolean := False;
      Last_Entry : Boolean;
      Empty_2    : Boolean := False;
   end record
   with Size => 8;
   for Long_File_Name_Sequence use
     record
       Number at 0 range 0 .. 4;
       Empty_1 at 0 range 5 .. 5;
       Last_Entry at 0 range 6 .. 6;
       Empty_2 at 0 range 7 .. 7;
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
       Sequence at 0 range 0 .. 7;
       Name_1 at 0 range 8 .. 87;
       Attributes at 0 range 88 .. 95;
       Entry_Type at 0 range 96 .. 103;
       Checksum at 0 range 104 .. 111;
       Name_2 at 0 range 112 .. 207;
       First_Cluster at 0 range 208 .. 223;
       Name_3 at 0 range 224 .. 255;
     end record;

   function Is_Last_Directory_Entry
     (Dir_Entry : FAT_Directory_Entry_T) return Boolean
   is (Dir_Entry.File_Name (1) = ASCII.NUL)
   with Pure_Function, Inline;

   function Is_Unused_Entry (Dir_Entry : FAT_Directory_Entry_T) return Boolean
   is (Character'Pos (Dir_Entry.File_Name (1)) = 16#E5#)
   with Pure_Function, Inline;

   function Is_LFN_Entry (Dir_Entry : FAT_Directory_Entry_T) return Boolean
   is (Dir_Entry.Attributes.Read_Only
       and then Dir_Entry.Attributes.Hidden
       and then Dir_Entry.Attributes.System_Entry
       and then Dir_Entry.Attributes.Volume_Label)
   with Pure_Function, Inline;

   function Get_Filesystem_Type (Total_Clusters : Natural) return FAT_Type_T;

   function Get_Total_Clusters (Boot_Sector : Boot_Sector_T) return Natural;

   function Get_FAT_Sector_Count (Boot_Sector : Boot_Sector_T) return Natural
   with Inline;

   function Is_Cluster_End_Of_Chain
     (Cluster : Unsigned_32; FAT_Type : FAT_Type_T) return Boolean
   with Pure_Function, Inline;

   function Get_First_Sector_Of_Cluster
     (Filesystem_Info : FAT_Filesystem_Info_T; Cluster : Unsigned_32)
      return Unsigned_64
   is (Unsigned_64 (Cluster - 2)
       * Unsigned_64 (Filesystem_Info.Sectors_Per_Cluster)
       + Filesystem_Info.First_Data_Sector);

   function Get_Directory_Entry_First_Sector
     (Filesystem_Info : FAT_Filesystem_Info_T;
      Dir_Entry       : FAT_Directory_Entry_T) return Unsigned_64;

   function Get_Root_Directory_Sector_Count
     (Boot_Sector : Boot_Sector_T) return Natural
   with Inline;

   procedure Read_Boot_Sector
     (Filesystem      : Filesystem_Access;
      Reading_Process : in out Process_Control_Block_T;
      Filesystem_Info : out FAT_Filesystem_Info_T;
      Result          : out Function_Result);

   procedure Find_File_In_Root_Directory
     (Filesystem      : Filesystem_Access;
      Reading_Process : in out Process_Control_Block_T;
      Filesystem_Info : FAT_Filesystem_Info_T;
      Filename        : Wide_String;
      Parent_Node     : Filesystem_Node_Access;
      Filesystem_Node : out Filesystem_Node_Access;
      Result          : out Function_Result);

   procedure Find_File_In_FAT16_Root_Directory
     (Filesystem      : Filesystem_Access;
      Reading_Process : in out Process_Control_Block_T;
      Filesystem_Info : FAT_Filesystem_Info_T;
      Filename        : Wide_String;
      Parent_Node     : Filesystem_Node_Access;
      Filesystem_Node : out Filesystem_Node_Access;
      Result          : out Function_Result);

   procedure Find_File_In_Directory
     (Filesystem      : Filesystem_Access;
      Reading_Process : in out Process_Control_Block_T;
      Filesystem_Info : FAT_Filesystem_Info_T;
      Filename        : Wide_String;
      Parent_Node     : Filesystem_Node_Access;
      Filesystem_Node : out Filesystem_Node_Access;
      Result          : out Function_Result);

   procedure Find_File_In_FAT16_Directory
     (Filesystem      : Filesystem_Access;
      Reading_Process : in out Process_Control_Block_T;
      Filesystem_Info : FAT_Filesystem_Info_T;
      Filename        : Wide_String;
      Parent_Node     : Filesystem_Node_Access;
      Filesystem_Node : out Filesystem_Node_Access;
      Result          : out Function_Result);

   procedure Search_FAT_Directory_For_File
     (Filesystem      : Filesystem_Access;
      Directory       : Directory_Index_T;
      Filename        : Wide_String;
      Parent_Node     : Filesystem_Node_Access;
      Filesystem_Node : out Filesystem_Node_Access;
      Result          : out Function_Result);

   ----------------------------------------------------------------------------
   --  Reads the portion of a file name from a long filename entry.
   ----------------------------------------------------------------------------
   procedure Read_LFN_Entry_Filename
     (Dir_Entry       : FAT_Directory_Entry_T;
      Filename        : in out Wide_String;
      Filename_Length : in out Natural;
      Result          : out Function_Result);

   ----------------------------------------------------------------------------
   --  Reads a DOS filename from an 8.3 directory entry.
   ----------------------------------------------------------------------------
   procedure Read_DOS_Filename
     (Dir_Entry       : FAT_Directory_Entry_T;
      Filename        : in out Wide_String;
      Filename_Length : in out Natural;
      Result          : out Function_Result);

   procedure Print_FAT_Filesystem_Info
     (FAT_Filesystem_Info : FAT_Filesystem_Info_T);

   function Get_Buffer_Max_Directory_Entry_Count
     (Buffer_Size : Natural) return Natural;

   procedure Read_FAT_Entry
     (Filesystem      : Filesystem_Access;
      Reading_Process : in out Process_Control_Block_T;
      Filesystem_Info : FAT_Filesystem_Info_T;
      Cluster         : Unsigned_32;
      FAT_Entry       : out Unsigned_32;
      Result          : out Function_Result);

   procedure Read_FAT16_Root_Directory_Into_Buffer
     (Filesystem             : Filesystem_Access;
      Reading_Process        : in out Process_Control_Block_T;
      Filesystem_Info        : FAT_Filesystem_Info_T;
      Buffer_Virtual_Address : Virtual_Address_T;
      Result                 : out Function_Result);

   procedure Read_Cluster_Into_Buffer
     (Filesystem             : Filesystem_Access;
      Reading_Process        : in out Process_Control_Block_T;
      Filesystem_Info        : FAT_Filesystem_Info_T;
      Cluster                : Unsigned_32;
      Buffer_Virtual_Address : Virtual_Address_T;
      Result                 : out Function_Result);

   --  Combine the two 16-bit halves of the cluster.
   function Get_First_Cluster_Of_Dir_Entry
     (Dir_Entry : FAT_Directory_Entry_T) return Unsigned_32
   is (Unsigned_32 (Dir_Entry.First_Cluster_Low)
       or Shift_Left (Unsigned_32 (Dir_Entry.First_Cluster_High), 16));

   function Get_Directory_Entry_Node_Index
     (First_Cluster : Unsigned_32; Directory_Index : Natural)
      return Filesystem_Node_Index_T
   is (Shift_Left (Unsigned_64 (First_Cluster), 32)
       + Unsigned_64 (Directory_Index));

   function Get_Filesystem_Node_First_FAT_Cluster
     (Node : Filesystem_Node_Access) return Unsigned_32;

   function Get_First_Cluster_From_Index
     (Index : Filesystem_Node_Index_T) return Unsigned_32;

   function Get_Node_Type_From_Directory_Entry
     (Dir_Entry : FAT_Directory_Entry_T) return Filesystem_Node_Type_T;

   procedure Get_Filesystem_Meta_Info
     (Filesystem      : Filesystem_Access;
      Reading_Process : in out Process_Control_Block_T;
      Result          : out Function_Result);

   procedure Read_File_Clusters
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
     (Filesystem_Info : FAT_Filesystem_Info_T; Result : out Function_Result);

end Filesystems.FAT;
