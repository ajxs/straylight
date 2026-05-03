private package Filesystems.FAT.FAT16 is
   pragma Preelaborate;

   procedure Find_File_In_FAT16_Root_Directory
     (Filesystem      : Filesystem_Access;
      Reading_Process : in out Process_Control_Block_T;
      Filesystem_Info : FAT_Filesystem_Info_T;
      Filename        : Filesystem_Path_T;
      Parent_Node     : Filesystem_Node_Access;
      Filesystem_Node : out Filesystem_Node_Access;
      Result          : out Function_Result);

   procedure Find_File_In_FAT16_Directory
     (Filesystem      : Filesystem_Access;
      Reading_Process : in out Process_Control_Block_T;
      Filesystem_Info : FAT_Filesystem_Info_T;
      Filename        : Filesystem_Path_T;
      Parent_Node     : Filesystem_Node_Access;
      Filesystem_Node : out Filesystem_Node_Access;
      Result          : out Function_Result);

   procedure Read_FAT16_Table_Entry
     (Filesystem      : Filesystem_Access;
      Reading_Process : in out Process_Control_Block_T;
      Filesystem_Info : FAT_Filesystem_Info_T;
      Cluster         : Unsigned_32;
      FAT_Entry       : out Unsigned_32;
      Result          : out Function_Result);

private

   procedure Get_FAT16_Table_Entry_Sector_Number
     (Filesystem_Info : FAT_Filesystem_Info_T;
      Cluster         : Unsigned_32;
      Sector_Number   : out Unsigned_64;
      Result          : out Function_Result);

   procedure Get_FAT16_Table_Cluster_Index
     (Filesystem_Info : FAT_Filesystem_Info_T;
      Cluster         : Unsigned_32;
      Cluster_Index   : out Natural;
      Result          : out Function_Result);

   type FAT16_Table_Entry_T is new Unsigned_16;

   type FAT16_Table_T is array (Natural range <>) of FAT16_Table_Entry_T
   with Pack;

end Filesystems.FAT.FAT16;
