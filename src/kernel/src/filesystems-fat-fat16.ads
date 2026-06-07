private package Filesystems.FAT.FAT16 is
   pragma Preelaborate;

   procedure Create_File_FAT16
     (Filesystem      : Filesystem_Access;
      Reading_Process : in out Process_Control_Block_T;
      Filesystem_Info : FAT_Filesystem_Info_T;
      Filename        : Filesystem_Path_T;
      Parent_Node     : Filesystem_Node_Access;
      New_Node        : out Filesystem_Node_Access;
      Result          : out Function_Result);

   procedure Find_File_FAT16
     (Filesystem      : Filesystem_Access;
      Reading_Process : in out Process_Control_Block_T;
      Filesystem_Info : FAT_Filesystem_Info_T;
      Filename        : Filesystem_Path_T;
      Parent_Node     : Filesystem_Node_Access;
      Found_Node      : out Filesystem_Node_Access;
      Result          : out Function_Result);

   procedure Get_FAT16_Entry
     (Filesystem      : Filesystem_Access;
      Reading_Process : in out Process_Control_Block_T;
      Filesystem_Info : FAT_Filesystem_Info_T;
      Cluster         : Unsigned_16;
      FAT_Entry       : out Unsigned_16;
      Result          : out Function_Result);

private
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

   procedure Get_FAT16_Table_Entry_Block_Number_And_Sector_Offset
     (Filesystem_Info            : FAT_Filesystem_Info_T;
      FAT_Index                  : Natural;
      Cluster                    : Unsigned_16;
      Block_Number               : out Unsigned_64;
      Sector_Offset_Within_Block : out Storage_Offset;
      Result                     : out Function_Result);

   procedure Get_FAT16_Table_Entry_Sector_Number
     (Filesystem_Info : FAT_Filesystem_Info_T;
      FAT_Index       : Natural;
      Cluster         : Unsigned_16;
      Sector_Number   : out Unsigned_64;
      Result          : out Function_Result);

   --  It's best to use 'Unsigned_16' for the cluster numbers in FAT16,
   --  since this type will never change.
   type FAT16_Table_T is array (Natural range <>) of Unsigned_16 with Pack;

   procedure Find_Free_Cluster_FAT16
     (Filesystem         : Filesystem_Access;
      Writing_Process    : in out Process_Control_Block_T;
      Filesystem_Info    : FAT_Filesystem_Info_T;
      Start_Cluster_Hint : Unsigned_16;
      Free_Cluster       : out Unsigned_16;
      Result             : out Function_Result);

   function Get_First_Cluster_From_Index_FAT16
     (Index : Filesystem_Node_Index_T) return Unsigned_16;

   procedure Write_Table_Entry_FAT16
     (Filesystem      : Filesystem_Access;
      Writing_Process : in out Process_Control_Block_T;
      Filesystem_Info : FAT_Filesystem_Info_T;
      Cluster         : Unsigned_16;
      FAT_Entry       : Unsigned_16;
      Result          : out Function_Result);

   procedure Allocate_Cluster_FAT16
     (Filesystem      : Filesystem_Access;
      Writing_Process : in out Process_Control_Block_T;
      Filesystem_Info : FAT_Filesystem_Info_T;
      New_Cluster     : out Unsigned_16;
      Result          : out Function_Result);

   procedure Extend_Cluster_Chain_FAT16
     (Filesystem      : Filesystem_Access;
      Writing_Process : in out Process_Control_Block_T;
      Filesystem_Info : FAT_Filesystem_Info_T;
      Cluster         : Unsigned_16;
      New_Cluster     : out Unsigned_16;
      Result          : out Function_Result);

end Filesystems.FAT.FAT16;
