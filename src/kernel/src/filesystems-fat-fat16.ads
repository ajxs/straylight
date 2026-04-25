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

   procedure Read_FAT16_Root_Directory_Into_Buffer
     (Filesystem             : Filesystem_Access;
      Reading_Process        : in out Process_Control_Block_T;
      Filesystem_Info        : FAT_Filesystem_Info_T;
      Buffer_Virtual_Address : Virtual_Address_T;
      Result                 : out Function_Result);

end Filesystems.FAT.FAT16;
