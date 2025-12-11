-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
--  The 'Root' filesystem is a special, simple in-memory filesystem designed
--  to hold the 'root' filesystem nodes that exist outside of any real
--  devices, such as mounted filesystems and devices.
-------------------------------------------------------------------------------

package Filesystems.Root is
   pragma Preelaborate;

   procedure Add_Filesystem_Node_To_Root_Filesystem
     (Filesystem      : Filesystem_Access;
      Filename        : Wide_String;
      Parent_Index    : Filesystem_Node_Index_T;
      New_Node_Index  : out Filesystem_Node_Index_T;
      Result          : out Function_Result;
      Node_Type       : Filesystem_Node_Type_T := Filesystem_Node_Type_File;
      Node_Filesystem : Filesystem_Access := null);

   procedure Find_File
     (Filesystem        : Filesystem_Access;
      Filename          : Wide_String;
      Parent_Node_Index : Unsigned_64;
      Filesystem_Node   : out Filesystem_Node_Access;
      Result            : out Function_Result);

   procedure Initialise_Root_Filesystem
     (Filesystem : Filesystem_Access; Result : out Function_Result);

private
   Logging_Tags_FS_Root : constant Log_Tags := [Log_Tag_Filesystems_Root];

   --  The maximum filename length for entries in the root filesystem
   --  is 32 characters. This has been drastically reduced from the 256
   --  *wide* characters allowed in an ordinary filesystem node to save the
   --  amount of memory required to statically allocate the filesystem nodes.
   --  Realistically, 32 is enough to express the names of root FS nodes.
   Maximum_Root_Filesystem_Filename_Length : constant := 32;

   type Root_Filesystem_Node_T is record
      Filename        :
        Wide_String (1 .. Maximum_Root_Filesystem_Filename_Length) :=
          [others => Wide_Character'Val (0)];
      Filename_Length : Natural := 0;
      Index           : Filesystem_Node_Index_T := 0;
      Parent_Index    : Filesystem_Node_Index_T := 0;
      Filesystem      : Filesystem_Access := null;
      Node_Type       : Filesystem_Node_Type_T := Filesystem_Node_Type_File;
      Entry_Used      : Boolean := False;
   end record;

   type Root_Filesystem_Node_Array is
     array (1 .. 32) of Root_Filesystem_Node_T;

   type Root_Filesystem_T is record
      Nodes : Root_Filesystem_Node_Array;
   end record;

   procedure Find_Free_Root_Filesystem_Node_Index
     (Root_Filesystem : Root_Filesystem_T;
      Free_Index      : out Positive;
      Result          : out Function_Result);

end Filesystems.Root;
