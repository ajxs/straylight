-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with RISCV.Paging; use RISCV.Paging;

private package Memory.Virtual.Paging is
   pragma Preelaborate;

   procedure Map
     (Base_Page_Table_Address : Physical_Address_T;
      Virtual_Address         : Virtual_Address_T;
      Physical_Address        : Physical_Address_T;
      Size                    : Memory_Region_Size;
      Region_Flags            : Memory_Region_Flags_T;
      Result                  : out Function_Result);

   procedure Unmap
     (Base_Page_Table_Address : Physical_Address_T;
      Virtual_Address         : Virtual_Address_T;
      Size                    : Memory_Region_Size;
      Result                  : out Function_Result);

   procedure Allocate_And_Initialise_New_Page_Table
     (Table_Physical_Address : out Physical_Address_T;
      Result                 : out Function_Result);

private
   Logging_Tags_Paging : constant Log_Tags := [Log_Tag_Memory_Page_Walking];

   procedure Map_Region
     (Base_Page_Table_Address : Physical_Address_T;
      Virtual_Address         : Virtual_Address_T;
      Physical_Address        : Physical_Address_T;
      Region_Size             : RISCV_Page_Size_T;
      Region_Flags            : Memory_Region_Flags_T;
      Result                  : out Function_Result);

   procedure Unmap_Region
     (Base_Page_Table_Address : Physical_Address_T;
      Virtual_Address         : Virtual_Address_T;
      Region_Size             : out RISCV_Page_Size_T;
      Result                  : out Function_Result);

   function Get_Largest_Page_Size_For_Remaining_Region
     (Virtual_Address  : Virtual_Address_T;
      Physical_Address : Physical_Address_T;
      Remaining_Size   : Storage_Offset) return RISCV_Page_Size_T
   with Pure_Function;

end Memory.Virtual.Paging;
