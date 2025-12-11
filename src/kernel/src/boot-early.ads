-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Interfaces;              use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;

with Function_Results; use Function_Results;
with Memory.Virtual;   use Memory.Virtual;
with RISCV.Paging;     use RISCV.Paging;

-------------------------------------------------------------------------------
--  This private package contains the Ada code used to allocate the boot
--  page tables, and create the initial virtual memory mappings required to
--  jump into the kernel's higher-half address space.
--  Everything here is allocated in its own section, and must not access
--  anything outside of this package, as the rest of the kernel is not yet
--  mapped.
-------------------------------------------------------------------------------

private package Boot.Early is
   function Initialise_Boot_Page_Tables return Unsigned_64
   with
     Export,
     Convention     => Assembler,
     External_Name  => "boot_initialise_boot_page_tables",
     Linker_Section => ".boot_text";

   procedure Allocate_New_Boot_Page_Table
     (Page_Table_Addr : out Physical_Address_T; Result : out Function_Result)
   with Linker_Section => ".boot_text";

   ----------------------------------------------------------------------------
   --  A pool of 'Boot Page Tables' is statically allocated in the linker
   --  script, in the '.boot_data' section. This pool is used to allocate page
   --  tables to map the kernel before the jump into higher-half memory.
   --  This memory will be 'freed' once userspace has been initialised.
   ----------------------------------------------------------------------------
   Boot_Page_Table_Pool_Status : array (1 .. 16) of Boolean := [others => True]
   with Linker_Section => ".boot_data";

   procedure Create_Virtual_Memory_Mapping
     (Base_Page_Table_Addr : Physical_Address_T;
      Virtual_Address      : Virtual_Address_T;
      Physical_Address     : Physical_Address_T;
      Region_Size          : RISCV_Page_Size_T;
      Region_Flags         : Memory_Region_Flags_T;
      Result               : out Function_Result)
   with Linker_Section => ".boot_text";

   function Get_Largest_Page_Size_For_Remaining_Region
     (Virtual_Address  : Virtual_Address_T;
      Physical_Address : Physical_Address_T;
      Remaining_Size   : Storage_Offset) return RISCV_Page_Size_T
   with Pure_Function, Linker_Section => ".boot_text";

   procedure Map_Boot_Memory_Section
     (Base_Page_Table_Addr  : Physical_Address_T;
      Virtual_Address_Start : Virtual_Address_T;
      Virtual_Address_End   : Virtual_Address_T;
      Base_Physical_Address : Physical_Address_T;
      Region_Flags          : Memory_Region_Flags_T;
      Result                : out Function_Result)
   with Linker_Section => ".boot_text";

   ----------------------------------------------------------------------------
   --  Early debug console print function.
   --  Prints a string to the SBI debug console.
   ----------------------------------------------------------------------------
   procedure Early_Debug_Console_Print (Str : String)
   with Linker_Section => ".boot_text";

private
   ----------------------------------------------------------------------------
   --  Because the boot code needs to be linked in its own section, it can't
   --  access normal the RISCV library code, which is linked in .text.
   --  Therefore this map is redefined here.
   ----------------------------------------------------------------------------
   Early_Page_Size_In_Bytes :
     constant array (RISCV_Page_Size_T) of Storage_Offset :=
       [Small => Small_Page_Size,
        Large => Large_Page_Size,
        Huge  => Huge_Page_Size]
   with Linker_Section => ".boot_rodata";

   ----------------------------------------------------------------------------
   --  The boot code and data are linked in their own section.
   --  Because the compiler will place any strings declared inline in the boot
   --  code into the normal .rodata section, we need to declare them here so
   --  that they are explicitly placed in the .boot_rodata section, and can
   --  be accessed by the boot code.
   ----------------------------------------------------------------------------
   Str_Allocated_Boot_Page_Table : constant String :=
     "Debug: Allocated boot page table." & ASCII.LF
   with Linker_Section => ".boot_rodata";

   Str_Initialising_Page_Tables : constant String :=
     "Debug: Initialising boot page tables..." & ASCII.LF
   with Linker_Section => ".boot_rodata";

   Str_Error_No_Boot_Table_Entries : constant String :=
     "Error: No free entries in boot table pool." & ASCII.LF
   with Linker_Section => ".boot_rodata";

   Str_Error_Unhandled : constant String :=
     "Error: Unhandled Exception." & ASCII.LF
   with Linker_Section => ".boot_rodata";

   Str_Error_Address_Non_Aligned : constant String :=
     "Error: Invalid non-aligned address." & ASCII.LF
   with Linker_Section => ".boot_rodata";

   Str_Error_Region_Not_Free : constant String :=
     "Error: Region Already Mapped." & ASCII.LF
   with Linker_Section => ".boot_rodata";
end Boot.Early;
