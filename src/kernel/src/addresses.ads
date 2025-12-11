-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Interfaces;              use Interfaces;
with System;                  use System;
with System.Storage_Elements; use System.Storage_Elements;

package Addresses is
   pragma Preelaborate;

   --  The virtual/physical addresses are declared as separate, incompatible
   --  types so that the compiler will prevent any mixing of the two.
   --  Physical addresses cannot be used in virtual space without first being
   --  either mapped into the current virtual space, or being access through
   --  the virtual mapping of all physical memory.
   type Physical_Address_T is new Address;

   --  Physical address arithmetic.
   function "+"
     (Left : Physical_Address_T; Right : Physical_Address_T)
      return Physical_Address_T;
   function "+"
     (Left : Physical_Address_T; Right : Storage_Offset)
      return Physical_Address_T;
   function "+"
     (Left : Storage_Offset; Right : Physical_Address_T)
      return Physical_Address_T;
   pragma Import (Intrinsic, "+");

   function "-"
     (Left : Physical_Address_T; Right : Storage_Offset)
      return Physical_Address_T;
   function "-" (Left, Right : Physical_Address_T) return Storage_Offset;
   pragma Import (Intrinsic, "-");

   subtype Virtual_Address_T is Address;

   --  This is kept as a constant because it's used in address arithmetic.
   Higher_Half_Offset : constant := 16#FFFF_FFFF_0000_0000#;

   --  This is kept as a constant because it's used in address arithmetic.
   Physical_Memory_Map_Address : constant := 16#FFFF_FFD8_0000_0000#;

   --  https://en.wikibooks.org/wiki/Ada_Programming/Attributes/%27To_Address
   Null_Physical_Address : constant Physical_Address_T :=
     Physical_Address_T (System'To_Address (0));

   Kernel_Address_Space_Start : constant Address :=
     System'To_Address (16#FFFF_FFC0_0000_0000#);

   --  16GiB kernel page pool maximum address space size.
   Kernel_Page_Pool_Virtual_Address : constant Address :=
     System'To_Address (16#FFFF_FFC4_0000_0000#);

   --  32GiB kernel heap maximum address space size.
   Kernel_Heap_Virtual_Address : constant Address :=
     System'To_Address (16#FFFF_FFC8_0000_0000#);

   Device_Mapping_Virtual_Address : constant Address :=
     System'To_Address (16#FFFF_FFE8_C000_0000#);

   --  All of the kernel stacks are mapped into the kernel address space,
   --  starting at this address.
   Kernel_Stack_Area_Virtual_Address : constant Address :=
     System'To_Address (16#FFFF_FFEA_0000_0000#);

   --  64GiB user address space limit.
   User_Address_Space_Limit : constant Address :=
     System'To_Address (16#10_0000_0000#);

   Process_Heap_Virtual_Address : constant Virtual_Address_T :=
     To_Address (16#0C_0000_0000#);

   Process_Stack_Virtual_Address : constant Virtual_Address_T :=
     To_Address (16#0E_0000_0000#);

   ----------------------------------------------------------------------------
   --  Checks whether a provided address is 4K aligned, as required by the
   --  paging entity structures.
   ----------------------------------------------------------------------------
   function Is_Address_Page_Aligned (Addr : System.Address) return Boolean
   is ((To_Integer (Addr) and 16#FFF#) = 0)
   with Inline, Pure_Function;

   ----------------------------------------------------------------------------
   --  Tests whether a specified address is aligned to a boundary.
   ----------------------------------------------------------------------------
   function Is_Address_Aligned
     (Addr : System.Address; Boundary : Integer_Address) return Boolean
   is ((To_Integer (Addr) and (Boundary - 1)) = 0)
   with Inline, Pure_Function;

   function Convert_Higher_Half_Address_To_Lower
     (Addr : Address) return Address
   is (Addr - Higher_Half_Offset)
   with Pure_Function;

   function Is_Userspace_Address (Addr : Address) return Boolean
   is (Addr < User_Address_Space_Limit);

   function Get_Lower_Physical_Address
     (Addr : Address) return Physical_Address_T
   is (Physical_Address_T (Convert_Higher_Half_Address_To_Lower (Addr)))
   with Pure_Function;

   function Get_Address_Word_Low (Addr : Address) return Unsigned_32
   with
     Pure_Function,
     Import,
     Convention    => Assembler,
     External_Name => "addresses_get_address_word_low";

   function Get_Address_Word_High (Addr : Address) return Unsigned_32
   with
     Pure_Function,
     Import,
     Convention    => Assembler,
     External_Name => "addresses_get_address_word_high";

   function Convert_BEU32_To_LEU32 (BEU32 : Unsigned_32) return Unsigned_32
   with
     Pure_Function,
     Import,
     Convention    => Assembler,
     External_Name => "addresses_convert_beu32_to_leu32";

   function Convert_BEU64_To_LEU64 (BEU64 : Unsigned_64) return Unsigned_64
   with
     Pure_Function,
     Import,
     Convention    => Assembler,
     External_Name => "addresses_convert_beu64_to_leu64";

   function Unsigned_32_To_Address (U32 : Unsigned_32) return Address
   is (To_Address (Integer_Address (U32)))
   with Inline, Pure_Function;

   function Unsigned_64_To_Address (U64 : Unsigned_64) return Address
   is (To_Address (Integer_Address (U64)))
   with Inline, Pure_Function;

   function Address_To_Unsigned_64 (Addr : Address) return Unsigned_64
   is (Unsigned_64 (To_Integer (Addr)))
   with Inline, Pure_Function;
end Addresses;
