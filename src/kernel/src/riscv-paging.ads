-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Ada.Unchecked_Conversion;

with Memory; use Memory;

package RISCV.Paging is
   pragma Preelaborate;

   --  The physical size of small pages in the system.
   Small_Page_Size : constant := 16#1000#;
   --  The physical size of large pages in the system.
   Large_Page_Size : constant := 16#20_0000#;
   --  The physical size of huge pages in the system.
   Huge_Page_Size  : constant := 16#4000_0000#;

   type RISCV_Page_Size_T is (Small, Large, Huge);

   --  Type for the indexes into the page table array stored within a
   --  virtual address.
   type Page_Table_Index_T is mod 2 ** 9;

   --  Page aligned address type.
   type Page_Aligned_Address_T is mod 2 ** 44;

   --  Type for the index into the leaf page table stored within a
   --  virtual address.
   type Page_Offset_T is mod 2 ** 12;

   --  Type for the 'array' of page table indexes stored within a virtual
   --  address.
   type Virtual_Page_Number_Array_T is array (0 .. 2) of Page_Table_Index_T
   with Pack;

   ----------------------------------------------------------------------------
   --  RISC-V Virtual Address Type.
   ----------------------------------------------------------------------------
   type Page_Table_Virtual_Address_T is record
      Offset   : Page_Offset_T;
      VPN      : Virtual_Page_Number_Array_T;
      Sign_Ext : Boolean;
   end record
   with Size => 64;
   for Page_Table_Virtual_Address_T use
     record
       Offset   at 0 range 0 .. 11;
       VPN      at 0 range 12 .. 38;
       Sign_Ext at 0 range 39 .. 63;
     end record;

   ----------------------------------------------------------------------------
   --  RISC-V SV39 Page Table Entry Type.
   ----------------------------------------------------------------------------
   type Page_Table_Entry_SV39_T is record
      V        : Boolean := False;
      R        : Boolean := False;
      W        : Boolean := False;
      X        : Boolean := False;
      U        : Boolean := False;
      G        : Boolean := False;
      A        : Boolean := False;
      D        : Boolean := False;
      RSW      : Natural range 0 .. 3 := 0;
      PPN      : Page_Aligned_Address_T := 0;
      Reserved : Boolean := False;
   end record
   with Size => 64, Volatile;
   for Page_Table_Entry_SV39_T use
     record
       V        at 0 range 0 .. 0;
       R        at 0 range 1 .. 1;
       W        at 0 range 2 .. 2;
       X        at 0 range 3 .. 3;
       U        at 0 range 4 .. 4;
       G        at 0 range 5 .. 5;
       A        at 0 range 6 .. 6;
       D        at 0 range 7 .. 7;
       RSW      at 0 range 8 .. 9;
       PPN      at 0 range 10 .. 53;
       Reserved at 0 range 54 .. 63;
     end record;

   --  Native RISC-V Page table type.
   type Page_Table_T is array (Page_Table_Index_T) of Page_Table_Entry_SV39_T
   with Convention => C, Volatile;

   function Is_Leaf_Entry
     (Table_Entry : Page_Table_Entry_SV39_T) return Boolean
   is (Table_Entry.V
       and then (Table_Entry.R or else Table_Entry.W or else Table_Entry.X));

   function Is_Table_Free (Page_Table : Page_Table_T) return Boolean;

   function Is_Valid_SV39_Virtual_Address
     (Virtual_Address : Address) return Boolean;

   ----------------------------------------------------------------------------
   --  Converts a native system address to a 44-bit page aligned address.
   ----------------------------------------------------------------------------
   function To_Virtual_Address is new
     Ada.Unchecked_Conversion
       (Source => System.Address,
        Target => Page_Table_Virtual_Address_T);

   ----------------------------------------------------------------------------
   --  Converts a 64-bit unsigned integer to a 44-bit page aligned address.
   ----------------------------------------------------------------------------
   function To_Virtual_Address is new
     Ada.Unchecked_Conversion
       (Source => Unsigned_64,
        Target => Page_Table_Virtual_Address_T);

   ----------------------------------------------------------------------------
   --  Converts a 44-bit page aligned address to a 64-bit native address.
   ----------------------------------------------------------------------------
   function To_Unsigned_64 is new
     Ada.Unchecked_Conversion
       (Source => Page_Table_Virtual_Address_T,
        Target => Unsigned_64);

   ----------------------------------------------------------------------------
   --  Converts a 64-bit unsigned integer to a 44-bit page aligned address.
   ----------------------------------------------------------------------------
   function To_Page_Aligned is new
     Ada.Unchecked_Conversion
       (Source => Unsigned_64,
        Target => Page_Table_Virtual_Address_T);

   ----------------------------------------------------------------------------
   --  This function converts a System Address to the 44bit 4kb page aligned
   --  address type expected by the page table entities.
   ----------------------------------------------------------------------------
   function To_Page_Aligned_Address
     (Addr : System.Address) return Page_Aligned_Address_T
   with Inline, Pure_Function;

   ----------------------------------------------------------------------------
   --  This function converts a 44bit 4kb aligned address type back to a
   --  native 64bit system address.
   ----------------------------------------------------------------------------
   function To_System_Address
     (Addr : Page_Aligned_Address_T) return System.Address
   is (Unsigned_64_To_Address (Shift_Left (Unsigned_64 (Addr), 12)))
   with Inline, Pure_Function;

   ----------------------------------------------------------------------------
   --  Null Page Table Entry.
   --  All fields are initialised to 0. This is used for initialisation
   --  purposes, and for returning null values.
   ----------------------------------------------------------------------------
   Null_Page_Table_Entry : constant Page_Table_Entry_SV39_T :=
     (V        => False,
      R        => False,
      W        => False,
      X        => False,
      U        => False,
      G        => False,
      A        => False,
      D        => False,
      RSW      => 0,
      PPN      => 0,
      Reserved => False);

   Page_Size_In_Bytes : constant array (RISCV_Page_Size_T) of Storage_Offset :=
     [Small => Small_Page_Size,
      Large => Large_Page_Size,
      Huge  => Huge_Page_Size];

private
   ----------------------------------------------------------------------------
   --  On RISC-V, in SV39 mode the top 25 bits of a virtual address must be the
   --  sign-extension of bit 39. This ensures that this property is satisfied.
   ----------------------------------------------------------------------------
   function Are_Address_Bits_39_To_63_Equal_To_Bit_38
     (Number : Unsigned_64) return Boolean
   with Pure_Function;
end RISCV.Paging;
