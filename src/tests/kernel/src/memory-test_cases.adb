-------------------------------------------------------------------------------
--  Copyright (c) 2026, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with AUnit.Assertions; use AUnit.Assertions;

package body Memory.Test_Cases is
   overriding
   procedure Run_Test (T : in out Test_Is_Valid_Userspace_Address_Range) is
      pragma Unreferenced (T);

      --  Derived from the constant under test, so that these cases track any
      --  future change to the userspace address space limit.
      Limit : constant Integer_Address :=
        To_Integer (User_Address_Space_Limit);

      --  Declared as a named number so that it can be used both as a size
      --  argument and in Integer_Address arithmetic below.
      Page_Size : constant := 16#1000#;
   begin
      --  Ranges wholly within the userspace address space.
      Assert
        (Is_Valid_Userspace_Address_Range (To_Address (0), 0),
         "An empty range at address zero is valid");
      Assert
        (Is_Valid_Userspace_Address_Range (To_Address (16#1000#), Page_Size),
         "A page-sized range in low memory is valid");
      Assert
        (Is_Valid_Userspace_Address_Range (To_Address (16#1000#), 1),
         "A single byte in low memory is valid");

      --  The range is half-open, so a range whose last byte is the final
      --  byte below the limit is valid.
      Assert
        (Is_Valid_Userspace_Address_Range
           (To_Address (Limit - Page_Size), Page_Size),
         "A range ending exactly at the limit is valid");
      Assert
        (not Is_Valid_Userspace_Address_Range
               (To_Address (Limit - Page_Size), Page_Size + 1),
         "A range ending one byte past the limit is rejected");

      --  Ranges starting at or above the limit.
      Assert
        (not Is_Valid_Userspace_Address_Range (User_Address_Space_Limit, 1),
         "A range starting at the limit is rejected");
      Assert
        (not Is_Valid_Userspace_Address_Range
               (To_Address (Limit + Page_Size), Page_Size),
         "A range starting above the limit is rejected");

      --  A kernel higher-half address is never a valid userspace range.
      Assert
        (not Is_Valid_Userspace_Address_Range
               (Kernel_Address_Space_Start, Page_Size),
         "A kernel higher-half range is rejected");
      Assert
        (not Is_Valid_Userspace_Address_Range
               (Kernel_Heap_Virtual_Address, Page_Size),
         "A kernel heap range is rejected");

      --  Regression cases for the wraparound fix. System.Address is a modular
      --  type, so without an explicit overflow guard a base near the top of
      --  the address space plus a size wraps to a low address and would
      --  otherwise satisfy the limit comparison.
      Assert
        (not Is_Valid_Userspace_Address_Range
               (To_Address (16#FFFF_FFFF_FFFF_FF00#), 16#200#),
         "A range wrapping past the top of the address space is rejected");
      Assert
        (not Is_Valid_Userspace_Address_Range
               (To_Address (Integer_Address'Last), 1),
         "A one byte range at the highest address is rejected");
      Assert
        (not Is_Valid_Userspace_Address_Range
               (To_Address (16#1000#), Storage_Count'Last),
         "The largest representable size is rejected");

      --  Degenerate case: an empty range accesses nothing, so it is accepted
      --  even at the first address outside the userspace address space. No
      --  caller reaches this, as all of them reject a zero length first.
      Assert
        (Is_Valid_Userspace_Address_Range (User_Address_Space_Limit, 0),
         "An empty range at the limit is accepted");
   end Run_Test;

   overriding
   procedure Run_Test (T : in out Test_Do_Memory_Regions_Overlap) is
      pragma Unreferenced (T);

      A : constant Address := To_Address (16#1000#);
      B : constant Address := To_Address (16#2000#);
      C : constant Address := To_Address (16#3000#);

      Page_Size : constant := 16#1000#;
   begin
      --  Identical and containing regions.
      Assert
        (Do_Memory_Regions_Overlap (A, Page_Size, A, Page_Size),
         "Identical regions overlap");
      Assert
        (Do_Memory_Regions_Overlap (A, 4 * Page_Size, B, Page_Size),
         "A region contained within another overlaps");
      Assert
        (Do_Memory_Regions_Overlap (B, Page_Size, A, 4 * Page_Size),
         "Containment is detected with the arguments reversed");

      --  Partial overlaps.
      Assert
        (Do_Memory_Regions_Overlap (A, 2 * Page_Size, B, 2 * Page_Size),
         "Regions overlapping at the tail of the first overlap");
      Assert
        (Do_Memory_Regions_Overlap (B, 2 * Page_Size, A, 2 * Page_Size),
         "Regions overlapping at the head of the first overlap");
      Assert
        (Do_Memory_Regions_Overlap (A, Page_Size + 1, B, Page_Size),
         "A single byte of overlap is detected");

      --  Disjoint regions.
      Assert
        (not Do_Memory_Regions_Overlap (A, Page_Size, C, Page_Size),
         "Regions separated by a gap do not overlap");
      Assert
        (not Do_Memory_Regions_Overlap (C, Page_Size, A, Page_Size),
         "A gap is detected with the arguments reversed");

      --  The regions are half-open, so a region ending exactly where the
      --  next begins shares no byte with it.
      Assert
        (not Do_Memory_Regions_Overlap (A, Page_Size, B, Page_Size),
         "Adjacent regions do not overlap");
      Assert
        (not Do_Memory_Regions_Overlap (B, Page_Size, A, Page_Size),
         "Adjacency is detected with the arguments reversed");

      --  An empty region contains no bytes, so it overlaps nothing.
      Assert
        (not Do_Memory_Regions_Overlap (A, 0, A, Page_Size),
         "An empty first region overlaps nothing");
      Assert
        (not Do_Memory_Regions_Overlap (A, Page_Size, A, 0),
         "An empty second region overlaps nothing");
      Assert
        (not Do_Memory_Regions_Overlap (A, 0, A, 0),
         "Two empty regions do not overlap");

      --  Documents a known limitation rather than desired behaviour. The end
      --  of each region is computed with modular address arithmetic, so a
      --  region wrapping past the top of the address space is not detected.
      --  Callers are required to pass non-wrapping ranges.
      Assert
        (not Do_Memory_Regions_Overlap
               (To_Address (16#FFFF_FFFF_FFFF_F000#),
                2 * Page_Size,
                To_Address (0),
                Page_Size),
         "A wrapping region is not detected against a low region");
   end Run_Test;

end Memory.Test_Cases;
