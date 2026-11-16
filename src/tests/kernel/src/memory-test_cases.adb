-------------------------------------------------------------------------------
--  Copyright (c) 2026, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with AUnit.Assertions; use AUnit.Assertions;

package body Memory.Test_Cases is
   use Function_Results;

   function Create_Region
     (Base : Integer_Address; Size : Storage_Count)
      return Physical_Memory_Region_T
   is ((Base => Physical_Address_T (To_Address (Base)), Size => Size));

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

   overriding
   procedure Run_Test (T : in out Test_Align_Physical_Address) is
      pragma Unreferenced (T);

      function P (Value : Integer_Address) return Physical_Address_T
      is (Physical_Address_T (To_Address (Value)));

      Page_Size : constant := 16#1000#;
   begin
      Assert
        (Align_Down (P (16#8006_0000#), Page_Size) = P (16#8006_0000#),
         "Align_Down leaves an aligned address unchanged");
      Assert
        (Align_Down (P (16#8006_0FFF#), Page_Size) = P (16#8006_0000#),
         "Align_Down rounds down to the page start");

      Assert
        (Align_Up (P (16#8006_0000#), Page_Size) = P (16#8006_0000#),
         "Align_Up leaves an aligned address unchanged");
      Assert
        (Align_Up (P (16#8006_0001#), Page_Size) = P (16#8006_1000#),
         "Align_Up rounds up to the next page");

      Assert
        (Align_Up (P (0), Page_Size) = P (0),
         "Align_Up leaves zero unchanged");

      --  Documents the limit named in the spec, rather than desired
      --  behaviour: rounding up within a page of the top wraps to zero.
      Assert
        (Align_Up (P (Integer_Address'Last), Page_Size) = P (0),
         "Align_Up wraps at the top of the address space");
   end Run_Test;

   overriding
   procedure Run_Test (T : in out Test_Align_Virtual_Address) is
      pragma Unreferenced (T);

      function V (Value : Integer_Address) return Virtual_Address_T
      is (To_Address (Value));

      Page_Size : constant := 16#1000#;
   begin
      --  A higher-half address, as the kernel uses.
      Assert
        (Align_Down (V (16#FFFF_FFE8_C000_0000#), Page_Size)
         = V (16#FFFF_FFE8_C000_0000#),
         "Align_Down leaves an aligned address unchanged");
      Assert
        (Align_Down (V (16#FFFF_FFE8_C000_0FFF#), Page_Size)
         = V (16#FFFF_FFE8_C000_0000#),
         "Align_Down rounds down to the page start");

      Assert
        (Align_Up (V (16#FFFF_FFE8_C000_0000#), Page_Size)
         = V (16#FFFF_FFE8_C000_0000#),
         "Align_Up leaves an aligned address unchanged");
      Assert
        (Align_Up (V (16#FFFF_FFE8_C000_0001#), Page_Size)
         = V (16#FFFF_FFE8_C000_1000#),
         "Align_Up rounds up to the next page");

      Assert
        (Align_Up_To_Page_Boundary (V (16#FFFF_FFE8_C000_0100#))
         = V (16#FFFF_FFE8_C000_1000#),
         "Align_Up_To_Page_Boundary rounds up to the next page");
      Assert
        (Align_Down_To_Page_Boundary (V (16#FFFF_FFE8_C000_0100#))
         = V (16#FFFF_FFE8_C000_0000#),
         "Align_Down_To_Page_Boundary rounds down to the page start");

      --  Documents the limit named in the spec, rather than desired
      --  behaviour: rounding up within a page of the top wraps to zero.
      Assert
        (Align_Up (V (Integer_Address'Last), Page_Size) = V (0),
         "Align_Up wraps at the top of the address space");
   end Run_Test;

   overriding
   procedure Run_Test (T : in out Test_Align_Memory_Size_Up) is
      pragma Unreferenced (T);

      Page_Size : constant := 16#1000#;

      -------------------------------------------------------------------------
      --  Asserts that calling Align_Memory_Size_Up with these arguments
      --  raises Constraint_Error.
      --  The arguments are Storage_Count, not the function's parameter
      --  subtypes, so each one is converted to the function parameter's
      --  constrained subtype as part of the call (Ada RM 6.4.1).
      --  That conversion is where an out-of-range argument would raise a
      --  Constraint_Error, before the body runs.
      -------------------------------------------------------------------------
      procedure Assert_Raises_Constraint_Error
        (Size : Storage_Count; Boundary : Storage_Count; Message : String) is
      begin
         --  The call is in the message, so its result is always used.
         Assert
           (False,
            Message
            & ", but it returned"
            & Storage_Count'Image (Align_Memory_Size_Up (Size, Boundary)));
      exception
         when Constraint_Error =>
            null;
      end Assert_Raises_Constraint_Error;
   begin
      Assert
        (Align_Memory_Size_Up (0, Page_Size) = 0, "A size of zero stays zero");
      Assert
        (Align_Memory_Size_Up (1, Page_Size) = Page_Size,
         "A single byte rounds up to a page");
      Assert
        (Align_Memory_Size_Up (16#100#, Page_Size) = Page_Size,
         "The UART's register size rounds up to a page");
      Assert
        (Align_Memory_Size_Up (Page_Size, Page_Size) = Page_Size,
         "An aligned size is unchanged");
      Assert
        (Align_Memory_Size_Up (Page_Size + 1, Page_Size) = 2 * Page_Size,
         "One byte over a page rounds up to two pages");
      Assert
        (Align_Memory_Size_Up (16#60_0000#, Page_Size) = 16#60_0000#,
         "The PLIC's register size is already page aligned");

      --  Unlike Align_Up for addresses, the boundary doesn't need to be a
      --  power of two.
      Assert
        (Align_Memory_Size_Up (7, 3) = 9,
         "A boundary that isn't a power of two rounds up");
      Assert
        (Align_Memory_Size_Up (9, 3) = 9,
         "A multiple of a boundary that isn't a power of two is unchanged");

      --  The largest arguments the subtypes allow.
      Assert
        (Align_Memory_Size_Up (Alignable_Size_T'Last, Page_Size)
         = Alignable_Size_T'Last + 1,
         "The largest size rounds up past Alignable_Size_T'Last");
      Assert
        (Align_Memory_Size_Up (Alignable_Size_T'Last, Alignment_T'Last)
         = Alignable_Size_T'Last,
         "The largest size is a multiple of the largest boundary");
      Assert
        (Align_Memory_Size_Up (1, Alignment_T'Last) = Alignment_T'Last,
         "A single byte rounds up to the largest boundary");

      Assert
        (Align_Memory_Size_Up_To_Page_Boundary (16#100#) = Page_Size,
         "Align_Memory_Size_Up_To_Page_Boundary rounds up to a page");

      --  Arguments outside the parameter subtypes.
      Assert_Raises_Constraint_Error
        (Alignable_Size_T'Last + 1,
         Page_Size,
         "A size above Alignable_Size_T'Last should raise Constraint_Error");
      Assert_Raises_Constraint_Error
        (Page_Size, 0, "A boundary of zero should raise Constraint_Error");
      Assert_Raises_Constraint_Error
        (Page_Size,
         Alignment_T'Last + 1,
         "A boundary above Alignment_T'Last should raise Constraint_Error");
   end Run_Test;

   overriding
   procedure Run_Test (T : in out Test_Append_Physical_Memory_Region_To_Array)
   is
      pragma Unreferenced (T);

      --  Starting the array at 5 checks that entries are placed from
      --  Regions'First, not from 1.
      Regions : Physical_Memory_Region_Array_T (5 .. 6);
      Count   : Natural := 0;
      Result  : Function_Result := Unset;
   begin
      Append_Physical_Memory_Region_To_Array
        (Regions, Count, Create_Region (16#1000#, 16#1000#), Result);
      Assert
        (Result = Success and then Count = 1, "The first region is appended");
      Assert
        (Regions (5) = Create_Region (16#1000#, 16#1000#),
         "The first region goes in the first entry");

      Append_Physical_Memory_Region_To_Array
        (Regions, Count, Create_Region (16#3000#, 0), Result);
      Assert
        (Result = Success and then Count = 1, "A zero-size region is skipped");

      Append_Physical_Memory_Region_To_Array
        (Regions, Count, Create_Region (16#3000#, 16#1000#), Result);
      Assert
        (Result = Success
         and then Count = 2
         and then Regions (6) = Create_Region (16#3000#, 16#1000#),
         "The second region goes in the second entry");

      Append_Physical_Memory_Region_To_Array
        (Regions, Count, Create_Region (16#5000#, 16#1000#), Result);
      Assert
        (Result = Region_Array_Exhausted and then Count = 2,
         "Appending to a full array fails with Region_Array_Exhausted");
   end Run_Test;

   overriding
   procedure Run_Test
     (T : in out Test_Remove_Physical_Memory_Region_From_Array)
   is
      pragma Unreferenced (T);

      --  Every case starts from the one region 0x1000 to 0x5000.
      Region : constant Physical_Memory_Region_T :=
        Create_Region (16#1000#, 16#4000#);

      Regions : Physical_Memory_Region_Array_T (1 .. 4);
      Count   : Natural := 0;
      Result  : Function_Result := Unset;

      --  Removes Base .. Base + Size from the one starting region.
      procedure Remove (Base : Integer_Address; Size : Storage_Count) is
      begin
         Regions := [others => <>];
         Regions (1) := Region;
         Count := 1;
         Remove_Physical_Memory_Region_From_Array
           (Regions, Count, Create_Region (Base, Size), Result);
      end Remove;
   begin
      Remove (16#2000#, 16#1000#);
      Assert
        (Result = Success
         and then Count = 2
         and then Regions (1) = Create_Region (16#1000#, 16#1000#)
         and then Regions (2) = Create_Region (16#3000#, 16#2000#),
         "Removing the middle splits the region in two");

      Remove (16#0800#, 16#1800#);
      Assert
        (Result = Success
         and then Count = 1
         and then Regions (1) = Create_Region (16#2000#, 16#3000#),
         "Removing across the start keeps the part above");

      Remove (16#4000#, 16#2000#);
      Assert
        (Result = Success
         and then Count = 1
         and then Regions (1) = Create_Region (16#1000#, 16#3000#),
         "Removing across the end keeps the part below");

      Remove (16#1000#, 16#4000#);
      Assert
        (Result = Success and then Count = 0,
         "Removing exactly the region leaves nothing");

      Remove (16#0000#, 16#8000#);
      Assert
        (Result = Success and then Count = 0,
         "Removing more than the region leaves nothing");

      Remove (16#8000#, 16#1000#);
      Assert
        (Result = Success and then Count = 1 and then Regions (1) = Region,
         "Removing a region that doesn't overlap changes nothing");

      Remove (16#2000#, 0);
      Assert
        (Result = Success and then Count = 1 and then Regions (1) = Region,
         "Removing an empty region changes nothing, and doesn't split");

      --  One removal across two regions trims both.
      Regions := [others => <>];
      Regions (1) := Create_Region (16#1000#, 16#1000#);
      Regions (2) := Create_Region (16#3000#, 16#1000#);
      Count := 2;
      Remove_Physical_Memory_Region_From_Array
        (Regions, Count, Create_Region (16#1800#, 16#2000#), Result);
      Assert
        (Result = Success
         and then Count = 2
         and then Regions (1) = Create_Region (16#1000#, 16#800#)
         and then Regions (2) = Create_Region (16#3800#, 16#800#),
         "Removing across two regions trims the end of one and the start of "
         & "the other");

      --  A split needs one more entry than there is room for.
      Full_Regions : Physical_Memory_Region_Array_T (1 .. 1) := [Region];
      Full_Count : Natural := 1;
      Remove_Physical_Memory_Region_From_Array
        (Full_Regions, Full_Count, Create_Region (16#2000#, 16#1000#), Result);
      Assert
        (Result = Region_Array_Exhausted,
         "A split with no room left fails with Region_Array_Exhausted");

      Remove (Integer_Address'Last - 16#FFF#, 16#2000#);
      Assert
        (Result = Invalid_Argument,
         "Removing a region that wraps around the address space fails");
   end Run_Test;

end Memory.Test_Cases;
