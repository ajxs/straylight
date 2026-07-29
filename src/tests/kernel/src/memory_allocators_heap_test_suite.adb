-------------------------------------------------------------------------------
--  Copyright (c) 2026, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with AUnit.Test_Suites;                 use AUnit.Test_Suites;
with AUnit.Simple_Test_Cases;           use AUnit.Simple_Test_Cases;
with Memory.Allocators.Heap.Test_Cases; use Memory.Allocators.Heap.Test_Cases;

function Memory_Allocators_Heap_Test_Suite return Access_Test_Suite is
   Test_Suite_Ptr : constant Access_Test_Suite := new Test_Suite;
begin
   Test_Suite_Ptr.all.Add_Test
     (Test_Case_Access'(new Test_Calculate_Header_Checksum));

   Test_Suite_Ptr.all.Add_Test (Test_Case_Access'(new Test_Allocate));

   Test_Suite_Ptr.all.Add_Test (Test_Case_Access'(new Test_Allocate_Aligned));

   Test_Suite_Ptr.all.Add_Test
     (Test_Case_Access'(new Test_Allocate_Aligned_Exact));

   Test_Suite_Ptr.all.Add_Test
     (Test_Case_Access'(new Test_Allocate_Invalid_Alignment));

   Test_Suite_Ptr.all.Add_Test
     (Test_Case_Access'(new Test_Calculate_Region_Header_Checksum));

   Test_Suite_Ptr.all.Add_Test
     (Test_Case_Access'(new Test_Region_Header_Validation));

   Test_Suite_Ptr.all.Add_Test (Test_Case_Access'(new Test_Multiple_Regions));

   Test_Suite_Ptr.all.Add_Test
     (Test_Case_Access'(new Test_Add_Overlapping_Region));

   return Test_Suite_Ptr;
end Memory_Allocators_Heap_Test_Suite;
