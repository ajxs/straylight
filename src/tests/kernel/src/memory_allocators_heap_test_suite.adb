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

   return Test_Suite_Ptr;
end Memory_Allocators_Heap_Test_Suite;
