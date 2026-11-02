-------------------------------------------------------------------------------
--  Copyright (c) 2026, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with AUnit.Simple_Test_Cases; use AUnit.Simple_Test_Cases;

with Memory.Page_Pool_Allocator.Test_Cases;
use Memory.Page_Pool_Allocator.Test_Cases;

function Memory.Page_Pool_Allocator_Test_Suite return Access_Test_Suite is
   Test_Suite_Ptr : constant Access_Test_Suite := new Test_Suite;
begin
   Test_Suite_Ptr.all.Add_Test (Test_Case_Access'(new Test_Add_Region));

   Test_Suite_Ptr.all.Add_Test (Test_Case_Access'(new Test_Allocate));

   Test_Suite_Ptr.all.Add_Test
     (Test_Case_Access'(new Test_Allocate_Multiple_Regions));

   Test_Suite_Ptr.all.Add_Test (Test_Case_Access'(new Test_Free));

   Test_Suite_Ptr.all.Add_Test (Test_Case_Access'(new Test_Free_Invalid));

   return Test_Suite_Ptr;
end Memory.Page_Pool_Allocator_Test_Suite;
