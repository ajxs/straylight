-------------------------------------------------------------------------------
--  Copyright (c) 2026, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with AUnit.Test_Suites;       use AUnit.Test_Suites;
with AUnit.Simple_Test_Cases; use AUnit.Simple_Test_Cases;
with Devicetree.Test_Cases;   use Devicetree.Test_Cases;

function Devicetree_Test_Suite return Access_Test_Suite is
   Test_Suite_Ptr : constant Access_Test_Suite := new Test_Suite;
begin
   Test_Suite_Ptr.all.Add_Test
     (Test_Case_Access'(new Test_Parse_Virt_Devicetree));
   Test_Suite_Ptr.all.Add_Test
     (Test_Case_Access'(new Test_Parse_Devicetree_Errors));
   Test_Suite_Ptr.all.Add_Test (Test_Case_Access'(new Test_Parse_Node_Status));

   return Test_Suite_Ptr;
end Devicetree_Test_Suite;
