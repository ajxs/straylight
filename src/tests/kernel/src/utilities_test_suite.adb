-------------------------------------------------------------------------------
--  Copyright (c) 2026, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with AUnit.Test_Suites;       use AUnit.Test_Suites;
with AUnit.Simple_Test_Cases; use AUnit.Simple_Test_Cases;
with Utilities.Test_Cases;    use Utilities.Test_Cases;

function Utilities_Test_Suite return Access_Test_Suite is
   Test_Suite_Ptr : constant Access_Test_Suite := new Test_Suite;
begin
   Test_Suite_Ptr.all.Add_Test
     (Test_Case_Access'(new Test_Convert_Wide_Char_To_ASCII));
   Test_Suite_Ptr.all.Add_Test
     (Test_Case_Access'(new Test_Encode_UCS2_Wide_Char_As_UTF8_Buffer));
   Test_Suite_Ptr.all.Add_Test
     (Test_Case_Access'(new Test_Convert_BEU32_To_LEU32));
   Test_Suite_Ptr.all.Add_Test
     (Test_Case_Access'(new Test_Convert_BEU64_To_LEU64));

   return Test_Suite_Ptr;
end Utilities_Test_Suite;
