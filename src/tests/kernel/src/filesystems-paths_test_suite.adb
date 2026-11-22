-------------------------------------------------------------------------------
--  Copyright (c) 2026, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with AUnit.Simple_Test_Cases;      use AUnit.Simple_Test_Cases;
with Filesystems.Paths.Test_Cases; use Filesystems.Paths.Test_Cases;

function Filesystems.Paths_Test_Suite return Access_Test_Suite is
   Test_Suite_Ptr : constant Access_Test_Suite := new Test_Suite;
begin
   Test_Suite_Ptr.all.Add_Test
     (Test_Case_Access'(new Test_Resolve_Absolute_Path));
   Test_Suite_Ptr.all.Add_Test
     (Test_Case_Access'(new Test_Resolve_Dot_Components));
   Test_Suite_Ptr.all.Add_Test
     (Test_Case_Access'(new Test_Resolve_Dot_Dot_Components));
   Test_Suite_Ptr.all.Add_Test
     (Test_Case_Access'(new Test_Resolve_Relative_Path));
   Test_Suite_Ptr.all.Add_Test
     (Test_Case_Access'(new Test_Resolve_Invalid_Path));

   return Test_Suite_Ptr;
end Filesystems.Paths_Test_Suite;
