-------------------------------------------------------------------------------
--  Copyright (c) 2026, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with AUnit;
with AUnit.Simple_Test_Cases; use AUnit.Simple_Test_Cases;

package Memory.Test_Cases is
   type Test_Is_Valid_Userspace_Address_Range is new Test_Case
   with null record;

   type Test_Do_Memory_Regions_Overlap is new Test_Case with null record;

   overriding
   function Name
     (T : Test_Is_Valid_Userspace_Address_Range) return AUnit.Message_String
   is (AUnit.Format ("Memory.Is_Valid_Userspace_Address_Range"));

   overriding
   function Name
     (T : Test_Do_Memory_Regions_Overlap) return AUnit.Message_String
   is (AUnit.Format ("Memory.Do_Memory_Regions_Overlap"));

   overriding
   procedure Run_Test (T : in out Test_Is_Valid_Userspace_Address_Range);

   overriding
   procedure Run_Test (T : in out Test_Do_Memory_Regions_Overlap);

end Memory.Test_Cases;
