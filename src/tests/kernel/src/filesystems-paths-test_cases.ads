-------------------------------------------------------------------------------
--  Copyright (c) 2026, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with AUnit;
with AUnit.Simple_Test_Cases; use AUnit.Simple_Test_Cases;

package Filesystems.Paths.Test_Cases is
   type Test_Resolve_Absolute_Path is new Test_Case with null record;

   type Test_Resolve_Dot_Components is new Test_Case with null record;

   type Test_Resolve_Dot_Dot_Components is new Test_Case with null record;

   type Test_Resolve_Relative_Path is new Test_Case with null record;

   type Test_Resolve_Invalid_Path is new Test_Case with null record;

   overriding
   function Name (T : Test_Resolve_Absolute_Path) return AUnit.Message_String
   is (AUnit.Format ("Filesystems.Paths.Resolve_Path (absolute paths)"));

   overriding
   function Name (T : Test_Resolve_Dot_Components) return AUnit.Message_String
   is (AUnit.Format ("Filesystems.Paths.Resolve_Path (. components)"));

   overriding
   function Name
     (T : Test_Resolve_Dot_Dot_Components) return AUnit.Message_String
   is (AUnit.Format ("Filesystems.Paths.Resolve_Path (.. components)"));

   overriding
   function Name (T : Test_Resolve_Relative_Path) return AUnit.Message_String
   is (AUnit.Format ("Filesystems.Paths.Resolve_Path (relative paths)"));

   overriding
   function Name (T : Test_Resolve_Invalid_Path) return AUnit.Message_String
   is (AUnit.Format ("Filesystems.Paths.Resolve_Path (invalid paths)"));

   overriding
   procedure Run_Test (T : in out Test_Resolve_Absolute_Path);

   overriding
   procedure Run_Test (T : in out Test_Resolve_Dot_Components);

   overriding
   procedure Run_Test (T : in out Test_Resolve_Dot_Dot_Components);

   overriding
   procedure Run_Test (T : in out Test_Resolve_Relative_Path);

   overriding
   procedure Run_Test (T : in out Test_Resolve_Invalid_Path);

end Filesystems.Paths.Test_Cases;
