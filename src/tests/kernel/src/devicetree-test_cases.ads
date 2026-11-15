-------------------------------------------------------------------------------
--  Copyright (c) 2026, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with AUnit;
with AUnit.Simple_Test_Cases; use AUnit.Simple_Test_Cases;

package Devicetree.Test_Cases is
   type Test_Parse_Virt_Devicetree is new Test_Case with null record;

   type Test_Parse_Virt_Devicetree_Without_Exclusion is new Test_Case
   with null record;

   type Test_Parse_Devicetree_Errors is new Test_Case with null record;

   overriding
   function Name (T : Test_Parse_Virt_Devicetree) return AUnit.Message_String
   is (AUnit.Format ("Devicetree.Parse_Devicetree (QEMU virt)"));

   overriding
   function Name
     (T : Test_Parse_Virt_Devicetree_Without_Exclusion)
      return AUnit.Message_String
   is (AUnit.Format
         ("Devicetree.Parse_Devicetree (QEMU virt, nothing excluded)"));

   overriding
   function Name (T : Test_Parse_Devicetree_Errors) return AUnit.Message_String
   is (AUnit.Format ("Devicetree.Parse_Devicetree (errors)"));

   overriding
   procedure Run_Test (T : in out Test_Parse_Virt_Devicetree);

   overriding
   procedure Run_Test
     (T : in out Test_Parse_Virt_Devicetree_Without_Exclusion);

   overriding
   procedure Run_Test (T : in out Test_Parse_Devicetree_Errors);

end Devicetree.Test_Cases;
