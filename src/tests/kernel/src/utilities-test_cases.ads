-------------------------------------------------------------------------------
--  Copyright (c) 2026, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with AUnit;
with AUnit.Simple_Test_Cases; use AUnit.Simple_Test_Cases;

package Utilities.Test_Cases is
   type Test_Convert_Wide_Char_To_ASCII is new Test_Case with null record;

   type Test_Encode_UCS2_Wide_Char_As_UTF8_Buffer is new Test_Case
   with null record;

   type Test_Convert_BEU32_To_LEU32 is new Test_Case with null record;

   type Test_Convert_BEU64_To_LEU64 is new Test_Case with null record;

   overriding
   function Name
     (T : Test_Convert_Wide_Char_To_ASCII) return AUnit.Message_String
   is (AUnit.Format ("Utilities.Convert_Wide_Char_To_ASCII"));

   overriding
   function Name
     (T : Test_Encode_UCS2_Wide_Char_As_UTF8_Buffer)
      return AUnit.Message_String
   is (AUnit.Format ("Utilities.Encode_UCS2_Wide_Char_As_UTF8_Buffer"));

   overriding
   function Name (T : Test_Convert_BEU32_To_LEU32) return AUnit.Message_String
   is (AUnit.Format ("Utilities.Convert_BEU32_To_LEU32"));

   overriding
   function Name (T : Test_Convert_BEU64_To_LEU64) return AUnit.Message_String
   is (AUnit.Format ("Utilities.Convert_BEU64_To_LEU64"));

   overriding
   procedure Run_Test (T : in out Test_Convert_Wide_Char_To_ASCII);

   overriding
   procedure Run_Test (T : in out Test_Encode_UCS2_Wide_Char_As_UTF8_Buffer);

   overriding
   procedure Run_Test (T : in out Test_Convert_BEU32_To_LEU32);

   overriding
   procedure Run_Test (T : in out Test_Convert_BEU64_To_LEU64);

end Utilities.Test_Cases;
