with AUnit;
with AUnit.Simple_Test_Cases; use AUnit.Simple_Test_Cases;

package Memory.Page_Pool_Allocator.Test_Cases is
   type Test_Add_Region is new Test_Case with null record;

   overriding
   function Name (T : Test_Add_Region) return AUnit.Message_String
   is (AUnit.Format ("Memory.Page_Pool_Allocator.Add_Region_To_Page_Pool"));

   overriding
   procedure Run_Test (T : in out Test_Add_Region);

   type Test_Allocate is new Test_Case with null record;

   overriding
   function Name (T : Test_Allocate) return AUnit.Message_String
   is (AUnit.Format ("Memory.Page_Pool_Allocator.Allocate"));

   overriding
   procedure Run_Test (T : in out Test_Allocate);

   type Test_Allocate_Multiple_Regions is new Test_Case with null record;

   overriding
   function Name
     (T : Test_Allocate_Multiple_Regions) return AUnit.Message_String
   is (AUnit.Format
         ("Memory.Page_Pool_Allocator.Allocate (Multiple Regions)"));

   overriding
   procedure Run_Test (T : in out Test_Allocate_Multiple_Regions);

   type Test_Free is new Test_Case with null record;

   overriding
   function Name (T : Test_Free) return AUnit.Message_String
   is (AUnit.Format ("Memory.Page_Pool_Allocator.Free"));

   overriding
   procedure Run_Test (T : in out Test_Free);

   type Test_Free_Invalid is new Test_Case with null record;

   overriding
   function Name (T : Test_Free_Invalid) return AUnit.Message_String
   is (AUnit.Format ("Memory.Page_Pool_Allocator.Free (Invalid Address)"));

   overriding
   procedure Run_Test (T : in out Test_Free_Invalid);

end Memory.Page_Pool_Allocator.Test_Cases;
