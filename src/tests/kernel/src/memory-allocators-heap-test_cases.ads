with AUnit;
with AUnit.Simple_Test_Cases; use AUnit.Simple_Test_Cases;

package Memory.Allocators.Heap.Test_Cases is
   type Test_Calculate_Header_Checksum is new Test_Case with null record;

   overriding
   function Name
     (T : Test_Calculate_Header_Checksum) return AUnit.Message_String
   is (AUnit.Format ("Memory.Allocators.Heap.Calculate_Header_Checksum"));

   overriding
   procedure Run_Test (T : in out Test_Calculate_Header_Checksum);

   type Test_Allocate is new Test_Case with null record;

   overriding
   function Name (T : Test_Allocate) return AUnit.Message_String
   is (AUnit.Format ("Memory.Allocators.Heap.Allocate"));

   overriding
   procedure Run_Test (T : in out Test_Allocate);

   type Test_Allocate_Aligned is new Test_Case with null record;

   overriding
   function Name (T : Test_Allocate_Aligned) return AUnit.Message_String
   is (AUnit.Format ("Memory.Allocators.Heap.Allocate (Aligned)"));

   overriding
   procedure Run_Test (T : in out Test_Allocate_Aligned);

   type Test_Allocate_Aligned_Exact is new Test_Case with null record;

   overriding
   function Name (T : Test_Allocate_Aligned_Exact) return AUnit.Message_String
   is (AUnit.Format ("Memory.Allocators.Heap.Allocate (Aligned, Exact)"));

   overriding
   procedure Run_Test (T : in out Test_Allocate_Aligned_Exact);

   type Test_Allocate_Invalid_Alignment is new Test_Case with null record;

   overriding
   function Name
     (T : Test_Allocate_Invalid_Alignment) return AUnit.Message_String
   is (AUnit.Format ("Memory.Allocators.Heap.Allocate (Invalid Alignment)"));

   overriding
   procedure Run_Test (T : in out Test_Allocate_Invalid_Alignment);

   type Test_Calculate_Region_Header_Checksum is new Test_Case
   with null record;

   overriding
   function Name
     (T : Test_Calculate_Region_Header_Checksum) return AUnit.Message_String
   is (AUnit.Format
         ("Memory.Allocators.Heap.Calculate_Region_Header_Checksum"));

   overriding
   procedure Run_Test (T : in out Test_Calculate_Region_Header_Checksum);

   type Test_Region_Header_Validation is new Test_Case with null record;

   overriding
   function Name
     (T : Test_Region_Header_Validation) return AUnit.Message_String
   is (AUnit.Format ("Memory.Allocators.Heap.Region Header Validation"));

   overriding
   procedure Run_Test (T : in out Test_Region_Header_Validation);

   type Test_Multiple_Regions is new Test_Case with null record;

   overriding
   function Name (T : Test_Multiple_Regions) return AUnit.Message_String
   is (AUnit.Format ("Memory.Allocators.Heap.Multiple Regions"));

   overriding
   procedure Run_Test (T : in out Test_Multiple_Regions);

   type Test_Add_Overlapping_Region is new Test_Case with null record;

   overriding
   function Name (T : Test_Add_Overlapping_Region) return AUnit.Message_String
   is (AUnit.Format ("Memory.Allocators.Heap.Add_Memory_Region_To_Heap"));

   overriding
   procedure Run_Test (T : in out Test_Add_Overlapping_Region);

end Memory.Allocators.Heap.Test_Cases;
