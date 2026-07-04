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

end Memory.Allocators.Heap.Test_Cases;
