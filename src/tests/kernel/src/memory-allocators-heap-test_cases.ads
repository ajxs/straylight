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

end Memory.Allocators.Heap.Test_Cases;
