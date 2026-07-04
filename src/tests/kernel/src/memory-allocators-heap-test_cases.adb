with AUnit.Assertions; use AUnit.Assertions;

package body Memory.Allocators.Heap.Test_Cases is
   overriding
   procedure Run_Test (T : in out Test_Calculate_Header_Checksum) is
   begin
      Free_Block_Checksum : constant Unsigned_32 :=
        Calculate_Header_Checksum
          (Identity_Marker_Free, To_Address (16#C000_0000#), 16#1000#);

      Allocated_Block_Checksum : constant Unsigned_32 :=
        Calculate_Header_Checksum
          (Identity_Marker_Allocated, To_Address (16#C000_0000#), 16#1000#);

      Assert
        (Free_Block_Checksum = 1789543765, "Free block checksum is correct");

      Assert
        (Allocated_Block_Checksum = 2505423530,
         "Allocated block checksum is correct");

   end Run_Test;

end Memory.Allocators.Heap.Test_Cases;
