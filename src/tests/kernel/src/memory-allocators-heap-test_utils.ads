package Memory.Allocators.Heap.Test_Utils
  with Preelaborate
is
   type Block_Info_T is record
      Block_Virt_Address : Virtual_Address_T;
      Block_Phys_Address : Physical_Address_T;
      Block_Size         : Storage_Offset;
      Block_Is_Allocated : Boolean := False;
   end record;

   type Heap_Block_Info_Array_T is array (Natural range <>) of Block_Info_T;

   function Get_Block_Count_In_Heap_Memory_Region
     (Heap_Region : Heap_Memory_Region_T) return Natural;

   function Get_Heap_Memory_Region_Blocks
     (Heap_Region : Heap_Memory_Region_T) return Heap_Block_Info_Array_T;

end Memory.Allocators.Heap.Test_Utils;
