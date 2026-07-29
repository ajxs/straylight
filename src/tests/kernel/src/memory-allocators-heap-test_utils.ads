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

   --  Returns the number of memory regions in a heap's region list.
   function Get_Heap_Region_Count (Memory_Heap : Memory_Heap_T) return Natural;

   --  Returns the region at a zero-based index in a heap's region list.
   --  Raises Program_Error if the index is out of range.
   function Get_Heap_Region
     (Memory_Heap : Memory_Heap_T; Region_Index : Natural)
      return Heap_Memory_Region_T;

   --  Initialises a heap for use in a test, with a region window covering
   --  the memory backing the heap.
   procedure Initialise_Test_Heap
     (Memory_Heap : in out Memory_Heap_T;
      Window_Base : Virtual_Address_T;
      Window_Size : Storage_Offset);

end Memory.Allocators.Heap.Test_Utils;
