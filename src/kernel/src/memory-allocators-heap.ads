with Function_Results; use Function_Results;
with Locks;            use Locks;
with Logging;          use Logging;

package Memory.Allocators.Heap
  with Preelaborate
is
   type New_Heap_Memory_Region_T is record
      Heap_Region_Virt_Addr : Virtual_Address_T := Null_Address;
      Heap_Region_Phys_Addr : Physical_Address_T := Null_Physical_Address;
      Heap_Region_Size      : Storage_Offset := 0;
   end record;

   New_Heap_Max_Memory_Regions : constant := 16;

   type New_Heap_Memory_Region_List_T is
     array (1 .. New_Heap_Max_Memory_Regions) of New_Heap_Memory_Region_T;

   type Memory_Heap_T is record
      Memory_Regions : New_Heap_Memory_Region_List_T;
      Spinlock       : Spinlock_T;
   end record;

   procedure Allocate
     (Memory_Heap       : in out Memory_Heap_T;
      Size              : Positive;
      Allocation_Result : out Memory_Allocation_Result;
      Result            : out Function_Result;
      Alignment         : Storage_Offset := 1);

   procedure Free
     (Memory_Heap               : in out Memory_Heap_T;
      Allocated_Virtual_Address : Virtual_Address_T;
      Result                    : out Function_Result);

   --  @NOTE: New memory regions will need to be mapped into virtual memory
   --  before being added to the heap. Otherwise a fault will be generated when
   --  the heap allocator attempts to initialise the new block's header.
   procedure Add_Memory_Region_To_Heap
     (Memory_Heap      : in out Memory_Heap_T;
      Virtual_Address  : Virtual_Address_T;
      Physical_Address : Physical_Address_T;
      Size             : Storage_Offset;
      Result           : out Function_Result);

private
   Logging_Tags_Heap : constant Log_Tags := [Log_Tag_Heap, Log_Tag_Memory];

   type Allocation_Header_T is record
      --  The allocation header contains a checksum calculated from the block's
      --  address, size, and allocation status to verify the integrity of the
      --  header. This checksum is also used by the allocator to determine
      --  whether a block is free or allocated.
      Block_Checksum : Unsigned_32;
      Block_Size     : Storage_Offset;
   end record
   with Size => 16 * 8;

   Identity_Marker_Free      : constant := 16#AAAA_5555#;
   Identity_Marker_Allocated : constant := 16#5555_AAAA#;

   Header_Size : constant Storage_Offset := Allocation_Header_T'Size / 8;

   function Calculate_Header_Checksum
     (Block_Identity : Unsigned_32;
      Block_Address  : Virtual_Address_T;
      Block_Size     : Storage_Offset) return Unsigned_32;

end Memory.Allocators.Heap;
