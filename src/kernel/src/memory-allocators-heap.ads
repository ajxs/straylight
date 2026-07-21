with Function_Results; use Function_Results;
with Locks;            use Locks;
with Logging;          use Logging;

package Memory.Allocators.Heap
  with Preelaborate
is
   type Heap_Memory_Region_T is record
      Heap_Region_Virt_Addr : Virtual_Address_T := Null_Address;
      Heap_Region_Phys_Addr : Physical_Address_T := Null_Physical_Address;
      Heap_Region_Size      : Storage_Offset := 0;
   end record;

   New_Heap_Max_Memory_Regions : constant := 16;

   type Heap_Memory_Region_List_T is
     array (1 .. New_Heap_Max_Memory_Regions) of Heap_Memory_Region_T;

   type Memory_Heap_T is record
      Memory_Regions : Heap_Memory_Region_List_T;
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

   --  Atomically add a new memory region to the heap, and allocate memory.
   --  This is used to avoid data races when growing the heap.
   procedure Add_Memory_Region_To_Heap_And_Allocate
     (Memory_Heap       : in out Memory_Heap_T;
      Virtual_Address   : Virtual_Address_T;
      Physical_Address  : Physical_Address_T;
      Region_Size       : Storage_Offset;
      Allocation_Size   : Positive;
      Allocation_Result : out Memory_Allocation_Result;
      Result            : out Function_Result;
      Alignment         : Storage_Offset := 1);

   procedure Get_Minimum_Region_Size
     (Allocation_Size     : Positive;
      Alignment           : Storage_Offset;
      Minimum_Region_Size : out Storage_Offset;
      Result              : out Function_Result);

private
   Logging_Tags_Heap : constant Log_Tags :=
     [Log_Tag_Heap, Log_Tag_Memory, Log_Tag_Memory_Allocators];

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

   function Is_Valid_Header_Address
     (Region : Heap_Memory_Region_T; Addr : Virtual_Address_T) return Boolean
   is (Addr >= Region.Heap_Region_Virt_Addr
       and then
         Addr
         <= Region.Heap_Region_Virt_Addr
            + Region.Heap_Region_Size
            - Header_Size)
   with Pure_Function, Inline;

   function Test_Header_Checksum
     (Test_Identity  : Unsigned_32;
      Block_Checksum : Unsigned_32;
      Block_Address  : Virtual_Address_T;
      Block_Size     : Storage_Offset) return Boolean
   is (Block_Checksum
       = Calculate_Header_Checksum (Test_Identity, Block_Address, Block_Size))
   with Inline;

   function Is_Block_Free
     (Block_Checksum : Unsigned_32;
      Block_Address  : Virtual_Address_T;
      Block_Size     : Storage_Offset) return Boolean
   is (Test_Header_Checksum
         (Identity_Marker_Free, Block_Checksum, Block_Address, Block_Size))
   with Inline;

   function Is_Block_Allocated
     (Block_Checksum : Unsigned_32;
      Block_Address  : Virtual_Address_T;
      Block_Size     : Storage_Offset) return Boolean
   is (Test_Header_Checksum
         (Identity_Marker_Allocated,
          Block_Checksum,
          Block_Address,
          Block_Size))
   with Inline;

   function Is_Valid_Header
     (Block_Checksum : Unsigned_32;
      Block_Address  : Virtual_Address_T;
      Block_Size     : Storage_Offset) return Boolean
   is (Is_Block_Free (Block_Checksum, Block_Address, Block_Size)
       or else Is_Block_Allocated (Block_Checksum, Block_Address, Block_Size));

end Memory.Allocators.Heap;
