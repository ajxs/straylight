with Ada.Unchecked_Conversion;

with Function_Results; use Function_Results;
with Locks;            use Locks;
with Logging;          use Logging;

package Memory.Allocators.Heap
  with Preelaborate
is
   type Heap_Memory_Region_T;

   type Heap_Memory_Region_Access is access all Heap_Memory_Region_T
   with Convention => C;

   type Heap_Memory_Region_T is record
      Checksum              : Unsigned_64 := 0;
      Heap_Region_Virt_Addr : Virtual_Address_T := Null_Address;
      Heap_Region_Phys_Addr : Physical_Address_T := Null_Physical_Address;
      Heap_Region_Size      : Storage_Count := 0;
      Next_Region           : Heap_Memory_Region_Access := null;
   end record
   with Size => 5 * 8 * 8;

   type Memory_Heap_T is record
      Memory_Regions_List_Head : Heap_Memory_Region_Access;
      --  Window_Base and Window_Size describe the reserved virtual
      --  address window that this heap's regions are mapped within. They
      --  are used to validate that a region pointer refers to a valid heap
      --  address.
      Window_Base              : Virtual_Address_T;
      Window_Size              : Storage_Count;
      Spinlock                 : Spinlock_T;
   end record;

   procedure Allocate
     (Memory_Heap       : in out Memory_Heap_T;
      Size              : Positive;
      Allocation_Result : out Memory_Allocation_Result;
      Result            : out Function_Result;
      Alignment         : Storage_Count := 1);

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
      Size             : Storage_Count;
      Result           : out Function_Result);

   --  Atomically add a new memory region to the heap, and allocate memory.
   --  This is used to avoid data races when growing the heap.
   procedure Add_Memory_Region_To_Heap_And_Allocate
     (Memory_Heap       : in out Memory_Heap_T;
      Virtual_Address   : Virtual_Address_T;
      Physical_Address  : Physical_Address_T;
      Region_Size       : Storage_Count;
      Allocation_Size   : Positive;
      Allocation_Result : out Memory_Allocation_Result;
      Result            : out Function_Result;
      Alignment         : Storage_Count := 1);

   procedure Get_Minimum_Region_Size
     (Allocation_Size     : Positive;
      Alignment           : Storage_Count;
      Minimum_Region_Size : out Storage_Count;
      Result              : out Function_Result);

private
   Logging_Tags_Heap : constant Log_Tags :=
     [Log_Tag_Heap, Log_Tag_Memory, Log_Tag_Memory_Allocators];

   --  This needs to stay in this package to avoid issues re: strict aliasing.
   --  This automatically suppresses the aliasing optimisations.
   --  refer to: https://gcc.gnu.org/onlinedocs/gcc-9.4.0/
   --    gnat_ugn/Optimization-and-Strict-Aliasing.html
   function Convert_Address_To_Heap_Memory_Region_Access is new
     Ada.Unchecked_Conversion (Virtual_Address_T, Heap_Memory_Region_Access);

   --  Reinterprets a region pointer as the address it refers to. This reads
   --  only the pointer value, it does not dereference it, so it is safe to
   --  use to bound-check a pointer prior to accessing the region header.
   function Convert_Heap_Memory_Region_Access_To_Address is new
     Ada.Unchecked_Conversion (Heap_Memory_Region_Access, Virtual_Address_T);

   type Allocation_Header_T is record
      --  The allocation header contains a checksum calculated from the block's
      --  address, size, and allocation status to verify the integrity of the
      --  header. This checksum is also used by the allocator to determine
      --  whether a block is free or allocated.
      Block_Checksum : Unsigned_32;
      Block_Size     : Storage_Count;
   end record
   with Size => 16 * 8;

   Identity_Marker_Free      : constant := 16#AAAA_5555#;
   Identity_Marker_Allocated : constant := 16#5555_AAAA#;

   Heap_Region_Checksum_Identity : constant Unsigned_64 :=
     16#A5A5_A5A5_5A5A_5A5A#;

   Header_Size : constant Storage_Count := Allocation_Header_T'Size / 8;

   Region_Header_Size : constant Storage_Count :=
     Heap_Memory_Region_T'Size / 8;

   function Calculate_Header_Checksum
     (Block_Identity : Unsigned_32;
      Block_Address  : Virtual_Address_T;
      Block_Size     : Storage_Count) return Unsigned_32;

   function Is_Valid_Header_Address
     (Region : Heap_Memory_Region_T; Addr : Virtual_Address_T) return Boolean
   is (Addr >= Region.Heap_Region_Virt_Addr + Region_Header_Size
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
      Block_Size     : Storage_Count) return Boolean
   is (Block_Checksum
       = Calculate_Header_Checksum (Test_Identity, Block_Address, Block_Size))
   with Inline;

   function Is_Block_Free
     (Block_Checksum : Unsigned_32;
      Block_Address  : Virtual_Address_T;
      Block_Size     : Storage_Count) return Boolean
   is (Test_Header_Checksum
         (Identity_Marker_Free, Block_Checksum, Block_Address, Block_Size))
   with Inline;

   function Is_Block_Allocated
     (Block_Checksum : Unsigned_32;
      Block_Address  : Virtual_Address_T;
      Block_Size     : Storage_Count) return Boolean
   is (Test_Header_Checksum
         (Identity_Marker_Allocated,
          Block_Checksum,
          Block_Address,
          Block_Size))
   with Inline;

   function Is_Valid_Header
     (Block_Checksum : Unsigned_32;
      Block_Address  : Virtual_Address_T;
      Block_Size     : Storage_Count) return Boolean
   is (Is_Block_Free (Block_Checksum, Block_Address, Block_Size)
       or else Is_Block_Allocated (Block_Checksum, Block_Address, Block_Size));

   function Calculate_Region_Header_Checksum
     (Region_Address          : Virtual_Address_T;
      Region_Physical_Address : Physical_Address_T;
      Region_Size             : Storage_Count;
      Next_Region             : Heap_Memory_Region_Access) return Unsigned_64;

   function Test_Region_Header_Checksum
     (Region_Checksum         : Unsigned_64;
      Region_Address          : Virtual_Address_T;
      Region_Physical_Address : Physical_Address_T;
      Region_Size             : Storage_Count;
      Next_Region             : Heap_Memory_Region_Access) return Boolean
   is (Region_Checksum
       = Calculate_Region_Header_Checksum
           (Region_Address, Region_Physical_Address, Region_Size, Next_Region))
   with Inline;

end Memory.Allocators.Heap;
