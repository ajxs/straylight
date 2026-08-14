with AUnit.Assertions; use AUnit.Assertions;

with Memory.Allocators.Heap.Test_Utils; use Memory.Allocators.Heap.Test_Utils;

package body Memory.Allocators.Heap.Test_Cases is
   overriding
   procedure Run_Test (T : in out Test_Calculate_Header_Checksum) is
      Block_Address : constant Virtual_Address_T := To_Address (16#C000_0000#);
      Block_Size    : constant Storage_Offset := 16#1000#;
   begin
      Free_Block_Checksum : constant Unsigned_32 :=
        Calculate_Header_Checksum
          (Identity_Marker_Free, Block_Address, Block_Size);

      Allocated_Block_Checksum : constant Unsigned_32 :=
        Calculate_Header_Checksum
          (Identity_Marker_Allocated, Block_Address, Block_Size);

      Assert
        (Free_Block_Checksum = 1789543765, "Free block checksum is correct");

      Assert
        (Allocated_Block_Checksum = 2505423530,
         "Allocated block checksum is correct");

      Assert
        (Is_Valid_Header (Free_Block_Checksum, Block_Address, Block_Size),
         "Free block checksum is valid");

      Assert
        (Is_Valid_Header (Allocated_Block_Checksum, Block_Address, Block_Size),
         "Allocated block checksum is valid");

      Assert
        (Is_Block_Free (Free_Block_Checksum, Block_Address, Block_Size),
         "Free block is identified as free");
      Assert
        (not Is_Block_Free
               (Allocated_Block_Checksum, Block_Address, Block_Size),
         "Allocated block is not identified as free");
      Assert
        (Is_Block_Allocated
           (Allocated_Block_Checksum, Block_Address, Block_Size),
         "Allocated block is identified as allocated");
      Assert
        (not Is_Block_Allocated
               (Free_Block_Checksum, Block_Address, Block_Size),
         "Free block is not identified as allocated");

      Assert
        (not Is_Valid_Header
               (Free_Block_Checksum, Block_Address, Block_Size - 1),
         "Free block checksum is invalid for incorrect size");
      Assert
        (not Is_Valid_Header
               (Allocated_Block_Checksum, Block_Address, Block_Size - 1),
         "Allocated block checksum is invalid for incorrect size");
      Assert
        (not Is_Valid_Header
               (Free_Block_Checksum, Block_Address + 1, Block_Size),
         "Free block checksum is invalid for incorrect address");
      Assert
        (not Is_Valid_Header
               (Allocated_Block_Checksum, Block_Address + 1, Block_Size),
         "Allocated block checksum is invalid for incorrect address");
      Assert
        (not Is_Valid_Header
               (Free_Block_Checksum - 1, Block_Address, Block_Size),
         "Free block checksum is invalid for incorrect checksum");

   end Run_Test;

   overriding
   procedure Run_Test (T : in out Test_Allocate) is
      Test_Heap : Memory_Heap_T;

      Heap_Backend_Size : constant := 16#1000#;

      Heap_Backend : aliased array (0 .. Heap_Backend_Size - 1) of Unsigned_8;

      Result : Function_Result := Unset;

      Allocation_Result : Memory_Allocation_Result :=
        (Virtual_Address  => Null_Address,
         Physical_Address => Null_Physical_Address);

      Blocks_Total_Size : Storage_Offset := 0;

      --  The region header is stored at the base of the region itself, so the
      --  usable block area is the region minus that header.
      Block_Area_Size : constant Storage_Offset :=
        Heap_Backend_Size - Region_Header_Size;
   begin
      Initialise_Test_Heap
        (Test_Heap, Heap_Backend'Address, Heap_Backend_Size);

      Add_Memory_Region_To_Heap
        (Test_Heap,
         Heap_Backend'Address,
         Null_Physical_Address,
         Heap_Backend_Size,
         Result);
      Assert (Result = Success, "Adding memory region succeeded");

      Assert
        (Get_Heap_Region_Count (Test_Heap) = 1,
         "Heap has one region after initialization");

      Block_Info : constant Heap_Block_Info_Array_T :=
        Get_Heap_Memory_Region_Blocks (Get_Heap_Region (Test_Heap, 0));

      Assert
        (Block_Info'Length = 1, "Heap has one block after initialization");
      Assert
        (Block_Info (0).Block_Size = Block_Area_Size - Header_Size,
         "Initial block size is correct");

      Allocate (Test_Heap, 16#100#, Allocation_Result, Result);
      Assert (Result = Success, "Allocation succeeded");

      Block_Info_2 : constant Heap_Block_Info_Array_T :=
        Get_Heap_Memory_Region_Blocks (Get_Heap_Region (Test_Heap, 0));

      Assert (Block_Info_2'Length = 2, "Heap has two blocks after allocation");
      Assert
        (Block_Info_2 (0).Block_Is_Allocated = False,
         "Leading free block is not allocated");
      Assert
        (Block_Info_2 (1).Block_Is_Allocated = True,
         "Allocated block is marked as allocated");

      for I in Block_Info_2'Range loop
         Blocks_Total_Size :=
           Blocks_Total_Size + Block_Info_2 (I).Block_Size + Header_Size;
      end loop;

      Assert
        (Blocks_Total_Size = Block_Area_Size,
         "Total size of blocks matches heap block area size");
      Assert
        (Block_Info_2 (1).Block_Size = 256, "Block successfully allocated");
      Assert
        (Block_Info_2 (0).Block_Size = 3768,
         "Remaining block size is correct");

      Allocate (Test_Heap, 16#200#, Allocation_Result, Result);
      Assert (Result = Success, "Allocation succeeded");

      Block_Info_3 : constant Heap_Block_Info_Array_T :=
        Get_Heap_Memory_Region_Blocks (Get_Heap_Region (Test_Heap, 0));

      Assert
        (Block_Info_3'Length = 3, "Heap has three blocks after allocation");
      Assert
        (Block_Info_3 (0).Block_Is_Allocated = False,
         "Leading free block is not allocated");
      Assert
        (Block_Info_3 (1).Block_Is_Allocated = True,
         "First allocated block is marked as allocated");
      Assert
        (Block_Info_3 (2).Block_Is_Allocated = True,
         "Second allocated block is marked as allocated");

      Blocks_Total_Size := 0;
      for I in Block_Info_3'Range loop
         Blocks_Total_Size :=
           Blocks_Total_Size + Block_Info_3 (I).Block_Size + Header_Size;
      end loop;

      Assert
        (Blocks_Total_Size = Block_Area_Size,
         "Total size of blocks matches heap block area size");
      Assert
        (Block_Info_3 (2).Block_Size = 256,
         "Last allocated block size is correct");
      Assert
        (Block_Info_3 (1).Block_Size = 512,
         "Second allocated block size is correct");
      Assert
        (Block_Info_3 (0).Block_Size = 3240,
         "Remaining block size is correct");

      Assert
        (Block_Info_3 (0).Block_Virt_Address
         + Header_Size
         + Block_Info_3 (0).Block_Size
         = Block_Info_3 (1).Block_Virt_Address,
         "First allocated block is contiguous with remaining free block");
      Assert
        (Block_Info_3 (1).Block_Virt_Address
         + Header_Size
         + Block_Info_3 (1).Block_Size
         = Block_Info_3 (2).Block_Virt_Address,
         "Second allocated block is contiguous with first allocated block");

      Assert
        (Block_Info_3 (0).Block_Phys_Address
         + Header_Size
         + Block_Info_3 (0).Block_Size
         = Block_Info_3 (1).Block_Phys_Address,
         "First allocated block physical memory is contiguous "
         & "with remaining free block");
      Assert
        (Block_Info_3 (1).Block_Phys_Address
         + Header_Size
         + Block_Info_3 (1).Block_Size
         = Block_Info_3 (2).Block_Phys_Address,
         "Second allocated block physical memory is contiguous "
         & "with first allocated block");

   end Run_Test;

   overriding
   procedure Run_Test (T : in out Test_Allocate_Aligned) is
      Test_Heap : Memory_Heap_T;

      Heap_Backend_Size : constant := 16#1000#;

      --  Page-align the base address so the alignment arithmetic below is
      --  predictable.
      Heap_Backend : aliased array (0 .. Heap_Backend_Size - 1) of Unsigned_8
      with Alignment => 16#1000#;

      Result : Function_Result := Unset;

      Allocation_Result : Memory_Allocation_Result :=
        (Virtual_Address  => Null_Address,
         Physical_Address => Null_Physical_Address);

      Alignment : constant Storage_Offset := 64;
   begin
      Initialise_Test_Heap
        (Test_Heap, Heap_Backend'Address, Heap_Backend_Size);

      Add_Memory_Region_To_Heap
        (Test_Heap,
         Heap_Backend'Address,
         Null_Physical_Address,
         Heap_Backend_Size,
         Result);
      Assert (Result = Success, "Adding memory region succeeded");

      --  Allocating 100 bytes at a 64-byte alignment from a 4080-byte block
      --  does not land the aligned data address exactly at the end of the
      --  block. This exercises the back-alignment path, which leaves a small
      --  free fragment between the allocated block and the end of the
      --  region.
      Allocate (Test_Heap, 100, Allocation_Result, Result, Alignment);
      Assert (Result = Success, "Aligned allocation succeeded");

      Assert
        (Allocation_Result.Virtual_Address mod Alignment = 0,
         "Allocated address satisfies alignment requirement");

      Block_Info : constant Heap_Block_Info_Array_T :=
        Get_Heap_Memory_Region_Blocks (Get_Heap_Region (Test_Heap, 0));

      Assert
        (Block_Info'Length = 3,
         "Heap has three blocks after aligned allocation");
      Assert
        (Block_Info (0).Block_Size = 3896,
         "Leading free block size is correct");
      Assert
        (Block_Info (0).Block_Is_Allocated = False,
         "Leading free block is not allocated");
      Assert
        (Block_Info (1).Block_Size = 100, "Allocated block size is correct");
      Assert
        (Block_Info (1).Block_Is_Allocated = True,
         "Allocated block is marked as allocated");
      Assert
        (Block_Info (2).Block_Size = 12,
         "Trailing free fragment size is correct");
      Assert
        (Block_Info (2).Block_Is_Allocated = False,
         "Trailing free fragment is not allocated");

   end Run_Test;

   overriding
   procedure Run_Test (T : in out Test_Allocate_Aligned_Exact) is
      Test_Heap : Memory_Heap_T;

      Heap_Backend_Size : constant := 16#1000#;

      Heap_Backend : aliased array (0 .. Heap_Backend_Size - 1) of Unsigned_8
      with Alignment => 16#1000#;

      Result : Function_Result := Unset;

      Allocation_Result : Memory_Allocation_Result :=
        (Virtual_Address  => Null_Address,
         Physical_Address => Null_Physical_Address);

      Alignment : constant Storage_Offset := 16;
   begin
      Initialise_Test_Heap
        (Test_Heap, Heap_Backend'Address, Heap_Backend_Size);

      Add_Memory_Region_To_Heap
        (Test_Heap,
         Heap_Backend'Address,
         Null_Physical_Address,
         Heap_Backend_Size,
         Result);
      Assert (Result = Success, "Adding memory region succeeded");

      --  Since the region's base address is page-aligned, requesting a
      --  16-byte-aligned allocation lands exactly at the natural split
      --  point, leaving no leftover fragment.
      Allocate (Test_Heap, 32, Allocation_Result, Result, Alignment);
      Assert (Result = Success, "Aligned allocation succeeded");

      Assert
        (Allocation_Result.Virtual_Address mod Alignment = 0,
         "Allocated address satisfies alignment requirement");

      Block_Info : constant Heap_Block_Info_Array_T :=
        Get_Heap_Memory_Region_Blocks (Get_Heap_Region (Test_Heap, 0));

      Assert
        (Block_Info'Length = 2,
         "Heap has two blocks after aligned allocation");
      Assert
        (Block_Info (0).Block_Is_Allocated = False,
         "Leading free block is not allocated");
      Assert
        (Block_Info (0).Block_Size = 3992,
         "Remaining free block size is correct");
      Assert
        (Block_Info (1).Block_Is_Allocated = True,
         "Allocated block is marked as allocated");
      Assert
        (Block_Info (1).Block_Size = 32, "Allocated block size is correct");

   end Run_Test;

   overriding
   procedure Run_Test (T : in out Test_Allocate_Invalid_Alignment) is
      Test_Heap : Memory_Heap_T;

      Heap_Backend_Size : constant := 16#1000#;

      Heap_Backend : aliased array (0 .. Heap_Backend_Size - 1) of Unsigned_8;

      Result : Function_Result := Unset;

      Allocation_Result : Memory_Allocation_Result :=
        (Virtual_Address  => Null_Address,
         Physical_Address => Null_Physical_Address);
   begin
      Initialise_Test_Heap
        (Test_Heap, Heap_Backend'Address, Heap_Backend_Size);

      Add_Memory_Region_To_Heap
        (Test_Heap,
         Heap_Backend'Address,
         Null_Physical_Address,
         Heap_Backend_Size,
         Result);
      Assert (Result = Success, "Adding memory region succeeded");

      --  A non-power-of-two alignment is invalid and should be rejected by
      --  the allocator.
      Allocate (Test_Heap, 16#100#, Allocation_Result, Result, 3);
      Assert
        (Result = Invalid_Argument,
         "Allocation with non-power-of-two alignment is rejected");

   end Run_Test;

   overriding
   procedure Run_Test (T : in out Test_Calculate_Region_Header_Checksum) is
      Region_Address : constant Virtual_Address_T :=
        To_Address (16#C000_0000#);

      Region_Phys_Address : constant Physical_Address_T :=
        Physical_Address_T (To_Address (16#8000_0000#));

      Region_Size : constant Storage_Offset := 16#1000#;

      --  The checksum only reads the pointer's value, so this does not need
      --  to refer to a real region.
      Next_Region : constant Heap_Memory_Region_Access :=
        Convert_Address_To_Heap_Memory_Region_Access
          (To_Address (16#D000_0000#));
   begin
      Checksum : constant Unsigned_64 :=
        Calculate_Region_Header_Checksum
          (Region_Address, Region_Phys_Address, Region_Size, null);

      Checksum_With_Next : constant Unsigned_64 :=
        Calculate_Region_Header_Checksum
          (Region_Address, Region_Phys_Address, Region_Size, Next_Region);

      Assert
        (Checksum = 11936128515945679450, "Region header checksum is correct");
      Assert
        (Checksum_With_Next = 11936128518898469466,
         "Region header checksum with a next region is correct");

      Assert
        (Test_Region_Header_Checksum
           (Checksum, Region_Address, Region_Phys_Address, Region_Size, null),
         "Region header checksum is valid");
      Assert
        (Test_Region_Header_Checksum
           (Checksum_With_Next,
            Region_Address,
            Region_Phys_Address,
            Region_Size,
            Next_Region),
         "Region header checksum with a next region is valid");

      --  Every field the checksum covers must invalidate it when changed.
      Assert
        (not Test_Region_Header_Checksum
               (Checksum,
                Region_Address + 1,
                Region_Phys_Address,
                Region_Size,
                null),
         "Region header checksum is invalid for incorrect address");
      Assert
        (not Test_Region_Header_Checksum
               (Checksum,
                Region_Address,
                Region_Phys_Address + 1,
                Region_Size,
                null),
         "Region header checksum is invalid for incorrect physical address");
      Assert
        (not Test_Region_Header_Checksum
               (Checksum,
                Region_Address,
                Region_Phys_Address,
                Region_Size - 1,
                null),
         "Region header checksum is invalid for incorrect size");
      Assert
        (not Test_Region_Header_Checksum
               (Checksum,
                Region_Address,
                Region_Phys_Address,
                Region_Size,
                Next_Region),
         "Region header checksum is invalid for incorrect next region");
      Assert
        (not Test_Region_Header_Checksum
               (Checksum - 1,
                Region_Address,
                Region_Phys_Address,
                Region_Size,
                null),
         "Region header checksum is invalid for incorrect checksum");

      --  The identity marker exists so that a zeroed region header does not
      --  validate as correct.
      Assert
        (not Test_Region_Header_Checksum
               (0, Null_Address, Null_Physical_Address, 0, null),
         "Zeroed region header does not validate");

   end Run_Test;

   overriding
   procedure Run_Test (T : in out Test_Region_Header_Validation) is
      Test_Heap : Memory_Heap_T;

      Heap_Backend_Size : constant := 16#1000#;

      Heap_Backend : aliased array (0 .. Heap_Backend_Size - 1) of Unsigned_8;

      Result : Function_Result := Unset;

      Allocation_Result : Memory_Allocation_Result :=
        (Virtual_Address  => Null_Address,
         Physical_Address => Null_Physical_Address);

      Allocated_Address : Virtual_Address_T := Null_Address;
   begin
      --  A region lying outside the heap's window must be rejected by the
      --  bounds check, before its header is read.
      Initialise_Test_Heap
        (Test_Heap, Heap_Backend'Address + 16#1_0000#, Heap_Backend_Size);

      Add_Memory_Region_To_Heap
        (Test_Heap,
         Heap_Backend'Address,
         Null_Physical_Address,
         Heap_Backend_Size,
         Result);
      Assert (Result = Success, "Adding memory region succeeded");

      Allocate (Test_Heap, 16#100#, Allocation_Result, Result);
      Assert
        (Result = Region_Not_Mapped,
         "Region outside the heap window is rejected");

      --  With a window that covers the region, the same allocation succeeds.
      Initialise_Test_Heap
        (Test_Heap, Heap_Backend'Address, Heap_Backend_Size);

      Add_Memory_Region_To_Heap
        (Test_Heap,
         Heap_Backend'Address,
         Null_Physical_Address,
         Heap_Backend_Size,
         Result);
      Assert (Result = Success, "Adding memory region succeeded");

      Allocate (Test_Heap, 16#100#, Allocation_Result, Result);
      Assert (Result = Success, "Allocation from a valid region succeeded");

      Allocated_Address := Allocation_Result.Virtual_Address;

      Corrupt_Region_Header : declare
         Region_Header : Heap_Memory_Region_T
         with Import, Alignment => 1, Address => Heap_Backend'Address;
      begin
         --  Corrupting a covered field without updating the checksum must be
         --  detected by both the allocation and the free paths.
         Region_Header.Heap_Region_Size := Region_Header.Heap_Region_Size - 1;

         Allocate (Test_Heap, 16#100#, Allocation_Result, Result);
         Assert
           (Result = Region_Not_Mapped,
            "Corrupted region size is detected when allocating");

         Free (Test_Heap, Allocated_Address, Result);
         Assert
           (Result = Region_Not_Mapped,
            "Corrupted region size is detected when freeing");

         Region_Header.Heap_Region_Size := Region_Header.Heap_Region_Size + 1;

         --  Restoring the field restores the header's validity.
         Allocate (Test_Heap, 16#100#, Allocation_Result, Result);
         Assert
           (Result = Success, "Restored region header validates correctly");

         --  Corrupting the checksum itself must also be detected.
         Region_Header.Checksum := Region_Header.Checksum + 1;

         Allocate (Test_Heap, 16#100#, Allocation_Result, Result);
         Assert
           (Result = Region_Not_Mapped,
            "Corrupted region checksum is detected when allocating");
      end Corrupt_Region_Header;

   end Run_Test;

   overriding
   procedure Run_Test (T : in out Test_Multiple_Regions) is
      Test_Heap : Memory_Heap_T;

      Region_Size : constant := 16#1000#;

      --  Both regions are carved out of a single backing array so that the
      --  heap's region window covers them both.
      Heap_Backend : aliased array (0 .. (2 * Region_Size) - 1) of Unsigned_8;

      Result : Function_Result := Unset;

      Allocation_Result : Memory_Allocation_Result :=
        (Virtual_Address  => Null_Address,
         Physical_Address => Null_Physical_Address);

      Block_Area_Size : constant Storage_Count :=
        Region_Size - Region_Header_Size;
   begin
      Initialise_Test_Heap (Test_Heap, Heap_Backend'Address, 2 * Region_Size);

      Region_One_Address : constant Virtual_Address_T := Heap_Backend'Address;

      Region_Two_Address : constant Virtual_Address_T :=
        Heap_Backend'Address + Region_Size;

      Add_Memory_Region_To_Heap
        (Test_Heap,
         Region_One_Address,
         Null_Physical_Address,
         Region_Size,
         Result);
      Assert (Result = Success, "Adding first memory region succeeded");
      Assert (Get_Heap_Region_Count (Test_Heap) = 1, "Heap has one region");

      --  Appending to a non-empty list exercises the tail walk, the overlap
      --  check against the existing region, and the recalculation of the
      --  previous tail's header checksum.
      Add_Memory_Region_To_Heap
        (Test_Heap,
         Region_Two_Address,
         Null_Physical_Address + Region_Size,
         Region_Size,
         Result);
      Assert (Result = Success, "Adding second memory region succeeded");
      Assert (Get_Heap_Region_Count (Test_Heap) = 2, "Heap has two regions");

      Region_One : constant Heap_Memory_Region_T :=
        Get_Heap_Region (Test_Heap, 0);

      Region_Two : constant Heap_Memory_Region_T :=
        Get_Heap_Region (Test_Heap, 1);

      Assert
        (Region_One.Heap_Region_Virt_Addr = Region_One_Address,
         "First region is at the expected address");
      Assert
        (Region_Two.Heap_Region_Virt_Addr = Region_Two_Address,
         "Second region is at the expected address");
      Assert
        (Region_One.Next_Region /= null,
         "First region links to the second region");
      Assert (Region_Two.Next_Region = null, "Second region is the list tail");

      --  The first region's checksum was recalculated when the second region
      --  was appended to it. Allocating walks the list and validates every
      --  region header, so this fails if that recalculation was missed.
      Allocate (Test_Heap, 16#100#, Allocation_Result, Result);
      Assert (Result = Success, "Allocation from the first region succeeded");

      Assert
        (Allocation_Result.Virtual_Address > Region_One_Address
         and then Allocation_Result.Virtual_Address < Region_Two_Address,
         "Allocation was satisfied from the first region");

      --  This request cannot fit in the remaining space of the first region,
      --  so the allocator must traverse into the second region.
      Allocate (Test_Heap, Block_Area_Size - 256, Allocation_Result, Result);
      Assert (Result = Success, "Allocation from the second region succeeded");

      Assert
        (Allocation_Result.Virtual_Address > Region_Two_Address,
         "Allocation was satisfied from the second region");

      --  Freeing must also traverse the list to find the owning region.
      Free (Test_Heap, Allocation_Result.Virtual_Address, Result);
      Assert (Result = Success, "Freeing from the second region succeeded");

   end Run_Test;

   overriding
   procedure Run_Test (T : in out Test_Add_Overlapping_Region) is
      Test_Heap : Memory_Heap_T;

      Region_Size : constant := 16#1000#;

      Heap_Backend : aliased array (0 .. (2 * Region_Size) - 1) of Unsigned_8;

      Result : Function_Result := Unset;
   begin
      Initialise_Test_Heap (Test_Heap, Heap_Backend'Address, 2 * Region_Size);

      Add_Memory_Region_To_Heap
        (Test_Heap,
         Heap_Backend'Address,
         Null_Physical_Address,
         Region_Size,
         Result);
      Assert (Result = Success, "Adding first memory region succeeded");

      --  A region starting half way into the existing region overlaps it,
      --  and must be rejected rather than added to the list.
      Add_Memory_Region_To_Heap
        (Test_Heap,
         Heap_Backend'Address + (Region_Size / 2),
         Null_Physical_Address + (Region_Size / 2),
         Region_Size,
         Result);
      Assert
        (Result = Region_Is_Overlapping,
         "Overlapping memory region is rejected");
      Assert
        (Get_Heap_Region_Count (Test_Heap) = 1,
         "Rejected region was not added to the list");

      --  A region smaller than the combined region and block headers cannot
      --  hold a valid starting block.
      Add_Memory_Region_To_Heap
        (Test_Heap,
         Heap_Backend'Address + Region_Size,
         Null_Physical_Address + Region_Size,
         Region_Header_Size + Header_Size - 1,
         Result);
      Assert
        (Result = Invalid_Argument, "Undersized memory region is rejected");
      Assert
        (Get_Heap_Region_Count (Test_Heap) = 1,
         "Undersized region was not added to the list");

   end Run_Test;

end Memory.Allocators.Heap.Test_Cases;
