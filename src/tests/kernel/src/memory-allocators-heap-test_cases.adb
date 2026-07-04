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
   begin
      Add_Memory_Region_To_Heap
        (Test_Heap,
         Heap_Backend'Address,
         Null_Physical_Address,
         Heap_Backend_Size,
         Result);

      Block_Info : constant Heap_Block_Info_Array_T :=
        Get_Heap_Memory_Region_Blocks
          (Test_Heap.Memory_Regions (Test_Heap.Memory_Regions'First));

      Assert
        (Block_Info'Length = 1, "Heap has one block after initialization");
      Assert
        (Block_Info (0).Block_Size = Heap_Backend_Size - Header_Size,
         "Initial block size is correct");

      Allocate (Test_Heap, 16#100#, Allocation_Result, Result);
      Assert (Result = Success, "Allocation succeeded");

      Block_Info_2 : constant Heap_Block_Info_Array_T :=
        Get_Heap_Memory_Region_Blocks
          (Test_Heap.Memory_Regions (Test_Heap.Memory_Regions'First));

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
        (Blocks_Total_Size = Heap_Backend_Size,
         "Total size of blocks matches heap backend size");
      Assert
        (Block_Info_2 (1).Block_Size = 256, "Block successfully allocated");
      Assert
        (Block_Info_2 (0).Block_Size = 3808,
         "Remaining block size is correct");

      Allocate (Test_Heap, 16#200#, Allocation_Result, Result);
      Assert (Result = Success, "Allocation succeeded");

      Block_Info_3 : constant Heap_Block_Info_Array_T :=
        Get_Heap_Memory_Region_Blocks
          (Test_Heap.Memory_Regions (Test_Heap.Memory_Regions'First));

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
        (Blocks_Total_Size = Heap_Backend_Size,
         "Total size of blocks matches heap backend size");
      Assert
        (Block_Info_3 (2).Block_Size = 256,
         "Last allocated block size is correct");
      Assert
        (Block_Info_3 (1).Block_Size = 512,
         "Second allocated block size is correct");
      Assert
        (Block_Info_3 (0).Block_Size = 3280,
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
        Get_Heap_Memory_Region_Blocks
          (Test_Heap.Memory_Regions (Test_Heap.Memory_Regions'First));

      Assert
        (Block_Info'Length = 3,
         "Heap has three blocks after aligned allocation");
      Assert
        (Block_Info (0).Block_Size = 3936,
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
        Get_Heap_Memory_Region_Blocks
          (Test_Heap.Memory_Regions (Test_Heap.Memory_Regions'First));

      Assert
        (Block_Info'Length = 2,
         "Heap has two blocks after aligned allocation");
      Assert
        (Block_Info (0).Block_Is_Allocated = False,
         "Leading free block is not allocated");
      Assert
        (Block_Info (0).Block_Size = 4032,
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

end Memory.Allocators.Heap.Test_Cases;
