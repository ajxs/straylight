package body Memory.Allocators.Heap is
   function Calculate_Header_Checksum
     (Block_Identity : Unsigned_32;
      Block_Address  : Virtual_Address_T;
      Block_Size     : Storage_Offset) return Unsigned_32 is
   begin
      Size_Bits : constant Unsigned_64 := Unsigned_64 (Block_Size);

      return
        Block_Identity
        xor Get_Address_Word_Low (Block_Address)
        xor Get_Address_Word_High (Block_Address)
        xor Get_Dword_Word_Low (Size_Bits)
        xor Get_Dword_Word_High (Size_Bits);

   exception
      when Constraint_Error =>
         Log_Error
           ("Constraint_Error: Calculate_Header_Checksum", Logging_Tags_Heap);
         return 0;
   end Calculate_Header_Checksum;

   function Is_Allocated_Address_In_Region
     (Region : Heap_Memory_Region_T; Addr : Virtual_Address_T) return Boolean
   is (Addr >= Region.Heap_Region_Virt_Addr + Region_Header_Size + Header_Size
       and then Addr < Region.Heap_Region_Virt_Addr + Region.Heap_Region_Size)
   with Pure_Function, Inline;

   function Is_Valid_Alignment (Alignment : Storage_Offset) return Boolean is
   begin
      return
        Alignment > 0
        and then (Unsigned_64 (Alignment) and Unsigned_64 (Alignment - 1)) = 0;
   exception
      when Constraint_Error =>
         return False;
   end Is_Valid_Alignment;

   ----------------------------------------------------------------------------
   --  Tests if a given region intersects with a given address range.
   ----------------------------------------------------------------------------
   function Is_Region_Intersecting
     (Region           : Heap_Memory_Region_T;
      Virtual_Address  : Virtual_Address_T;
      Physical_Address : Physical_Address_T;
      Length           : Storage_Offset) return Boolean
   is ((Virtual_Address
        < (Region.Heap_Region_Virt_Addr + Region.Heap_Region_Size)
        and then (Virtual_Address + Length) > Region.Heap_Region_Virt_Addr)
       or else
         (Physical_Address
          < (Region.Heap_Region_Phys_Addr + Region.Heap_Region_Size)
          and then (Physical_Address + Length) > Region.Heap_Region_Phys_Addr))
   with Pure_Function;

   procedure Align_Address_Downwards
     (Virtual_Addr     : in out Virtual_Address_T;
      Physical_Addr    : in out Physical_Address_T;
      Alignment        : Storage_Offset;
      Alignment_Offset : out Storage_Offset;
      Result           : out Function_Result) is
   begin
      if Alignment = 1 then
         Alignment_Offset := 0;
         Result := Success;
         return;
      end if;

      Alignment_Offset := Virtual_Addr mod Alignment;

      if Virtual_Addr mod Alignment /= 0 then
         Virtual_Addr := Virtual_Addr - Alignment_Offset;
         Physical_Addr := Physical_Addr - Alignment_Offset;
      end if;

      Result := Success;
   exception
      when Constraint_Error =>
         Result := Constraint_Exception;
   end Align_Address_Downwards;

   procedure Set_New_Block_Header
     (Block_Address     : Virtual_Address_T;
      Block_Size        : Storage_Offset;
      New_Block_Is_Free : Boolean := True) is
   begin
      New_Block : Allocation_Header_T
      with Import, Alignment => 1, Address => Block_Address;

      New_Block :=
        (Calculate_Header_Checksum
           ((if New_Block_Is_Free
             then Identity_Marker_Free
             else Identity_Marker_Allocated),
            Block_Address,
            Block_Size),
         Block_Size);
   end Set_New_Block_Header;

   procedure Allocate_In_Region
     (Region            : Heap_Memory_Region_T;
      Size              : Storage_Offset;
      Allocation_Result : out Memory_Allocation_Result;
      Result            : out Function_Result;
      Alignment         : Storage_Offset := 1)
   is
      Current_Block_Address    : Virtual_Address_T :=
        Region.Heap_Region_Virt_Addr + Region_Header_Size;
      Current_Physical_Address : Physical_Address_T :=
        Region.Heap_Region_Phys_Addr + Region_Header_Size;

      Alignment_Offset : Storage_Offset := 0;
   begin
      Allocation_Result := (Null_Address, Null_Physical_Address);

      while Is_Valid_Header_Address (Region, Current_Block_Address) loop
         Current_Block : Allocation_Header_T
         with Import, Alignment => 1, Address => Current_Block_Address;

         --  If we reach an invalid heap region, return an error.
         if not Is_Valid_Header
                  (Current_Block.Block_Checksum,
                   Current_Block_Address,
                   Current_Block.Block_Size)
         then
            Log_Error ("Invalid heap block encountered.", Logging_Tags_Heap);
            Result := Region_Not_Mapped;
            return;
         end if;

         if Is_Block_Free
              (Current_Block.Block_Checksum,
               Current_Block_Address,
               Current_Block.Block_Size)
           and then Current_Block.Block_Size >= Size
         then
            --  If the current block is big enough to satisfy the allocation
            --  request, check if we can split the block into two blocks. With
            --  the tail block being the allocated block.
            --  First check whether we can align the new second block address
            --  downwards to satisfy the alignment requirement while still
            --  keeping enough space within the original block to be valid.
            --  If not, check whether we can allocate from the start of the
            --  current block.
            End_Of_Current_Block : constant Virtual_Address_T :=
              Current_Block_Address + Header_Size + Current_Block.Block_Size;

            End_Of_Current_Block_Phys : constant Physical_Address_T :=
              Current_Physical_Address
              + Header_Size
              + Current_Block.Block_Size;

            --  First calculate the new block's *data* address, then align it
            --  downwards to satisfy the alignment requirements.
            --  Note that we need to align the *data* address, not the header
            --  address.
            New_Block_Data_Addr : Virtual_Address_T :=
              End_Of_Current_Block - Size;

            New_Block_Data_Addr_Phys : Physical_Address_T :=
              End_Of_Current_Block_Phys - Size;

            Align_Address_Downwards
              (New_Block_Data_Addr,
               New_Block_Data_Addr_Phys,
               Alignment,
               Alignment_Offset,
               Result);
            if Is_Error (Result) then
               return;
            end if;

            --  Get the header address of the new block, and check that there's
            --  still enough space in the original block to hold a valid block
            --  header.
            New_Block_Addr : constant Virtual_Address_T :=
              New_Block_Data_Addr - Header_Size;

            New_Block_Addr_Phys : constant Physical_Address_T :=
              New_Block_Data_Addr_Phys - Header_Size;

            Remaining_Size_In_Original_Block : constant Storage_Offset :=
              New_Block_Addr - Current_Block_Address - Header_Size;

            --  If the remaining size in the original block is big enough to
            --  hold a valid block, resize the current block, and create a new
            --  block at the end of the current block.
            if Remaining_Size_In_Original_Block > Header_Size then
               Current_Block.Block_Size := Remaining_Size_In_Original_Block;

               --  Update the identity checksum when the size changes.
               Current_Block.Block_Checksum :=
                 Calculate_Header_Checksum
                   (Identity_Marker_Free,
                    Current_Block_Address,
                    Current_Block.Block_Size);

               --  Take into account that the alignment offset by which this
               --  block was moved back may have left some space at the end of
               --  the block that is too small to hold a valid block. If so,
               --  include that space in the size of the new block.
               Effective_Size : constant Storage_Offset :=
                 Size
                 + (if Alignment_Offset <= Header_Size
                    then Alignment_Offset
                    else 0);

               Set_New_Block_Header (New_Block_Addr, Effective_Size, False);

               Allocation_Result :=
                 (New_Block_Addr + Header_Size,
                  New_Block_Addr_Phys + Header_Size);

               --  If moving the second block back to satisfy alignment left
               --  enough space for a new block at the end of this block,
               --  allocate it.
               if Alignment_Offset > Header_Size then
                  Set_New_Block_Header
                    (Allocation_Result.Virtual_Address + Size,
                     Alignment_Offset - Header_Size,
                     True);
               end if;

               Result := Success;
               return;
            elsif To_Integer (Current_Block_Address + Header_Size)
              mod Integer_Address (Alignment)
              = 0
            then
               --  If the remaining size in the first block is too small to
               --  hold a valid block, but the current block is aligned,
               --  allocate from the start of the current block.
               --  If there's enough space left over in the current block,
               --  create a new block at the end.
               Remaining_Size : constant Storage_Offset :=
                 Current_Block.Block_Size - Size - Header_Size;

               Allocation_Result :=
                 (Current_Block_Address + Header_Size,
                  Current_Physical_Address + Header_Size);

               --  Check if enough space is left over for a new block.
               if Remaining_Size > Header_Size then
                  Current_Block.Block_Size := Size;

                  Set_New_Block_Header
                    (Allocation_Result.Virtual_Address + Size,
                     Remaining_Size,
                     True);
               end if;

               Current_Block.Block_Checksum :=
                 Calculate_Header_Checksum
                   (Identity_Marker_Allocated,
                    Current_Block_Address,
                    Current_Block.Block_Size);

               Result := Success;
               return;
            end if;
         end if;

         --  Move to check the next block in the heap.
         Current_Block_Address :=
           Current_Block_Address + Header_Size + Current_Block.Block_Size;
         Current_Physical_Address :=
           Current_Physical_Address + Header_Size + Current_Block.Block_Size;
      end loop;

      Log_Error
        ("No space in heap region to satisfy allocation request: "
         & Size'Image,
         Logging_Tags_Heap);

      Result := No_Space_In_Region;
   exception
      when Constraint_Error =>
         Result := Constraint_Exception;
   end Allocate_In_Region;

   function Validate_Heap_Memory_Region_Pointer
     (Memory_Heap : Memory_Heap_T; Region_Ptr : Heap_Memory_Region_Access)
      return Boolean is
   begin
      if Region_Ptr = null then
         return False;
      end if;

      Region_Address : constant Virtual_Address_T :=
        Convert_Heap_Memory_Region_Access_To_Address (Region_Ptr);

      --  Test whether the pointer falls within the heap's reserved virtual
      --  address window. This helps ensure that unmapped pointers aren't
      --  dereferenced. Note that this can't perfectly guarantee that the
      --  address is actually mapped, only that it lies within the window of
      --  valid addresses.
      if Region_Address < Memory_Heap.Window_Base
        or else
          Region_Address
          > Memory_Heap.Window_Base
            + Memory_Heap.Window_Size
            - Region_Header_Size
      then
         return False;
      end if;

      return
        Region_Ptr.all'Address = Region_Ptr.all.Heap_Region_Virt_Addr
        and then
          Test_Region_Header_Checksum
            (Region_Ptr.all.Checksum,
             Region_Ptr.all.Heap_Region_Virt_Addr,
             Region_Ptr.all.Heap_Region_Phys_Addr,
             Region_Ptr.all.Heap_Region_Size,
             Region_Ptr.all.Next_Region);
   exception
      when Constraint_Error =>
         Log_Error
           ("Constraint_Error: Validate_Heap_Memory_Region_Pointer",
            Logging_Tags_Heap);
         return False;
   end Validate_Heap_Memory_Region_Pointer;

   procedure Allocate_Unlocked
     (Memory_Heap       : in out Memory_Heap_T;
      Size              : Positive;
      Allocation_Result : out Memory_Allocation_Result;
      Result            : out Function_Result;
      Alignment         : Storage_Offset := 1)
   is
      Curr_Region : Heap_Memory_Region_Access :=
        Memory_Heap.Memory_Regions_List_Head;
   begin
      if not Is_Valid_Alignment (Alignment) then
         Log_Error
           ("Invalid alignment argument: " & Alignment'Image,
            Logging_Tags_Heap);
         Result := Invalid_Argument;
         return;
      end if;

      Log_Debug
        ("Allocating heap memory:"
         & ASCII.LF
         & "  Size:      "
         & Size'Image
         & ASCII.LF
         & "  Alignment: "
         & Alignment'Image,
         Logging_Tags_Heap);

      while Curr_Region /= null loop
         if not Validate_Heap_Memory_Region_Pointer (Memory_Heap, Curr_Region)
         then
            Log_Error
              ("Invalid heap region header: " & Curr_Region.all'Address'Image,
               Logging_Tags_Heap);

            Result := Region_Not_Mapped;
            return;
         end if;

         Allocate_In_Region
           (Curr_Region.all,
            Storage_Offset (Size),
            Allocation_Result,
            Result,
            Alignment);

         --  If the allocation was successful, or if it failed for a reason
         --  other than heap exhaustion, return.
         if Result = Success then
            Log_Debug
              ("Allocated heap memory:"
               & ASCII.LF
               & "  VAddr: "
               & Allocation_Result.Virtual_Address'Image
               & ASCII.LF
               & "  PAddr: "
               & Allocation_Result.Physical_Address'Image
               & ASCII.LF
               & "  Size:  "
               & Size'Image,
               Logging_Tags_Heap);

            --  Zero the new block's memory.
            Set (Allocation_Result.Virtual_Address, 0, Size);

            return;
         elsif Is_Error (Result) then
            return;
         end if;

         Curr_Region := Curr_Region.all.Next_Region;
      end loop;

      Log_Error
        ("Heap exhausted: unable to allocate " & Size'Image & " bytes.",
         Logging_Tags_Heap);

      Result := Not_Enough_Memory_Available;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Allocate", Logging_Tags_Heap);
         Result := Constraint_Exception;
   end Allocate_Unlocked;

   procedure Allocate
     (Memory_Heap       : in out Memory_Heap_T;
      Size              : Positive;
      Allocation_Result : out Memory_Allocation_Result;
      Result            : out Function_Result;
      Alignment         : Storage_Offset := 1) is
   begin
      Acquire_Spinlock (Memory_Heap.Spinlock);

      Allocate_Unlocked
        (Memory_Heap, Size, Allocation_Result, Result, Alignment);

      Release_Spinlock (Memory_Heap.Spinlock);
   end Allocate;

   procedure Coalesce_Free_Blocks_In_Region
     (Memory_Heap_Region : Heap_Memory_Region_T; Result : out Function_Result)
   is
      Current_Block_Address : Virtual_Address_T :=
        Memory_Heap_Region.Heap_Region_Virt_Addr + Region_Header_Size;

      Next_Block_Address : Virtual_Address_T := Null_Address;
   begin
      --  Loop over the heap, checking for adjacent free blocks.
      --  If two adjacent free blocks are found, merge them into one block.
      while Is_Valid_Header_Address (Memory_Heap_Region, Current_Block_Address)
      loop
         Current_Block : Allocation_Header_T
         with Import, Alignment => 1, Address => Current_Block_Address;

         if not Is_Valid_Header
                  (Current_Block.Block_Checksum,
                   Current_Block_Address,
                   Current_Block.Block_Size)
         then
            Log_Error
              ("Invalid heap block encountered while coalescing free blocks.",
               Logging_Tags_Heap);

            Result := Region_Not_Mapped;
            return;
         end if;

         Next_Block_Address :=
           Current_Block_Address + Header_Size + Current_Block.Block_Size;

         if not Is_Valid_Header_Address
                  (Memory_Heap_Region, Next_Block_Address)
         then
            --  If the next block is outside the heap, we're done.
            Result := Success;
            return;
         end if;

         Next_Block : Allocation_Header_T
         with Import, Alignment => 1, Address => Next_Block_Address;

         if not Is_Valid_Header
                  (Next_Block.Block_Checksum,
                   Next_Block_Address,
                   Next_Block.Block_Size)
         then
            Result := Region_Not_Mapped;
            return;
         end if;

         if Is_Block_Free
              (Current_Block.Block_Checksum,
               Current_Block_Address,
               Current_Block.Block_Size)
           and then
             Is_Block_Free
               (Next_Block.Block_Checksum,
                Next_Block_Address,
                Next_Block.Block_Size)
         then
            --  If the current block and the next block are both free, merge
            --  them into a single block.
            Current_Block.Block_Size :=
              Current_Block.Block_Size + Header_Size + Next_Block.Block_Size;

            --  Update the identity checksum when the size changes.
            Current_Block.Block_Checksum :=
              Calculate_Header_Checksum
                (Identity_Marker_Free,
                 Current_Block_Address,
                 Current_Block.Block_Size);
         else
            --  Otherwise move to check the next block in the heap.
            Current_Block_Address := Next_Block_Address;
         end if;
      end loop;

      Result := Success;
   exception
      when Constraint_Error =>
         Result := Constraint_Exception;
   end Coalesce_Free_Blocks_In_Region;

   procedure Free_Allocation_In_Region
     (Memory_Heap_Region        : Heap_Memory_Region_T;
      Allocated_Virtual_Address : Virtual_Address_T;
      Result                    : out Function_Result) is
   begin
      Block_Header : Allocation_Header_T
      with
        Import,
        Alignment => 1,
        Address   => Allocated_Virtual_Address - Header_Size;

      if not Is_Block_Allocated
               (Block_Header.Block_Checksum,
                Allocated_Virtual_Address - Header_Size,
                Block_Header.Block_Size)
      then
         Log_Error
           ("Attempted to free an unallocated block: "
            & Allocated_Virtual_Address'Image,
            Logging_Tags_Heap);

         Result := Invalid_Argument;
         return;
      end if;

      Block_Header.Block_Checksum :=
        Calculate_Header_Checksum
          (Identity_Marker_Free,
           Allocated_Virtual_Address - Header_Size,
           Block_Header.Block_Size);

      --  Coalesce any adjacent free blocks.
      --  Result set by this call.
      Coalesce_Free_Blocks_In_Region (Memory_Heap_Region, Result);
   exception
      when Constraint_Error =>
         Log_Error
           ("Constraint_Error: Free_Allocation_In_Region", Logging_Tags_Heap);
         Result := Constraint_Exception;
   end Free_Allocation_In_Region;

   procedure Free_Unlocked
     (Memory_Heap               : Memory_Heap_T;
      Allocated_Virtual_Address : Virtual_Address_T;
      Result                    : out Function_Result)
   is
      Curr_Region : Heap_Memory_Region_Access :=
        Memory_Heap.Memory_Regions_List_Head;
   begin
      if Allocated_Virtual_Address = Null_Address then
         Log_Error ("Attempted to free a null address.", Logging_Tags_Heap);
         Result := Invalid_Argument;
         return;
      end if;

      Log_Debug
        ("Freeing heap memory: " & Allocated_Virtual_Address'Image,
         Logging_Tags_Heap);

      while Curr_Region /= null loop
         if not Validate_Heap_Memory_Region_Pointer (Memory_Heap, Curr_Region)
         then
            Log_Error
              ("Invalid heap region header checksum: "
               & Curr_Region.all'Address'Image,
               Logging_Tags_Heap);

            Result := Region_Not_Mapped;
            return;
         end if;

         if Is_Allocated_Address_In_Region
              (Curr_Region.all, Allocated_Virtual_Address)
         then
            Free_Allocation_In_Region
              (Curr_Region.all, Allocated_Virtual_Address, Result);
            return;
         end if;

         Curr_Region := Curr_Region.all.Next_Region;
      end loop;

      Log_Error
        ("Attempted to free an address not in the heap: "
         & Allocated_Virtual_Address'Image,
         Logging_Tags_Heap);

      Result := Address_Not_In_Heap;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Free", Logging_Tags_Heap);
         Result := Constraint_Exception;
   end Free_Unlocked;

   procedure Free
     (Memory_Heap               : in out Memory_Heap_T;
      Allocated_Virtual_Address : Virtual_Address_T;
      Result                    : out Function_Result) is
   begin
      Acquire_Spinlock (Memory_Heap.Spinlock);

      Free_Unlocked (Memory_Heap, Allocated_Virtual_Address, Result);

      Release_Spinlock (Memory_Heap.Spinlock);
   end Free;

   procedure Initialise_New_Region
     (Virtual_Address  : Virtual_Address_T;
      Physical_Address : Physical_Address_T;
      Size             : Storage_Offset;
      Result           : out Function_Result) is
   begin
      --  Initialize the new region's header and first block.
      New_Region_Header : Heap_Memory_Region_T
      with Import, Alignment => 1, Address => Virtual_Address;

      New_Region_Header :=
        (Checksum              =>
           Calculate_Region_Header_Checksum
             (Virtual_Address, Physical_Address, Size, null),
         Heap_Region_Virt_Addr => Virtual_Address,
         Heap_Region_Phys_Addr => Physical_Address,
         Heap_Region_Size      => Size,
         Next_Region           => null);

      Heap_Starting_Block_Address : constant Virtual_Address_T :=
        Virtual_Address + Region_Header_Size;

      Heap_Starting_Block : Allocation_Header_T
      with Import, Alignment => 1, Address => Heap_Starting_Block_Address;

      Heap_Starting_Block.Block_Size :=
        Size - Region_Header_Size - Header_Size;

      Heap_Starting_Block.Block_Checksum :=
        Calculate_Header_Checksum
          (Identity_Marker_Free,
           Heap_Starting_Block_Address,
           Heap_Starting_Block.Block_Size);

      Log_Debug
        ("Inserted new heap memory region: "
         & ASCII.LF
         & "  VAddr: "
         & New_Region_Header.Heap_Region_Virt_Addr'Image
         & ASCII.LF
         & "  PAddr: "
         & New_Region_Header.Heap_Region_Phys_Addr'Image
         & ASCII.LF
         & "  Size:  "
         & New_Region_Header.Heap_Region_Size'Image,
         Logging_Tags_Heap);

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error
           ("Constraint_Error: Initialise_New_Region", Logging_Tags_Heap);
         Result := Constraint_Exception;
   end Initialise_New_Region;

   procedure Add_Memory_Region_To_Heap_Unlocked
     (Memory_Heap      : in out Memory_Heap_T;
      Virtual_Address  : Virtual_Address_T;
      Physical_Address : Physical_Address_T;
      Size             : Storage_Offset;
      Result           : out Function_Result)
   is
      Curr_Region : Heap_Memory_Region_Access :=
        Memory_Heap.Memory_Regions_List_Head;
   begin
      if Size < (Header_Size + Region_Header_Size) then
         Log_Error ("Region size is too small.", Logging_Tags_Heap);

         Result := Invalid_Argument;
         return;
      end if;

      if Memory_Heap.Memory_Regions_List_Head = null then
         --  If the list of regions is empty, add the new region as the new
         --  head of the list.
         Memory_Heap.Memory_Regions_List_Head :=
           Convert_Address_To_Heap_Memory_Region_Access (Virtual_Address);
      else
         --  Iterate to the end of the region list, checking for overlaps with
         --  existing regions.
         loop
            if not Validate_Heap_Memory_Region_Pointer
                     (Memory_Heap, Curr_Region)
            then
               Log_Error
                 ("Invalid heap region header checksum: "
                  & Curr_Region.all'Address'Image,
                  Logging_Tags_Heap);

               Result := Region_Not_Mapped;
               return;
            end if;

            if Is_Region_Intersecting
                 (Curr_Region.all, Virtual_Address, Physical_Address, Size)
            then
               Log_Error
                 ("New heap memory region overlaps with existing region.",
                  Logging_Tags_Heap);

               Result := Region_Is_Overlapping;
               return;
            end if;

            exit when Curr_Region.all.Next_Region = null;

            Curr_Region := Curr_Region.all.Next_Region;
         end loop;

         Curr_Region.all.Next_Region :=
           Convert_Address_To_Heap_Memory_Region_Access (Virtual_Address);

         --  Update the current region's checksum to account for the new next
         --  region pointer.
         Curr_Region.all.Checksum :=
           Calculate_Region_Header_Checksum
             (Curr_Region.all.Heap_Region_Virt_Addr,
              Curr_Region.all.Heap_Region_Phys_Addr,
              Curr_Region.all.Heap_Region_Size,
              Curr_Region.all.Next_Region);
      end if;

      Initialise_New_Region (Virtual_Address, Physical_Address, Size, Result);
   exception
      when Constraint_Error =>
         Log_Error
           ("Constraint_Error: Add_Memory_Region_To_Heap", Logging_Tags_Heap);
         Result := Constraint_Exception;
   end Add_Memory_Region_To_Heap_Unlocked;

   procedure Add_Memory_Region_To_Heap
     (Memory_Heap      : in out Memory_Heap_T;
      Virtual_Address  : Virtual_Address_T;
      Physical_Address : Physical_Address_T;
      Size             : Storage_Offset;
      Result           : out Function_Result) is
   begin
      Acquire_Spinlock (Memory_Heap.Spinlock);

      Add_Memory_Region_To_Heap_Unlocked
        (Memory_Heap, Virtual_Address, Physical_Address, Size, Result);

      Release_Spinlock (Memory_Heap.Spinlock);
   end Add_Memory_Region_To_Heap;

   procedure Add_Memory_Region_To_Heap_And_Allocate
     (Memory_Heap       : in out Memory_Heap_T;
      Virtual_Address   : Virtual_Address_T;
      Physical_Address  : Physical_Address_T;
      Region_Size       : Storage_Offset;
      Allocation_Size   : Positive;
      Allocation_Result : out Memory_Allocation_Result;
      Result            : out Function_Result;
      Alignment         : Storage_Offset := 1) is
   begin
      Acquire_Spinlock (Memory_Heap.Spinlock);

      Add_Memory_Region_To_Heap_Unlocked
        (Memory_Heap, Virtual_Address, Physical_Address, Region_Size, Result);
      if Is_Error (Result) then
         Release_Spinlock (Memory_Heap.Spinlock);
         return;
      end if;

      Allocate_Unlocked
        (Memory_Heap, Allocation_Size, Allocation_Result, Result, Alignment);

      Release_Spinlock (Memory_Heap.Spinlock);
   end Add_Memory_Region_To_Heap_And_Allocate;

   procedure Get_Minimum_Region_Size
     (Allocation_Size     : Positive;
      Alignment           : Storage_Offset;
      Minimum_Region_Size : out Storage_Offset;
      Result              : out Function_Result) is
   begin
      Minimum_Region_Size :=
        Storage_Offset (Allocation_Size) + Region_Header_Size + Alignment
        + 3 * Header_Size;
      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error
           ("Constraint_Error: Get_Minimum_Region_Size", Logging_Tags_Heap);
         Result := Constraint_Exception;
   end Get_Minimum_Region_Size;

   function Calculate_Region_Header_Checksum
     (Region_Address          : Virtual_Address_T;
      Region_Physical_Address : Physical_Address_T;
      Region_Size             : Storage_Offset;
      Next_Region             : Heap_Memory_Region_Access) return Unsigned_64
   is
   begin
      return
        Heap_Region_Checksum_Identity
        xor Address_To_Unsigned_64 (Region_Address)
        xor Address_To_Unsigned_64 (Address (Region_Physical_Address))
        xor Unsigned_64 (Region_Size)
        xor
          (if Next_Region = null
           then 0
           else Address_To_Unsigned_64 (Next_Region.all'Address));
   exception
      when Constraint_Error =>
         Log_Error
           ("Constraint_Error: Calculate_Region_Header_Checksum",
            Logging_Tags_Heap);
         return 0;
   end Calculate_Region_Header_Checksum;

end Memory.Allocators.Heap;
