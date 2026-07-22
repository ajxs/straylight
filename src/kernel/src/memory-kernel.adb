with Memory.Physical; use Memory.Physical;
with Memory.Virtual;  use Memory.Virtual;

package body Memory.Kernel is
   --  The offset into the page pool's virtual address window at which the
   --  next growth region will be mapped.
   --  NOTE: This variable is protected by the kernel page pool's spinlock.
   Kernel_Page_Pool_Next_Region_Offset : Storage_Offset := 0;

   --  The offset into the kernel heap's virtual address window at which the
   --  next growth region will be mapped.
   --  NOTE: This variable is protected by the kernel heap's spinlock.
   Kernel_Heap_Next_Region_Offset : Storage_Offset := 0;

   procedure Reserve_Virtual_Memory_Space_Unlocked
     (New_Region_Size_In_Bytes : Positive;
      Start_Address            : Virtual_Address_T;
      Next_Region_Offset       : in out Storage_Offset;
      Region_Virtual_Address   : out Virtual_Address_T;
      Result                   : out Function_Result) is
   begin
      Region_Virtual_Address := Start_Address + Next_Region_Offset;

      Next_Region_Offset :=
        Next_Region_Offset + Storage_Offset (New_Region_Size_In_Bytes);

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error
           ("Constraint_Error: " & "Reserve_Virtual_Memory_Space_Unlocked");
         Result := Constraint_Exception;
   end Reserve_Virtual_Memory_Space_Unlocked;

   procedure Reserve_Kernel_Page_Pool_Virtual_Address_Space
     (New_Region_Size_In_Bytes : Positive;
      Region_Virtual_Address   : out Virtual_Address_T;
      Result                   : out Function_Result) is
   begin
      Acquire_Spinlock (Kernel_Page_Pool.Spinlock);
      Reserve_Virtual_Memory_Space_Unlocked
        (New_Region_Size_In_Bytes,
         Kernel_Page_Pool_Virtual_Address,
         Kernel_Page_Pool_Next_Region_Offset,
         Region_Virtual_Address,
         Result);
      Release_Spinlock (Kernel_Page_Pool.Spinlock);
   end Reserve_Kernel_Page_Pool_Virtual_Address_Space;

   procedure Recover_Virtual_Memory_Space_Unlocked
     (Region_Virtual_Address         : Virtual_Address_T;
      Recovered_Region_Size_In_Bytes : Positive;
      Start_Address                  : Virtual_Address_T;
      Next_Region_Offset             : in out Storage_Offset) is
   begin
      Reserved_Region_Offset : constant Storage_Offset :=
        Next_Region_Offset - Storage_Offset (Recovered_Region_Size_In_Bytes);

      --  A reservation can only be rolled back while it's still the most
      --  recent one: If another hart has reserved address space since, the
      --  region being recovered isn't at the top of the window anymore, and
      --  subtracting its size would un-reserve that hart's live region.
      --  In that case the reserved space is deliberately leaked; the pool's
      --  virtual address window is large enough that this is harmless.
      if Reserved_Region_Offset >= 0
        and then
          Start_Address + Reserved_Region_Offset = Region_Virtual_Address
      then
         Next_Region_Offset := Reserved_Region_Offset;
      end if;
   exception
      when Constraint_Error =>
         --  The reserved space is leaked, which is safe.
         Log_Error ("Constraint_Error: Recover_Virtual_Memory_Space_Unlocked");
   end Recover_Virtual_Memory_Space_Unlocked;

   procedure Recover_Kernel_Page_Pool_Virtual_Address_Space
     (Region_Virtual_Address         : Virtual_Address_T;
      Recovered_Region_Size_In_Bytes : Positive) is
   begin
      Acquire_Spinlock (Kernel_Page_Pool.Spinlock);
      Recover_Virtual_Memory_Space_Unlocked
        (Region_Virtual_Address,
         Recovered_Region_Size_In_Bytes,
         Kernel_Page_Pool_Virtual_Address,
         Kernel_Page_Pool_Next_Region_Offset);
      Release_Spinlock (Kernel_Page_Pool.Spinlock);
   end Recover_Kernel_Page_Pool_Virtual_Address_Space;

   --  Allocates and maps the physical memory backing a new page pool region,
   --  without adding it to the pool.
   --  Each region is a single physically contiguous block: If a full-sized
   --  region cannot be satisfied, progressively smaller region sizes are
   --  attempted, provided they can still satisfy the required page count.
   procedure Provision_New_Page_Pool_Region
     (Minimum_Page_Count      : Positive;
      Region_Virtual_Address  : out Virtual_Address_T;
      Region_Physical_Address : out Physical_Address_T;
      Region_Page_Count       : out Positive;
      Result                  : out Function_Result)
   is
      Growth_Region_Page_Counts :
        constant array (Positive range <>) of Positive :=
          [Max_Page_Pool_Region_Size, 256, 64];

      Region_Size_In_Bytes : Positive := 1;

      Free_Result : Function_Result := Unset;
   begin
      Region_Virtual_Address := Null_Address;
      Region_Physical_Address := Null_Physical_Address;
      Region_Page_Count := 1;

      if Minimum_Page_Count > Max_Page_Pool_Region_Size then
         Result := Invalid_Argument;
         return;
      end if;

      --  Query each of the possible allocation sizes, until one is accepted
      --  by the physical memory allocator.
      for Candidate_Page_Count of Growth_Region_Page_Counts loop
         if Candidate_Page_Count >= Minimum_Page_Count then
            Region_Size_In_Bytes := Candidate_Page_Count * Page_Pool_Page_Size;

            --  Allocate the physical memory backing the new region.
            Allocate_Physical_Memory
              (Region_Size_In_Bytes, Region_Physical_Address, Result);
            if Result = Success then
               --  Reserve the virtual address space for the new region.
               Reserve_Kernel_Page_Pool_Virtual_Address_Space
                 (Region_Size_In_Bytes, Region_Virtual_Address, Result);
               if Is_Error (Result) then
                  goto Error_Free_Physical_Memory;
               end if;

               --  Map the new region into the pool's virtual address window.
               Map_Kernel_Memory
                 (Region_Virtual_Address,
                  Region_Physical_Address,
                  Storage_Offset (Region_Size_In_Bytes),
                  (True, True, False, False),
                  Result);
               if Is_Error (Result) then
                  goto Error_Recover_Virtual_Memory_Space;
               end if;

               --  Zero the newly allocated page pool region.
               Set (Region_Virtual_Address, 0, Region_Size_In_Bytes);

               Region_Page_Count := Candidate_Page_Count;

               Log_Debug
                 ("Provisioned new page pool region of"
                  & Candidate_Page_Count'Image
                  & " pages",
                  Logging_Tags);

               Result := Success;
               return;
            elsif Result = No_Block_Large_Enough then
               --  The physical memory allocator couldn't satisfy the request.
               --  Try the next smaller region size.
               null;
            else
               --  An unexpected error occurred.
               return;
            end if;
         end if;
      end loop;

      --  All candidate region sizes were rejected by the physical memory
      --  allocator. Result holds the error from the last attempt.
      return;

      --  Result retains the error which caused the growth attempt to fail
      --  throughout the error handling below.
      <<Error_Recover_Virtual_Memory_Space>>
      Recover_Kernel_Page_Pool_Virtual_Address_Space
        (Region_Virtual_Address, Region_Size_In_Bytes);

      <<Error_Free_Physical_Memory>>
      Free_Physical_Memory (Region_Physical_Address, Free_Result);
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Provision_New_Page_Pool_Region");
         Result := Constraint_Exception;
   end Provision_New_Page_Pool_Region;

   procedure Grow_Kernel_Page_Pool
     (Minimum_Page_Count : Positive; Result : out Function_Result)
   is
      Region_Virtual_Address  : Virtual_Address_T;
      Region_Physical_Address : Physical_Address_T;
      Region_Page_Count       : Positive;
   begin
      Provision_New_Page_Pool_Region
        (Minimum_Page_Count,
         Region_Virtual_Address,
         Region_Physical_Address,
         Region_Page_Count,
         Result);
      if Is_Error (Result) then
         return;
      end if;

      Kernel_Page_Pool.Add_Region_To_Page_Pool
        (Region_Virtual_Address,
         Region_Physical_Address,
         Region_Page_Count,
         Result);
      --  If this fails, the region is already mapped, so its physical memory
      --  can't be returned to the physical allocator.
      --  This only occurs if the pool's region array is exhausted.
      --  @TODO: Handle this error.
   end Grow_Kernel_Page_Pool;

   --  Grow the kernel page pool by provisioning a new region, adding it to the
   --  pool, and creating a new allocation from it, atomically under the page
   --  pool's spinlock.
   procedure Grow_Kernel_Page_Pool_And_Allocate
     (Allocation_Page_Count : Positive;
      Allocation_Result     : out Memory_Allocation_Result;
      Result                : out Function_Result)
   is
      Region_Virtual_Address  : Virtual_Address_T;
      Region_Physical_Address : Physical_Address_T;
      Region_Page_Count       : Positive;
   begin
      Provision_New_Page_Pool_Region
        (Allocation_Page_Count,
         Region_Virtual_Address,
         Region_Physical_Address,
         Region_Page_Count,
         Result);
      if Is_Error (Result) then
         return;
      end if;

      Kernel_Page_Pool.Add_Region_To_Page_Pool_And_Allocate
        (Region_Virtual_Address,
         Region_Physical_Address,
         Region_Page_Count,
         Allocation_Page_Count,
         Allocation_Result,
         Result);
      --  If this fails, the region is already mapped, so its physical memory
      --  can't be returned to the page pool.
      --  This only occurs if the pool's region array is exhausted.
      --  @TODO: Handle this error.
   end Grow_Kernel_Page_Pool_And_Allocate;

   procedure Reserve_Kernel_Heap_Virtual_Address_Space
     (New_Region_Size_In_Bytes : Positive;
      Region_Virtual_Address   : out Virtual_Address_T;
      Result                   : out Function_Result) is
   begin
      Acquire_Spinlock (Kernel_Heap.Spinlock);
      Reserve_Virtual_Memory_Space_Unlocked
        (New_Region_Size_In_Bytes,
         Kernel_Heap_Virtual_Address,
         Kernel_Heap_Next_Region_Offset,
         Region_Virtual_Address,
         Result);
      Release_Spinlock (Kernel_Heap.Spinlock);
   end Reserve_Kernel_Heap_Virtual_Address_Space;

   procedure Recover_Kernel_Heap_Virtual_Address_Space
     (Region_Virtual_Address         : Virtual_Address_T;
      Recovered_Region_Size_In_Bytes : Positive) is
   begin
      Acquire_Spinlock (Kernel_Heap.Spinlock);
      Recover_Virtual_Memory_Space_Unlocked
        (Region_Virtual_Address,
         Recovered_Region_Size_In_Bytes,
         Kernel_Heap_Virtual_Address,
         Kernel_Heap_Next_Region_Offset);
      Release_Spinlock (Kernel_Heap.Spinlock);
   end Recover_Kernel_Heap_Virtual_Address_Space;

   --  Grows the kernel heap by acquiring new memory from the kernel page
   --  pool, mapping it at the next free offset in the heap's virtual
   --  address window, and adding it to the heap as a new region.
   --  Each region is backed by a single run of pages from the kernel page
   --  pool, and is therefore a single physically contiguous block: If a
   --  full-sized region cannot be satisfied, progressively smaller region
   --  sizes are attempted, provided they can still satisfy the allocation
   --  which triggered the growth.
   procedure Grow_Kernel_Heap_And_Allocate
     (Allocation_Size   : Positive;
      Allocation_Result : out Memory_Allocation_Result;
      Result            : out Function_Result;
      Alignment         : Storage_Offset := 1)
   is
      Growth_Region_Page_Counts :
        constant array (Positive range <>) of Positive :=
          [Max_Page_Pool_Region_Size, 256, 64];

      Pages_Allocation_Result : Memory_Allocation_Result;
      Region_Virtual_Address  : Virtual_Address_T := Null_Address;

      Region_Size_In_Bytes         : Positive := 1;
      Minimum_Region_Size_In_Bytes : Storage_Offset := 1;

      Free_Result : Function_Result := Unset;
   begin
      Get_Minimum_Region_Size
        (Allocation_Size, Alignment, Minimum_Region_Size_In_Bytes, Result);
      if Is_Error (Result) then
         return;
      end if;

      if Minimum_Region_Size_In_Bytes > Page_Pool_Region_Size_In_Bytes then
         Result := Invalid_Argument;
         return;
      end if;

      --  Query each of the possible region sizes, until one is accepted
      --  by the kernel page pool.
      for Region_Page_Count of Growth_Region_Page_Counts loop
         if Storage_Offset (Region_Page_Count * Kernel_Page_Pool_Page_Size)
           >= Minimum_Region_Size_In_Bytes
         then
            Region_Size_In_Bytes :=
              Region_Page_Count * Kernel_Page_Pool_Page_Size;

            --  Allocate the physical memory backing the new region from the
            --  kernel page pool.
            Allocate_Pages
              (Region_Page_Count, Pages_Allocation_Result, Result);
            if Result = Success then
               --  Reserve the virtual address space for the new region.
               Reserve_Kernel_Heap_Virtual_Address_Space
                 (Region_Size_In_Bytes, Region_Virtual_Address, Result);
               if Is_Error (Result) then
                  goto Error_Free_Pages;
               end if;

               --  Map the new region into the heap's virtual address window.
               Map_Kernel_Memory
                 (Region_Virtual_Address,
                  Pages_Allocation_Result.Physical_Address,
                  Storage_Offset (Region_Size_In_Bytes),
                  (True, True, False, False),
                  Result);
               if Is_Error (Result) then
                  goto Error_Recover_Virtual_Memory_Space;
               end if;

               --  @TODO: In the future, check whether the newly allocated
               --  pages are physically and virtually contiguous with the last
               --  allocated region. If so, the region can be 'extended'.
               Add_Memory_Region_To_Heap_And_Allocate
                 (Kernel_Heap,
                  Region_Virtual_Address,
                  Pages_Allocation_Result.Physical_Address,
                  Storage_Offset (Region_Size_In_Bytes),
                  Allocation_Size,
                  Allocation_Result,
                  Result,
                  Alignment);

               if Is_Error (Result) then
                  --  The region is already mapped, so its physical memory
                  --  can't be returned to the page pool.
                  --  This only occurs if the heap's region array is exhausted.
                  --  @TODO: Handle this error.
                  return;
               end if;

               Log_Debug
                 ("Grew kernel heap by"
                  & Region_Size_In_Bytes'Image
                  & " bytes",
                  Logging_Tags);

               return;
            elsif Result = Not_Enough_Memory_Available then
               --  The kernel page pool couldn't satisfy the request.
               --  Try the next smaller region size.
               null;
            else
               --  An unexpected error occurred.
               return;
            end if;
         end if;
      end loop;

      --  All candidate region sizes were rejected by the kernel page pool.
      --  Result holds the error from the last attempt.
      return;

      <<Error_Recover_Virtual_Memory_Space>>
      --  Result retains the error which caused the growth attempt to fail
      --  throughout the error handling below.
      Recover_Kernel_Heap_Virtual_Address_Space
        (Region_Virtual_Address, Region_Size_In_Bytes);

      <<Error_Free_Pages>>
      Free_Pages (Allocation_Result.Virtual_Address, Free_Result);
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Grow_Kernel_Heap_And_Allocate");
         Result := Constraint_Exception;
   end Grow_Kernel_Heap_And_Allocate;

   procedure Allocate_Kernel_Memory
     (Size              : Positive;
      Allocated_Address : out Virtual_Address_T;
      Result            : out Function_Result;
      Alignment         : Storage_Offset := 1)
   is
      Allocation_Result : Memory_Allocation_Result;
   begin
      Allocate_Kernel_Physical_Memory
        (Size, Allocation_Result, Result, Alignment);
      if Is_Error (Result) then
         return;
      end if;

      Allocated_Address := Allocation_Result.Virtual_Address;
   end Allocate_Kernel_Memory;

   procedure Allocate_Kernel_Physical_Memory
     (Size              : Positive;
      Allocation_Result : out Memory_Allocation_Result;
      Result            : out Function_Result;
      Alignment         : Storage_Offset := 1) is
   begin
      Kernel_Heap.Allocate (Size, Allocation_Result, Result, Alignment);

      --  If the allocation can't be fulfilled, attempt to grow the heap,
      --  then retry the allocation.
      if Result = Not_Enough_Memory_Available then
         Grow_Kernel_Heap_And_Allocate
           (Size, Allocation_Result, Result, Alignment);
      end if;
   end Allocate_Kernel_Physical_Memory;

   procedure Allocate_Pages
     (Number_of_Pages   : Positive;
      Allocation_Result : out Memory_Allocation_Result;
      Result            : out Function_Result)
   is
      --  When the pool's free page count falls below this threshold after an
      --  allocation, the pool is grown pre-emptively, while the physical
      --  allocator is more likely to still hold large contiguous blocks.
      Page_Pool_Low_Watermark : constant := 64;

      Grow_Result : Function_Result := Unset;
   begin
      Kernel_Page_Pool.Allocate (Number_of_Pages, Allocation_Result, Result);

      --  If the allocation can't be fulfilled, attempt to grow the page pool,
      --  which will itself satisfy the allocation from the new region.
      if Result = Not_Enough_Memory_Available then
         Grow_Kernel_Page_Pool_And_Allocate
           (Number_of_Pages, Allocation_Result, Result);
      end if;

      if Is_Error (Result) then
         return;
      end if;

      --  Grow the pool pre-emptively when the number of free pages is low.
      --  A failure here is not a fatal error: the current allocation has
      --  already been satisfied.
      if Kernel_Page_Pool.Free_Page_Count < Page_Pool_Low_Watermark then
         Grow_Kernel_Page_Pool (1, Grow_Result);
         if Is_Error (Grow_Result) then
            Log_Error
              ("Failed to grow kernel page pool pre-emptively", Logging_Tags);
         end if;
      end if;
   end Allocate_Pages;

   procedure Free_Kernel_Memory
     (Allocated_Virtual_Address : Virtual_Address_T;
      Result                    : out Function_Result) is
   begin
      Kernel_Heap.Free (Allocated_Virtual_Address, Result);
   end Free_Kernel_Memory;

   procedure Free_Pages
     (Virtual_Address : Virtual_Address_T; Result : out Function_Result) is
   begin
      Kernel_Page_Pool.Free (Virtual_Address, Result);
   end Free_Pages;

end Memory.Kernel;
