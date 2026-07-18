with Memory.Physical; use Memory.Physical;
with Memory.Virtual;  use Memory.Virtual;
with Hart_State;      use Hart_State;

package body Memory.Kernel is
   --  The offset into the page pool's virtual address window at which the
   --  next growth region will be mapped.
   --  NOTE: This variable is protected by the kernel page pool's spinlock.
   Kernel_Page_Pool_Next_Region_Offset : Storage_Offset := 0;

   --  When the pool's free page count falls below this threshold after an
   --  allocation, the pool is grown pre-emptively, while the physical
   --  allocator is more likely to still hold large contiguous blocks.
   Page_Pool_Low_Watermark : constant := 64;

   procedure Reserve_Kernel_Page_Pool_Virtual_Address_Space_Unlocked
     (New_Region_Size_In_Bytes : Positive;
      Region_Virtual_Address   : out Virtual_Address_T;
      Result                   : out Function_Result) is
   begin
      Region_Virtual_Address :=
        Kernel_Page_Pool_Virtual_Address + Kernel_Page_Pool_Next_Region_Offset;

      Kernel_Page_Pool_Next_Region_Offset :=
        Kernel_Page_Pool_Next_Region_Offset
        + Storage_Offset (New_Region_Size_In_Bytes);

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error
           ("Constraint_Error: "
            & "Reserve_Kernel_Page_Pool_Virtual_Address_Space");
         Result := Constraint_Exception;
   end Reserve_Kernel_Page_Pool_Virtual_Address_Space_Unlocked;

   procedure Reserve_Kernel_Page_Pool_Virtual_Address_Space
     (New_Region_Size_In_Bytes : Positive;
      Region_Virtual_Address   : out Virtual_Address_T;
      Result                   : out Function_Result) is
   begin
      Acquire_Spinlock (Kernel_Page_Pool.Spinlock);
      Reserve_Kernel_Page_Pool_Virtual_Address_Space_Unlocked
        (New_Region_Size_In_Bytes, Region_Virtual_Address, Result);
      Release_Spinlock (Kernel_Page_Pool.Spinlock);
   end Reserve_Kernel_Page_Pool_Virtual_Address_Space;

   procedure Recover_Kernel_Page_Pool_Virtual_Address_Space_Unlocked
     (Region_Virtual_Address         : Virtual_Address_T;
      Recovered_Region_Size_In_Bytes : Positive) is
   begin
      Reserved_Region_Offset : constant Storage_Offset :=
        Kernel_Page_Pool_Next_Region_Offset
        - Storage_Offset (Recovered_Region_Size_In_Bytes);

      --  A reservation can only be rolled back while it's still the most
      --  recent one: If another hart has reserved address space since, the
      --  region being recovered isn't at the top of the window anymore, and
      --  subtracting its size would un-reserve that hart's live region.
      --  In that case the reserved space is deliberately leaked; the pool's
      --  virtual address window is large enough that this is harmless.
      if Reserved_Region_Offset >= 0
        and then
          Kernel_Page_Pool_Virtual_Address + Reserved_Region_Offset
          = Region_Virtual_Address
      then
         Kernel_Page_Pool_Next_Region_Offset := Reserved_Region_Offset;
      end if;
   exception
      when Constraint_Error =>
         --  The reserved space is leaked, which is safe.
         Log_Error
           ("Constraint_Error: "
            & "Recover_Kernel_Page_Pool_Virtual_Address_Space_Unlocked");
   end Recover_Kernel_Page_Pool_Virtual_Address_Space_Unlocked;

   procedure Recover_Kernel_Page_Pool_Virtual_Address_Space
     (Region_Virtual_Address         : Virtual_Address_T;
      Recovered_Region_Size_In_Bytes : Positive) is
   begin
      Acquire_Spinlock (Kernel_Page_Pool.Spinlock);
      Recover_Kernel_Page_Pool_Virtual_Address_Space_Unlocked
        (Region_Virtual_Address, Recovered_Region_Size_In_Bytes);
      Release_Spinlock (Kernel_Page_Pool.Spinlock);
   end Recover_Kernel_Page_Pool_Virtual_Address_Space;

   --  Grows the kernel page pool by acquiring new memory from the physical
   --  memory allocator, mapping it at the next free offset in the
   --  pool's virtual address window, and adding it to the pool.
   --  Each region is a single physically contiguous block: If a full-sized
   --  region cannot be satisfied, progressively smaller region sizes are
   --  attempted, provided they can still satisfy the required page count.
   procedure Grow_Kernel_Page_Pool
     (Minimum_Page_Count : Positive; Result : out Function_Result)
   is
      Growth_Region_Page_Counts :
        constant array (Positive range <>) of Positive :=
          [Max_Page_Pool_Region_Size, 256, 64];

      Allocated_Physical_Address : Physical_Address_T := Null_Physical_Address;
      Region_Virtual_Address     : Virtual_Address_T := Null_Address;

      Region_Size_In_Bytes : Positive := 1;

      Free_Result : Function_Result := Unset;
   begin
      if Minimum_Page_Count > Max_Page_Pool_Region_Size then
         Result := Invalid_Argument;
         return;
      end if;

      --  Query each of the possible allocation sizes, until one is accepted
      --  by the physical memory allocator.
      for Region_Page_Count of Growth_Region_Page_Counts loop
         if Region_Page_Count >= Minimum_Page_Count then
            Region_Size_In_Bytes := Region_Page_Count * Page_Pool_Page_Size;

            --  Allocate the physical memory backing the new region.
            Allocate_Physical_Memory
              (Region_Size_In_Bytes, Allocated_Physical_Address, Result);
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
                  Allocated_Physical_Address,
                  Storage_Offset (Region_Size_In_Bytes),
                  (True, True, False, False),
                  Result);
               if Is_Error (Result) then
                  goto Error_Recover_Virtual_Memory_Space;
               end if;

               --  Zero the newly allocated page pool region.
               Set (Region_Virtual_Address, 0, Region_Size_In_Bytes);

               Kernel_Page_Pool.Add_Region_To_Page_Pool
                 (Region_Virtual_Address,
                  Allocated_Physical_Address,
                  Region_Page_Count,
                  Result);
               if Is_Error (Result) then
                  --  The region is already mapped, so its physical memory
                  --  can't be returned to the physical allocator.
                  --  This only occurs if the pool's region array is exhausted.
                  --  @TODO: Handle this error.
                  return;
               end if;

               Log_Debug
                 ("Grew kernel page pool by"
                  & Region_Page_Count'Image
                  & " pages",
                  Logging_Tags);

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
      Free_Physical_Memory (Allocated_Physical_Address, Free_Result);
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Grow_Kernel_Page_Pool");
         Result := Constraint_Exception;
   end Grow_Kernel_Page_Pool;

   procedure Allocate_Kernel_Memory
     (Size              : Positive;
      Allocated_Address : out Virtual_Address_T;
      Result            : out Function_Result;
      Alignment         : Storage_Offset := 1)
   is
      Allocation_Result : Memory_Allocation_Result;
   begin
      Kernel_Heap.Allocate (Size, Allocation_Result, Result, Alignment);

      Allocated_Address := Allocation_Result.Virtual_Address;
   end Allocate_Kernel_Memory;

   procedure Allocate_Kernel_Physical_Memory
     (Size              : Positive;
      Allocation_Result : out Memory_Allocation_Result;
      Result            : out Function_Result;
      Alignment         : Storage_Offset := 1) is
   begin
      Kernel_Heap.Allocate (Size, Allocation_Result, Result, Alignment);
   end Allocate_Kernel_Physical_Memory;

   procedure Allocate_Pages
     (Number_of_Pages   : Positive;
      Allocation_Result : out Memory_Allocation_Result;
      Result            : out Function_Result)
   is
      Grow_Result : Function_Result := Unset;
   begin
      Kernel_Page_Pool.Allocate (Number_of_Pages, Allocation_Result, Result);

      --  If the allocation can't be fulfilled, attempt to grow the page pool,
      --  then retry the allocation.
      if Result = Not_Enough_Memory_Available then
         Grow_Kernel_Page_Pool (Number_of_Pages, Grow_Result);
         if Is_Error (Grow_Result) then
            return;
         end if;

         Kernel_Page_Pool.Allocate
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

   procedure Initialise_Kernel_Heap is
      Allocated_Physical_Address : Physical_Address_T := Null_Physical_Address;

      Result : Function_Result := Unset;
   begin
      Log_Debug ("Initialising kernel heap...", Logging_Tags);

      Kernel_Heap.Spinlock.Lock_Id := Lock_Id_Kernel_Heap;

      Allocate_Physical_Memory
        (Kernel_Heap_Initial_Size, Allocated_Physical_Address, Result);
      if Is_Error (Result) then
         --  Error already printed.
         Panic;
      end if;

      Log_Debug ("Mapping initial kernel heap region...", Logging_Tags);

      Map_Kernel_Memory
        (Kernel_Heap_Virtual_Address,
         Allocated_Physical_Address,
         Storage_Offset (Kernel_Heap_Initial_Size),
         (True, True, False, False),
         Result);
      if Is_Error (Result) then
         --  Error already printed.
         Panic;
      end if;

      Log_Debug ("Adding initial kernel heap region...", Logging_Tags);

      Kernel_Heap.Add_Memory_Region_To_Heap
        (Kernel_Heap_Virtual_Address,
         Allocated_Physical_Address,
         Storage_Offset (Kernel_Heap_Initial_Size),
         Result);
      if Is_Error (Result) then
         --  Error already printed.
         Panic;
      end if;

      Log_Debug ("Initialised kernel heap.", Logging_Tags);
   end Initialise_Kernel_Heap;

   procedure Initialise_Kernel_Page_Pool is
      --  16MiB kernel page pool initial size.
      Kernel_Page_Pool_Initial_Size : constant Positive := 16#100_0000#;

      Kernel_Page_Pool_Region_Count : Natural := 0;
      Page_Pool_Mapping_Offset      : Storage_Offset := 0;

      Allocated_Physical_Address : Physical_Address_T := Null_Physical_Address;

      Result : Function_Result := Unset;
   begin
      Log_Debug ("Initialising kernel page pool...", Logging_Tags);

      Allocate_Physical_Memory
        (Kernel_Page_Pool_Initial_Size, Allocated_Physical_Address, Result);
      if Is_Error (Result) then
         --  Error already printed.
         Panic;
      end if;

      Kernel_Page_Pool_Region_Count :=
        Natural
          (Storage_Offset (Kernel_Page_Pool_Initial_Size)
           / Page_Pool_Region_Size_In_Bytes);

      for I in 1 .. Kernel_Page_Pool_Region_Count loop
         Log_Debug ("Adding region to kernel page pool...", Logging_Tags);
         Page_Pool_Mapping_Offset :=
           Storage_Offset (I - 1) * Page_Pool_Region_Size_In_Bytes;

         Kernel_Page_Pool.Add_Region_To_Page_Pool
           (Kernel_Page_Pool_Virtual_Address + Page_Pool_Mapping_Offset,
            Allocated_Physical_Address + Page_Pool_Mapping_Offset,
            Max_Page_Pool_Region_Size,
            Result);
         if Is_Error (Result) then
            --  Error already printed.
            Panic;
         end if;
      end loop;

      Log_Debug ("Mapping kernel page pool regions...", Logging_Tags);

      --  Map kernel page pool.
      for I in 1 .. Kernel_Page_Pool_Region_Count loop
         Log_Debug
           ("Mapping page pool region:"
            & Kernel_Page_Pool.Page_Pool_Regions (I).Virtual_Address'Image,
            Logging_Tags);

         Map_Kernel_Memory
           (Kernel_Page_Pool.Page_Pool_Regions (I).Virtual_Address,
            Kernel_Page_Pool.Page_Pool_Regions (I).Physical_Address,
            Page_Pool_Region_Size_In_Bytes,
            (True, True, False, False),
            Result);
         if Is_Error (Result) then
            --  Error already printed.
            Panic;
         end if;

         --  Zero the newly allocated page pool region.
         Set
           (Kernel_Page_Pool.Page_Pool_Regions (I).Virtual_Address,
            0,
            Page_Pool_Region_Size_In_Bytes);
      end loop;

      --  Any regions the pool is grown by at runtime are mapped into the
      --  pool's virtual address window after the initial regions.
      Kernel_Page_Pool_Next_Region_Offset :=
        Storage_Offset (Kernel_Page_Pool_Initial_Size);

      Log_Debug ("Initialised kernel page pool.", Logging_Tags);
   exception
      when Constraint_Error =>
         Panic ("Constraint_Error: Initialise_Kernel_Page_Pool");
   end Initialise_Kernel_Page_Pool;

end Memory.Kernel;
