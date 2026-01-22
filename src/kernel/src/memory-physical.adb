-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Memory.Virtual;

package body Memory.Physical is
   procedure Allocate_Physical_Memory
     (Required_Size     : Positive;
      Allocated_Address : out Physical_Address_T;
      Result            : out Function_Result) is
   begin
      Acquire_Spinlock (Phys_Memory_Space.Spinlock);

      Allocate_Physical_Memory_Unlocked
        (Required_Size, Allocated_Address, Result);

      Release_Spinlock (Phys_Memory_Space.Spinlock);

      Log_Debug
        ("Initialising allocated physical memory block.", Logging_Tags);

      --  @NOTE: Only the specified required size will be initialised, not the
      --  whole underlying physical memory block. Normally this shouldn't be
      --  a problem due to the alignment of the memory blocks. It's important
      --  that no more physical memory is mapped into a process' virtual
      --  address space than is allocated here, as it would expose
      --  uninitialised memory to the process.
      Initialise_Allocated_Physical_Memory : declare
         Mapped_Address : Virtual_Address_T := Null_Address;
      begin
         Mapped_Address :=
           Memory.Virtual.Get_Physical_Address_Virtual_Mapping
             (Allocated_Address);

         Memory.Set (Mapped_Address, 0, Required_Size);
      end Initialise_Allocated_Physical_Memory;
   end Allocate_Physical_Memory;

   procedure Allocate_Physical_Memory_Unlocked
     (Required_Size     : Positive;
      Allocated_Address : out Physical_Address_T;
      Result            : out Function_Result)
   is
      --  The smallest possible block order which contains the required
      --  amount of physical memory.
      Smallest_Possible_Order : Block_Order := 0;
      Current_Block           : Block_Access := null;
      --  As all blocks are iterated through, this block points to the
      --  current 'best fit' block. If a block matching the exact required
      --  size cannot be found, this block will be split until it is.
      Best_Fit_Block          : Block_Access := null;
      Allocated_Block         : Block_Access := null;
   begin
      Log_Debug
        ("Allocating physical memory block with size " & Required_Size'Image,
         Logging_Tags);

      --  If the list is empty, we know there are no free blocks.
      if Is_List_Empty then
         Log_Error ("No free physical memory", Logging_Tags);

         Allocated_Address := Null_Physical_Address;
         Result := Memory_Map_Not_Present;
         return;
      end if;

      Get_Smallest_Possible_Block_Order
        (Required_Size, Smallest_Possible_Order, Result);
      if Is_Error (Result) then
         return;
      end if;

      Log_Debug
        ("Smallest possible block order: " & Smallest_Possible_Order'Image,
         Logging_Tags);

      Current_Block := Phys_Memory_Space.Physical_Memory_Map_List_Head;

      --  Iterate through the linked list of physical memory blocks.
      while Current_Block /= null loop
         if Current_Block.all.Free then
            --  If we've found a block matching the required size.
            if Current_Block.all.Order = Smallest_Possible_Order then
               Allocated_Address := Current_Block.all.Address;
               Current_Block.all.Free := False;

               Log_Debug ("Allocated perfect fit block.", Logging_Tags);

               Allocated_Block := Current_Block;
               goto Post_Successful_Allocation;
            elsif Current_Block.all.Order > Smallest_Possible_Order then
               --  If we have a current ideal block, check whether the current
               --  accessed block is a better fit.
               if Best_Fit_Block /= null then
                  if Current_Block.all.Order < Best_Fit_Block.all.Order then
                     Best_Fit_Block := Current_Block;
                  end if;
               else
                  Best_Fit_Block := Current_Block;
               end if;
            end if;
         end if;

         Current_Block := Current_Block.all.Next_Block;
      end loop;

      --  If there's a block suitable for splitting, begin the process
      --  of splitting this block until it's the ideal size.
      if Best_Fit_Block /= null then
         Log_Debug
           ("Dividing best fit block with order: "
            & Best_Fit_Block.all.Order'Image,
            Logging_Tags);

         Divide_Block_To_Specified_Order
           (Best_Fit_Block.all, Smallest_Possible_Order, Result);
         if Is_Error (Result) then
            return;
         end if;

         Allocated_Address := Best_Fit_Block.all.Address;
         Best_Fit_Block.all.Free := False;

         Log_Debug
           ("Divided best fit block to order: "
            & Best_Fit_Block.all.Order'Image,
            Logging_Tags);

         Allocated_Block := Best_Fit_Block;
         goto Post_Successful_Allocation;
      end if;

      Log_Error ("No block large enough", Logging_Tags);
      Allocated_Address := Null_Physical_Address;
      Result := No_Block_Large_Enough;
      return;

      <<Post_Successful_Allocation>>
      Log_Debug
        ("Allocated physical memory block"
         & ASCII.LF
         & "  Address: "
         & Allocated_Block.all.Address'Image
         & ASCII.LF
         & "  Order:   "
         & Allocated_Block.all.Order'Image
         & ASCII.LF
         & "  Size:    "
         & Get_Block_Size_In_Bytes (Allocated_Block.all.Order)'Image,
         Logging_Tags);

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Memory.Physical.Allocate");
         Allocated_Address := Null_Physical_Address;
         Result := Constraint_Exception;
   end Allocate_Physical_Memory_Unlocked;

   procedure Consolidate_Adjacent_Memory_Blocks
     (Block : in out Physical_Memory_Block_T; Result : out Function_Result)
   is
      Block_End_Address : Physical_Address_T := Null_Physical_Address;
   begin
      if Block.Entry_Used = False or else Block.Next_Block = null then
         Result := Memory_Block_Cannot_Be_Consolidated;
         return;
      end if;

      --  If this block is already at the maximum size, it cannot be
      --  consolidated with another.
      if Block.Order = Maximum_Block_Order then
         Result := Memory_Block_Cannot_Be_Consolidated;
         return;
      end if;

      --  Check that both adjacent blocks are free.
      if not Block.Next_Block.all.Free or else not Block.Free then
         Result := Memory_Block_Cannot_Be_Consolidated;
         return;
      end if;

      --  For a block to be consolidated, we need it to match the order
      --  of the next block.
      if Block.Next_Block.all.Order /= Block.Order then
         Result := Memory_Block_Cannot_Be_Consolidated;
         return;
      end if;

      Block_End_Address :=
        Block.Address + Storage_Offset (Get_Block_Size_In_Bytes (Block.Order));
      --  Test that the two blocks are adjacent in memory.
      if Block_End_Address /= Block.Next_Block.all.Address then
         Result := Memory_Block_Cannot_Be_Consolidated;
         return;
      end if;

      Block.Next_Block.all.Entry_Used := False;

      Block.Order := Block.Order + 1;

      --  Set this block's next pointer to that of the next block.
      Block.Next_Block := Block.Next_Block.all.Next_Block;

      Result := Success;
   exception
      when Constraint_Error =>
         Result := Constraint_Exception;
   end Consolidate_Adjacent_Memory_Blocks;

   procedure Consolidate_Free_Physical_Memory_Blocks
     (Result : out Function_Result)
   is
      Current_Block  : Block_Access := null;
      Previous_Block : Block_Access := null;
   begin
      Current_Block := Phys_Memory_Space.Physical_Memory_Map_List_Head;

      --  Iterate through the linked list of physical memory blocks,
      --  consolidating each block with the next until the list is exhausted.
      while Current_Block /= null loop
         Consolidate_Loop : loop
            Consolidate_Adjacent_Memory_Blocks (Current_Block.all, Result);
            if Is_Error (Result) then
               return;
            end if;

            exit Consolidate_Loop when
              Result = Memory_Block_Cannot_Be_Consolidated;

            --  If the block has been successfully consolidated, 'rewind'
            --  one position in the list by setting the current block to
            --  the previous, to see if the list can be further consolidated
            --  from this block.
            Current_Block := Previous_Block;
         end loop Consolidate_Loop;

         Previous_Block := Current_Block;
         Current_Block := Current_Block.all.Next_Block;
      end loop;

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error
           ("Constraint_Error: Consolidate_Free_Physical_Memory_Blocks");
         Result := Constraint_Exception;
   end Consolidate_Free_Physical_Memory_Blocks;

   procedure Create_Free_Physical_Memory_Region
     (Region_Start  : Physical_Address_T;
      Region_Length : Positive;
      Result        : out Function_Result) is
   begin
      Acquire_Spinlock (Phys_Memory_Space.Spinlock);

      Create_Free_Region_Unlocked (Region_Start, Region_Length, Result);

      Release_Spinlock (Phys_Memory_Space.Spinlock);
   end Create_Free_Physical_Memory_Region;

   procedure Create_Free_Region_Unlocked
     (Region_Start  : Physical_Address_T;
      Region_Length : Positive;
      Result        : out Function_Result)
   is
      Current_Block : Block_Access := null;
      Tail_Block    : Block_Access := null;

      Current_Order      : Natural := Maximum_Block_Order;
      Current_Block_Addr : Natural := 0;
   begin
      --  If the region length is not evenly divisible by the base block
      --  size, then it will end up with wasted space that cannot be
      --  allocated.
      if Region_Length mod Base_Block_Size /= 0 then
         Log_Error ("Invalid region size", Logging_Tags);
         Result := Invalid_Physical_Memory_Size;
         return;
      end if;

      if not Is_List_Empty then
         --  Iterate through the linked list of physical memory blocks
         --  to check for any intersection.
         Current_Block := Phys_Memory_Space.Physical_Memory_Map_List_Head;

         while Current_Block /= null loop
            if Is_Region_Intersecting
                 (Region_Start, Region_Length, Current_Block.all)
            then
               Log_Error ("Region overlaps existing region", Logging_Tags);
               Result := Region_Is_Overlapping;
               return;
            end if;

            Current_Block := Current_Block.all.Next_Block;
         end loop;
      end if;

      Log_Debug
        ("Creating free physical memory region:"
         & ASCII.LF
         & "  Address: "
         & Region_Start'Image
         & ASCII.LF
         & "  Size:    "
         & Region_Length'Image,
         Logging_Tags);

      --  Find the tail of the list, to attach new blocks to.
      Tail_Block := Get_List_Tail;

      Allocate_Blocks : loop
         --  If we've evenly arrived at the end of the memory region, exit.
         exit Allocate_Blocks when Current_Block_Addr = Region_Length;

         --  If the memory block array becomes exhausted, this will return
         --  a status of 'Memory_Map_Array_Exhausted'.
         Current_Block := Find_Free_Entry;
         if Current_Block = null then
            Log_Error ("Memory map array exhausted", Logging_Tags);
            Result := Memory_Map_Array_Exhausted;
            return;
         end if;

         --  Calculate the highest possible block order that fits into
         --  the available memory space.
         Get_Highest_Possible_Block_Order
           (Region_Length - Current_Block_Addr, Current_Order, Result);
         if Is_Error (Result) then
            return;
         end if;

         Current_Block.all :=
           (Address    => Region_Start + Storage_Offset (Current_Block_Addr),
            Order      => Current_Order,
            Free       => True,
            Next_Block => null,
            Entry_Used => True);

         Log_Debug
           ("Initialising block with order " & Current_Order'Image,
            Logging_Tags);

         if Phys_Memory_Space.Physical_Memory_Map_List_Head = null then
            Phys_Memory_Space.Physical_Memory_Map_List_Head := Current_Block;
         else
            Tail_Block.all.Next_Block := Current_Block;
         end if;

         Tail_Block := Current_Block;

         Current_Block_Addr :=
           Current_Block_Addr + Get_Block_Size_In_Bytes (Current_Order);

      end loop Allocate_Blocks;

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Create_Free_Physical_Memory_Region");
         Result := Constraint_Exception;
   end Create_Free_Region_Unlocked;

   procedure Divide_Block_To_Specified_Order
     (Block          : in out Physical_Memory_Block_T;
      Required_Order : Block_Order;
      Result         : out Function_Result) is
   begin
      --  If the new required size is smaller than the current size of
      --  this allocated block, then divide it until it fits.
      loop
         exit when Block.Order = Required_Order;

         --  Divide this memory block into two equally sized blocks
         --  of an order lower than this block.
         Divide_Physical_Memory_Block (Block, Result);
         if Is_Error (Result) then
            return;
         end if;
      end loop;

      --  The function result will have been set by the last divide call.
   end Divide_Block_To_Specified_Order;

   procedure Divide_Physical_Memory_Block
     (Block : in out Physical_Memory_Block_T; Result : out Function_Result)
   is
      Next_Block_Address : Address := Null_Address;
   begin
      if Block.Entry_Used = False
        or else Block.Free = False
        or else Block.Order = 0
      then
         Log_Error ("Block cannot be divided", Logging_Tags);
         Result := Invalid_Argument;
         return;
      end if;

      Log_Debug
        ("Dividing block: "
         & ASCII.LF
         & "  Address: "
         & Block.Address'Image
         & "  Order "
         & Block.Order'Image,
         Logging_Tags);

      Block.Order := Block.Order - 1;

      --  The divided block's memory address is the same size as the
      --  newly divided original block. This size is added to the
      --  previous block's address.
      Next_Block_Address :=
        Address (Block.Address)
        + Storage_Offset (Get_Block_Size_In_Bytes (Block.Order));

      --  Iterate through all available physical memory blocks in the
      --  reserved array until an unused entry is found. If it is,
      --  this then becomes the new 'divided' block in the linked list.
      for I in Phys_Memory_Space.Physical_Memory_Blocks'Range loop
         if Phys_Memory_Space.Physical_Memory_Blocks (I).Entry_Used = False
         then
            --  Initialise new split block.
            Phys_Memory_Space.Physical_Memory_Blocks (I) :=
              (Address    => Physical_Address_T (Next_Block_Address),
               Order      => Block.Order,
               Free       => True,
               Next_Block => Block.Next_Block,
               Entry_Used => True);

            --  The first divided block's 'next' link now points to the
            --  new block.
            Block.Next_Block :=
              Phys_Memory_Space.Physical_Memory_Blocks (I)'Unchecked_Access;

            Result := Success;
            return;
         end if;
      end loop;

      --  If we've fallen off the end of the loop it means that we've
      --- exhausted all entries in the memory map array.
      Log_Error ("Memory map array exhausted", Logging_Tags);
      Result := Memory_Map_Array_Exhausted;
   exception
      when Constraint_Error =>
         Log_Error
           ("Constraint_Error: Divide_Physical_Memory_Block", Logging_Tags);
         Result := Constraint_Exception;
   end Divide_Physical_Memory_Block;

   function Find_Block_With_Address
     (Addr : Physical_Address_T) return Block_Access is
   begin
      Current_Block : Block_Access :=
        Phys_Memory_Space.Physical_Memory_Map_List_Head;

      --  Iterate through the linked list of physical memory blocks
      --  until we find one matching the specified address.
      while Current_Block /= null loop
         if Current_Block.all.Address = Addr then
            return Current_Block;
         end if;

         Current_Block := Current_Block.all.Next_Block;
      end loop;

      --  If we reach the end of the list without finding a block with the
      --  specified address, return null.
      return null;
   end Find_Block_With_Address;

   function Find_Free_Entry return Block_Access is
   begin
      --  Iterate through the physical memory block entry array until
      --  we find a free entry.
      for Test_Block of Phys_Memory_Space.Physical_Memory_Blocks loop
         if Test_Block.Entry_Used = False then
            return Test_Block'Unchecked_Access;
         end if;
      end loop;

      --  If we've reached the end of the loop without returning,
      --  it means that we've run out of free entries in the array.
      return null;
   end Find_Free_Entry;

   procedure Free_Physical_Memory
     (Addr : Physical_Address_T; Result : out Function_Result) is
   begin
      Acquire_Spinlock (Phys_Memory_Space.Spinlock);

      Free_Physical_Memory_Unlocked (Addr, Result);

      Release_Spinlock (Phys_Memory_Space.Spinlock);
   end Free_Physical_Memory;

   procedure Free_Physical_Memory_Unlocked
     (Addr : Physical_Address_T; Result : out Function_Result)
   is
      Current_Block : Block_Access := null;
   begin
      Current_Block := Find_Block_With_Address (Addr);
      if Current_Block = null then
         Log_Error ("Memory block not found", Logging_Tags);
         Result := Memory_Block_Not_Found;
         return;
      end if;

      Current_Block.all.Free := True;

      Log_Debug ("Freed block at " & Addr'Image, Logging_Tags);

      --  Once this block has been freed, consolidate all of the
      --  physical memory blocks, then return.
      --  The 'Result' will be set by this call.
      Consolidate_Free_Physical_Memory_Blocks (Result);
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Free");
         Result := Constraint_Exception;
   end Free_Physical_Memory_Unlocked;

   function Get_Block_Size_In_Bytes (Order : Block_Order) return Natural is
   begin
      return Base_Block_Size * (2 ** Order);
   exception
      when Constraint_Error =>
         return 0;
   end Get_Block_Size_In_Bytes;

   procedure Get_Highest_Possible_Block_Order
     (Physical_Memory_Length       : Natural;
      Highest_Possible_Block_Order : out Natural;
      Result                       : out Function_Result) is
   begin
      --  Start at the maximum block order and work downwards.
      for Current_Order in reverse 0 .. Maximum_Block_Order loop
         --  If we've reached a block that fits within the total size of
         --  the physical memory space, exit.
         if Get_Block_Size_In_Bytes (Current_Order) <= Physical_Memory_Length
         then
            Highest_Possible_Block_Order := Current_Order;
            Result := Success;
            return;
         end if;
      end loop;

      --  If we've reached the end of the loop, the physical memory
      --  size is smaller than the smallest memory block, and is invalid.
      Highest_Possible_Block_Order := 0;
      Result := No_Block_Small_Enough;
   end Get_Highest_Possible_Block_Order;

   function Get_List_Tail return Block_Access is
      Current_Block : Block_Access := null;
   begin
      if Is_List_Empty then
         return null;
      end if;

      Current_Block := Phys_Memory_Space.Physical_Memory_Map_List_Head;
      while Current_Block.all.Next_Block /= null loop
         Current_Block := Current_Block.all.Next_Block;
      end loop;

      return Current_Block;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Get_List_Tail", Logging_Tags);
         return null;
   end Get_List_Tail;

   procedure Get_Smallest_Possible_Block_Order
     (Required_Size                 : Positive;
      Smallest_Possible_Block_Order : out Natural;
      Result                        : out Function_Result) is
   begin
      --  Start at the minimum block order and work upwards.
      for Current_Order in 0 .. Maximum_Block_Order loop
         --  If we've reached a block order large enough to fit the specified
         --  size, then return.
         if Get_Block_Size_In_Bytes (Current_Order) >= Required_Size then
            Smallest_Possible_Block_Order := Current_Order;
            Result := Success;
            return;
         end if;
      end loop;

      --  If we've reached the end of the loop, the physical memory
      --  size is larger than the largest memory block, and is invalid.
      Log_Error ("No physical memory block large enough", Logging_Tags);
      Smallest_Possible_Block_Order := 0;
      Result := Invalid_Physical_Memory_Size;
   end Get_Smallest_Possible_Block_Order;

   function Is_List_Empty return Boolean is
   begin
      return Phys_Memory_Space.Physical_Memory_Map_List_Head = null;
   end Is_List_Empty;

   function Is_Region_Intersecting
     (Start  : Physical_Address_T;
      Length : Positive;
      Block  : Physical_Memory_Block_T) return Boolean
   is
      Maximum_Start   : Address := Null_Address;
      Minimum_End     : Address := Null_Address;
      Block_End       : Address := Null_Address;
      Test_Region_End : Address := Null_Address;
   begin
      Test_Region_End := Address (Start) + Storage_Offset (Length);
      Block_End :=
        Address (Block.Address)
        + Storage_Offset (Get_Block_Size_In_Bytes (Block.Order));

      if Block.Address > Start then
         Maximum_Start := Address (Block.Address);
      else
         Maximum_Start := Address (Start);
      end if;

      if Block_End < Test_Region_End then
         Minimum_End := Block_End;
      else
         Minimum_End := Test_Region_End;
      end if;

      if Minimum_End > Maximum_Start then
         return True;
      end if;

      return False;
   end Is_Region_Intersecting;

   procedure Reallocate_Physical_Memory
     (Addr     : in out Physical_Address_T;
      New_Size : Positive;
      Result   : out Function_Result) is
   begin
      Acquire_Spinlock (Phys_Memory_Space.Spinlock);

      Reallocate_Unlocked (Addr, New_Size, Result);

      Release_Spinlock (Phys_Memory_Space.Spinlock);
   end Reallocate_Physical_Memory;

   procedure Reallocate_Unlocked
     (Addr     : in out Physical_Address_T;
      New_Size : Positive;
      Result   : out Function_Result)
   is
      New_Address : Physical_Address_T := Null_Physical_Address;

      Current_Block  : Block_Access := null;
      --  The block order required for the new required size.
      Required_Order : Natural := 0;
   begin
      --  Get the smallest possible block order required for the
      --  newly resized block.
      Get_Smallest_Possible_Block_Order (New_Size, Required_Order, Result);
      if Is_Error (Result) then
         return;
      end if;

      Current_Block := Find_Block_With_Address (Addr);
      if Current_Block = null then
         Result := Memory_Block_Not_Found;
         return;
      end if;

      --  If the required order is equal, do nothing.
      if Required_Order = Current_Block.all.Order then
         Result := Success;
         return;
      elsif Required_Order < Current_Block.all.Order then
         Divide_Block_To_Specified_Order
           (Current_Block.all, Required_Order, Result);
         if Is_Error (Result) then
            return;
         end if;

         Log_Debug ("Divided block for reallocation.", Logging_Tags);

         Addr := Current_Block.all.Address;
         Result := Success;

         return;
      end if;

      --  If the new size is larger than the current block size,
      --  a new block needs to be allocated, and the contents
      --  copied in.
      Allocate_Physical_Memory_Unlocked (New_Size, New_Address, Result);
      if Is_Error (Result) then
         return;
      end if;

      --  Move the allocated memory to the new address.
      Move_Memory_To_New_Address : declare
      begin
         Mapped_Old_Address : constant Virtual_Address_T :=
           Memory.Virtual.Get_Physical_Address_Virtual_Mapping (Addr);

         Mapped_New_Address : constant Virtual_Address_T :=
           Memory.Virtual.Get_Physical_Address_Virtual_Mapping (New_Address);

         Move
           (Address (Mapped_New_Address),
            Address (Mapped_Old_Address),
            New_Size);
      end Move_Memory_To_New_Address;

      --  Free the old allocation.
      Free_Physical_Memory_Unlocked (Addr, Result);
      if Is_Error (Result) then
         return;
      end if;

      Log_Debug ("Allocated larger block for reallocation.", Logging_Tags);

      Addr := New_Address;
      Result := Success;
   exception
      when Constraint_Error =>
         Result := Constraint_Exception;
   end Reallocate_Unlocked;

end Memory.Physical;
