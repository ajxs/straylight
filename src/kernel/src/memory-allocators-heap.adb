-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

package body Memory.Allocators.Heap is
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

   procedure Add_Memory_Region_To_Heap_Unlocked
     (Memory_Heap      : in out Memory_Heap_T;
      Virtual_Address  : Virtual_Address_T;
      Physical_Address : Physical_Address_T;
      Size             : Storage_Offset;
      Result           : out Function_Result)
   is
      New_Index  : Heap_Memory_Region_Index_T := Null_Memory_Region_Index;
      Curr_Index : Heap_Memory_Region_Index_T := Null_Memory_Region_Index;
      Prev_Index : Heap_Memory_Region_Index_T := Null_Memory_Region_Index;

      Memory_Regions renames Memory_Heap.Memory_Regions;
   begin
      --  Ensure that the new region doesn't overlap any existing region.
      Curr_Index := Memory_Heap.Memory_Regions_Head;
      while Curr_Index /= Null_Memory_Region_Index loop
         if Is_Region_Intersecting
              (Memory_Heap.Memory_Regions (Curr_Index),
               Virtual_Address,
               Physical_Address,
               Size)
         then
            Log_Error ("Heap memory region overlapping", Logging_Tags);
            Result := Region_Is_Overlapping;
            return;
         end if;

         Curr_Index := Memory_Heap.Memory_Regions (Curr_Index).Next_Region;
      end loop;

      Log_Debug
        ("Inserting new heap memory region: "
         & ASCII.LF
         & "  VAddr: "
         & Virtual_Address'Image
         & ASCII.LF
         & "  PAddr: "
         & Physical_Address'Image
         & ASCII.LF
         & "  Size:  "
         & Size'Image,
         Logging_Tags);

      New_Index := Find_Unused_Memory_Region_Entry (Memory_Heap);
      if New_Index = Null_Memory_Region_Index then
         Log_Error ("Heap memory region array exhausted", Logging_Tags);
         Result := Region_Array_Exhausted;
         return;
      end if;

      Memory_Regions (New_Index) :=
        (Virtual_Address  => Virtual_Address,
         Physical_Address => Physical_Address,
         Size             => Size,
         Entry_Used       => True,
         Next_Region      => Null_Memory_Region_Index);

      if Memory_Heap.Memory_Regions_Head = Null_Memory_Region_Index then
         Memory_Heap.Memory_Regions_Head := New_Index;
         Result := Success;
      else
         Curr_Index := Memory_Heap.Memory_Regions_Head;

         while Curr_Index /= Null_Memory_Region_Index loop
            Prev_Index := Curr_Index;
            Curr_Index := Memory_Regions (Curr_Index).Next_Region;
         end loop;

         Memory_Regions (Prev_Index).Next_Region := New_Index;
      end if;

      --  Insert the corresponding free region.
      Insert_Free_Region
        (Memory_Heap, Virtual_Address, Physical_Address, Size, Result);
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Add_Memory_Region_To_Heap_Unlocked");
         Result := Constraint_Exception;
   end Add_Memory_Region_To_Heap_Unlocked;

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

   procedure Allocate_Unlocked
     (Memory_Heap       : in out Memory_Heap_T;
      Size              : Positive;
      Allocation_Result : out Memory_Allocation_Result;
      Result            : out Function_Result;
      Alignment         : Storage_Offset := 1)
   is
      --  This contains the physical and virtual addresses of the allocated
      --  memory REGION. The addresses will be adjusted to take the allocation
      --  header into account prior to being returned.
      Region_Allocation_Result : Memory_Allocation_Result :=
        (Virtual_Address  => Null_Address,
         Physical_Address => Null_Physical_Address);
   begin
      Log_Debug
        ("Allocating heap memory:"
         & ASCII.LF
         & "  Size:      "
         & Size'Image
         & ASCII.LF
         & "  Alignment: "
         & Alignment'Image,
         Logging_Tags);

      Find_And_Allocate_Free_Region
        (Memory_Heap,
         Storage_Offset (Size),
         Alignment,
         Region_Allocation_Result,
         Result);
      if Is_Error (Result) then
         Allocation_Result :=
           (Virtual_Address  => Null_Address,
            Physical_Address => Null_Physical_Address);

         return;
      end if;

      --  Set the allocation header for the newly allocated address.
      Initialise_Allocated_Region_Header
        (Region_Allocation_Result.Virtual_Address, Storage_Offset (Size));

      --  Note that this takes alignment into account.
      --  It's the final address that's aligned, not the start of the region.
      Allocation_Result :=
        (Virtual_Address  =>
           Region_Allocation_Result.Virtual_Address
           + (Allocation_Header_T'Size / 8),
         Physical_Address =>
           Region_Allocation_Result.Physical_Address
           + (Allocation_Header_T'Size / 8));

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
         Logging_Tags);

      --  Clear the newly allocated block of memory.
      Set (Allocation_Result.Virtual_Address, 0, Size);

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Allocate_Unlocked");
         Result := Constraint_Exception;
   end Allocate_Unlocked;

   function Calculate_Region_Alignment_Offset
     (Start_Address : Virtual_Address_T; Alignment : Storage_Offset)
      return Storage_Offset is
   begin
      --  Since a virtual address can only be 39-bits in length, we can mask
      --  the address to help avoid an arithmetic overflow.
      Masked_Address : constant Integer_Address :=
        To_Integer (Start_Address) and 16#7F_FFFF_FFFF#;

      --  The alignment offset needs to factor in the size of the allocation
      --  header, since it's the final allocated address that needs to be
      --  aligned, not the address of the allocation header.
      return
        Alignment
        - Storage_Offset (Masked_Address + (Allocation_Header_T'Size / 8))
          mod Alignment;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Calculate_Region_Alignment_Offset");
         return 0;
   end Calculate_Region_Alignment_Offset;

   procedure Coalesce_Free_Region_Entries (Memory_Heap : in out Memory_Heap_T)
   is
      Curr_Region : Free_Region_Access := Memory_Heap.Free_Regions_Head;
      Prev_Region : Free_Region_Access := null;

      End_Virtual_Address  : Virtual_Address_T := Null_Address;
      End_Physical_Address : Physical_Address_T := Null_Physical_Address;

      Matched : Boolean := False;
   begin
      while Curr_Region /= null and then Curr_Region.all.Entry_Used loop
         if Prev_Region /= null and then Curr_Region.all.Entry_Used then
            End_Virtual_Address :=
              Prev_Region.all.Virtual_Address + Prev_Region.all.Size;

            End_Physical_Address :=
              Prev_Region.all.Physical_Address + Prev_Region.all.Size;

            Matched :=
              Curr_Region.all.Virtual_Address = End_Virtual_Address
              and then Curr_Region.all.Physical_Address = End_Physical_Address;

            if Matched then
               Prev_Region.all.Size :=
                 Prev_Region.all.Size + Curr_Region.all.Size;
               Prev_Region.all.Next_Region := Curr_Region.all.Next_Region;

               Curr_Region.all.Entry_Used := False;
            else
               --  If we previously coalesced the current region, keep
               --  coalescing from its previous region.
               Prev_Region := Curr_Region;
            end if;
         end if;

         Curr_Region := Curr_Region.all.Next_Region;
      end loop;
   exception
      when Constraint_Error =>
         null;
   end Coalesce_Free_Region_Entries;

   --  Assumes heap spinlock already held.
   procedure Find_And_Allocate_Free_Region
     (Memory_Heap       : in out Memory_Heap_T;
      Size              : Storage_Offset;
      Alignment         : Storage_Offset;
      Allocation_Result : out Memory_Allocation_Result;
      Result            : out Function_Result)
   is
      Curr_Region : Free_Region_Access := Memory_Heap.Free_Regions_Head;
      Prev_Region : Free_Region_Access := null;

      Alignment_Offset     : Storage_Offset := 0;
      Remaining_Size       : Storage_Offset := 0;
      Inserted_Entry       : Free_Region_Access := null;
      Real_Allocation_Size : Storage_Offset := 0;
   begin
      Real_Allocation_Size := Size + (Allocation_Header_T'Size / 8);

      while Curr_Region /= null loop
         --  If the region aligment is 1, there's no need to calculate offset.
         if Alignment /= 1 then
            Alignment_Offset :=
              Calculate_Region_Alignment_Offset
                (Curr_Region.all.Virtual_Address, Alignment);
         end if;

         Log_Debug
           ("Testing free region:"
            & ASCII.LF
            & "  offset: "
            & Curr_Region.all.Virtual_Address'Image
            & ASCII.LF
            & "  size:   "
            & Curr_Region.all.Size'Image
            & ASCII.LF
            & "  align:  "
            & Alignment_Offset'Image,
            Logging_Tags);

         if (Curr_Region.all.Size = Real_Allocation_Size)
           and then Alignment_Offset = 0
         then
            Log_Debug
              ("Found region perfectly matching size/alignment requirements",
               Logging_Tags);

            --  If the current region is the same size and alignment as the
            --  desired allocation size, it can be 'fully allocated' without
            --  subdividing the free block.
            --  Clear the current region, and 'attach' its previous node to
            --  its next to 'remove' it from the list.
            Curr_Region.all.Entry_Used := False;

            --  If there is a previous region set its next region pointer to
            --  the next region pointer of the current entry.
            if Prev_Region /= null then
               Prev_Region.all.Next_Region := Curr_Region.all.Next_Region;
            else
               Memory_Heap.Free_Regions_Head := Curr_Region.all.Next_Region;
            end if;

            Curr_Region.all.Next_Region := null;

            Allocation_Result :=
              (Virtual_Address  => Curr_Region.all.Virtual_Address,
               Physical_Address => Curr_Region.all.Physical_Address);
            Result := Success;
            return;
         elsif (Curr_Region.all.Size > Real_Allocation_Size)
           and then Alignment_Offset = 0
         then
            Log_Debug
              ("Found region matching alignment requirements. Resizing to fit",
               Logging_Tags);

            --  If the current region is larger than the desired allocation
            --  size, and aligned, amend the current region so that its new
            --  size reflects the alignment.
            --  At the same time, move its start offset forward to reflect the
            --  allocated region being at its start.
            Allocation_Result :=
              (Virtual_Address  => Curr_Region.all.Virtual_Address,
               Physical_Address => Curr_Region.all.Physical_Address);

            Curr_Region.all.Size :=
              Curr_Region.all.Size - Real_Allocation_Size;

            Curr_Region.all.Virtual_Address :=
              Curr_Region.all.Virtual_Address + Real_Allocation_Size;

            Curr_Region.all.Physical_Address :=
              Curr_Region.all.Physical_Address + Real_Allocation_Size;

            Result := Success;
            return;
         elsif Curr_Region.all.Size
           >= (Real_Allocation_Size + Alignment_Offset)
         then
            Log_Debug
              ("Found region matching size requirements. "
               & "Resizing to pad alignment requirements: "
               & Alignment_Offset'Image,
               Logging_Tags);

            --  If the alignment doesn't match, but the current block is
            --  large enough to fit the allocation (and the number of bytes
            --  necessary to offset the start to make the block match the
            --  desired alignment), then resize the current block to match the
            --  amount necessary to get the desired alignment.
            --  If there is any remaining size after this, insert a new region
            --  with the remaining size after the allocation.
            Remaining_Size :=
              Curr_Region.all.Size - (Real_Allocation_Size + Alignment_Offset);

            Curr_Region.all.Size := Alignment_Offset;

            if Remaining_Size > 0 then
               Log_Debug
                 ("Inserting new region to fill remaining space "
                  & "after allocating with alignment padding, "
                  & "with size: "
                  & Remaining_Size'Image,
                  Logging_Tags);

               Inserted_Entry := Find_Unused_Free_Region_Entry (Memory_Heap);
               if Inserted_Entry = null then
                  goto Exit_Heap_Exhausted;
               end if;

               Inserted_Entry.all.Entry_Used := True;
               Inserted_Entry.all.Virtual_Address :=
                 Curr_Region.all.Virtual_Address + Alignment_Offset + Size;

               Inserted_Entry.all.Physical_Address :=
                 Curr_Region.all.Physical_Address + Alignment_Offset + Size;

               Inserted_Entry.all.Size := Remaining_Size;

               Inserted_Entry.all.Next_Region := Curr_Region.all.Next_Region;
               Curr_Region.all.Next_Region := Inserted_Entry;
            end if;

            Allocation_Result :=
              (Virtual_Address  =>
                 Curr_Region.all.Virtual_Address + Alignment_Offset,
               Physical_Address =>
                 Curr_Region.all.Physical_Address + Alignment_Offset);

            Result := Success;
            return;
         end if;

         Prev_Region := Curr_Region;
         Curr_Region := Curr_Region.all.Next_Region;
      end loop;

      <<Exit_Heap_Exhausted>>
      Log_Error ("Heap exhausted");
      Result := Heap_Exhausted;
   exception
      when Constraint_Error =>
         Result := Constraint_Exception;
   end Find_And_Allocate_Free_Region;

   procedure Find_Region_By_Physical_Address
     (Memory_Heap      : Memory_Heap_T;
      Physical_Address : Physical_Address_T;
      Found_Index      : out Heap_Memory_Region_Index_T;
      Found            : out Boolean)
   is
      Curr_Index : Heap_Memory_Region_Index_T := Null_Memory_Region_Index;
   begin
      Curr_Index := Memory_Heap.Memory_Regions_Head;
      while Curr_Index /= Null_Memory_Region_Index loop
         if Is_Physical_Address_within_Memory_Region
              (Memory_Heap.Memory_Regions (Curr_Index), Physical_Address)
         then
            Found_Index := Curr_Index;
            Found := True;

            return;
         end if;

         Curr_Index := Memory_Heap.Memory_Regions (Curr_Index).Next_Region;
      end loop;

      Found := False;
   exception
      when Constraint_Error =>
         Found_Index := Null_Memory_Region_Index;
   end Find_Region_By_Physical_Address;

   procedure Find_Region_By_Virtual_Address
     (Memory_Heap     : Memory_Heap_T;
      Virtual_Address : Address;
      Found_Index     : out Heap_Memory_Region_Index_T;
      Found           : out Boolean)
   is
      Curr_Index : Heap_Memory_Region_Index_T := Null_Memory_Region_Index;
   begin
      Curr_Index := Memory_Heap.Memory_Regions_Head;
      while Curr_Index /= Null_Memory_Region_Index loop
         if Is_Virtual_Address_Within_Memory_Region
              (Memory_Heap.Memory_Regions (Curr_Index), Virtual_Address)
         then
            Found_Index := Curr_Index;
            Found := True;

            return;
         end if;

         Curr_Index := Memory_Heap.Memory_Regions (Curr_Index).Next_Region;
      end loop;

      Found := False;
   exception
      when Constraint_Error =>
         Found_Index := Null_Memory_Region_Index;
   end Find_Region_By_Virtual_Address;

   function Find_Unused_Free_Region_Entry
     (Memory_Heap : in out Memory_Heap_T) return Free_Region_Access is
   begin
      for Region_Entry of Memory_Heap.Free_Regions loop
         if Region_Entry.Entry_Used = False then
            return Region_Entry'Unchecked_Access;
         end if;
      end loop;

      return null;
   end Find_Unused_Free_Region_Entry;

   function Find_Unused_Memory_Region_Entry
     (Memory_Heap : Memory_Heap_T) return Heap_Memory_Region_Index_T is
   begin
      for Index in Heap_Memory_Region_List_T'Range loop
         if not Memory_Heap.Memory_Regions (Index).Entry_Used then
            return Index;
         end if;
      end loop;

      return Null_Memory_Region_Index;
   end Find_Unused_Memory_Region_Entry;

   procedure Free
     (Memory_Heap               : in out Memory_Heap_T;
      Allocated_Virtual_Address : Virtual_Address_T;
      Result                    : out Function_Result) is
   begin
      Acquire_Spinlock (Memory_Heap.Spinlock);

      Free_Unlocked (Memory_Heap, Allocated_Virtual_Address, Result);

      Release_Spinlock (Memory_Heap.Spinlock);
   end Free;

   procedure Free_Unlocked
     (Memory_Heap               : in out Memory_Heap_T;
      Allocated_Virtual_Address : Virtual_Address_T;
      Result                    : out Function_Result)
   is
      Region_Size               : Storage_Offset := 0;
      Address_Is_In_Heap_Region : Boolean := False;
      Region_Index              : Heap_Memory_Region_Index_T :=
        Null_Memory_Region_Index;
   begin
      Log_Debug
        ("Freeing heap memory: " & Allocated_Virtual_Address'Image,
         Logging_Tags);

      Region_Virtual_Address : constant Virtual_Address_T :=
        Allocated_Virtual_Address
        - Storage_Offset (Allocation_Header_T'Size / 8);

      --  Read the allocation header to determine the size of the allocation.
      Read_Header :
      declare
         Header : Allocation_Header_T
         with Import, Alignment => 1, Address => Region_Virtual_Address;
      begin
         if not Is_Valid_Allocation_Header (Header) then
            Result := Address_Not_In_Heap;
            return;
         end if;

         --  It's important to take the header size into account!
         Region_Size := Header.Size + (Allocation_Header_T'Size / 8);
      end Read_Header;

      --  Find the memory region in this heap that contains the specified
      --  virtual address.
      Find_Region_By_Virtual_Address
        (Memory_Heap,
         Region_Virtual_Address,
         Region_Index,
         Address_Is_In_Heap_Region);
      if not Address_Is_In_Heap_Region then
         Result := Address_Not_In_Heap;
         return;
      end if;

      --  To get the physical address of the region, we calculate the offset
      --  of the virtual address from the start of the region, and add this
      --  to the physical address of the region.
      Offset_Within_Region : constant Storage_Offset :=
        Region_Virtual_Address
        - Memory_Heap.Memory_Regions (Region_Index).Virtual_Address;

      Region_Physical_Address : constant Physical_Address_T :=
        Memory_Heap.Memory_Regions (Region_Index).Physical_Address
        + Offset_Within_Region;

      Log_Debug
        ("Freeing allocated memory:"
         & ASCII.LF
         & "  VAddr: "
         & Region_Virtual_Address'Image
         & ASCII.LF
         & "  PAddr: "
         & Region_Physical_Address'Image
         & ASCII.LF
         & "  Size:  "
         & Region_Size'Image,
         Logging_Tags);

      --  Insert a new 'free region entry' to track the freed space.
      Insert_Free_Region
        (Memory_Heap,
         Region_Virtual_Address,
         Region_Physical_Address,
         Region_Size,
         Result);
      if Is_Error (Result) then
         return;
      end if;

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Free_Unlocked");
         Result := Constraint_Exception;
   end Free_Unlocked;

   procedure Initialise_Allocated_Region_Header
     (Region_Address : Address; Size : Storage_Offset)
   is
      New_Header : Allocation_Header_T
      with Import, Alignment => 1, Address => Region_Address;
   begin
      New_Header.Identity := Allocation_Header_Magic_Number;
      New_Header.Size := Size;
   end Initialise_Allocated_Region_Header;

   procedure Insert_Free_Region
     (Memory_Heap      : in out Memory_Heap_T;
      Virtual_Address  : Virtual_Address_T;
      Physical_Address : Physical_Address_T;
      Size             : Storage_Offset;
      Result           : out Function_Result)
   is
      --  The new entry being added.
      New_Entry : Free_Region_Access := null;

      Curr_Region : Free_Region_Access := null;
      Prev_Region : Free_Region_Access := null;
   begin
      --  Find an unused entry in the list, and initialise it.
      New_Entry := Find_Unused_Free_Region_Entry (Memory_Heap);
      if New_Entry = null then
         Log_Error ("No free entries in free region list");
         Result := No_Free_Entries;
         return;
      end if;

      New_Entry.all.Virtual_Address := Virtual_Address;
      New_Entry.all.Physical_Address := Physical_Address;
      New_Entry.all.Entry_Used := True;
      New_Entry.all.Size := Size;

      --  If the region list head is empty, assign the new entry node
      --  as the head node.
      if Memory_Heap.Free_Regions_Head = null then
         Memory_Heap.Free_Regions_Head := New_Entry;
         Result := Success;
         return;
      end if;

      --  Iterate through the list of free regions until one is found with
      --  a higher starting offset. Add the new region _before_ this entry in
      --  the list to perform an insertion sort. Ensuring all regions are
      --  sorted by offset into the heap. This allows for easily coalescing
      --  adjacent entries.
      --  @TODO: How to handle trying to find regions that are contiguous
      --  both virtually and physically?
      Curr_Region := Memory_Heap.Free_Regions_Head;
      while Curr_Region /= null loop
         if Curr_Region.all.Virtual_Address > Virtual_Address then
            New_Entry.all.Next_Region := Curr_Region;

            if Prev_Region /= null then
               Prev_Region.all.Next_Region := New_Entry;
            else
               --  If the _current_ region's offset is higher than that of the
               --  new entry, and there is no 'previous region', this means
               --  that the new region belongs at the head of the list.
               --  Make the new region the new list head. The 'current region'
               --  points to the original list head.
               Memory_Heap.Free_Regions_Head := New_Entry;
            end if;

            Result := Success;
            return;
         end if;

         Prev_Region := Curr_Region;
         Curr_Region := Curr_Region.all.Next_Region;
      end loop;

      --  After inserting the new free region 'Coalesce' all of the free
      --  region entries. This will combine any adjacent regions into a single
      --  region entry.
      Coalesce_Free_Region_Entries (Memory_Heap);

      Result := Success;
   exception
      when Constraint_Error =>
         Result := Constraint_Exception;
   end Insert_Free_Region;

   function Is_Virtual_Address_In_Heap
     (Memory_Heap : Memory_Heap_T; Virtual_Address : Virtual_Address_T)
      return Boolean
   is
      Curr_Index : Heap_Memory_Region_Index_T := Null_Memory_Region_Index;
   begin
      Curr_Index := Memory_Heap.Memory_Regions_Head;
      while Curr_Index /= Null_Memory_Region_Index loop
         if Is_Virtual_Address_Within_Memory_Region
              (Memory_Heap.Memory_Regions (Curr_Index), Virtual_Address)
         then
            return True;
         end if;
         Curr_Index := Memory_Heap.Memory_Regions (Curr_Index).Next_Region;
      end loop;

      return False;
   exception
      when Constraint_Error =>
         Log_Error
           ("Constraint_Error: Is_Virtual_Address_In_Heap", Logging_Tags);

         return False;
   end Is_Virtual_Address_In_Heap;

   procedure Print_Memory_Regions (Memory_Heap : Memory_Heap_T) is
      Curr_Region : Free_Region_Access := Memory_Heap.Free_Regions_Head;
      Curr_Index  : Heap_Memory_Region_Index_T;
   begin
      Log_Debug ("Heap Free Memory Regions:", Logging_Tags);
      while Curr_Region /= null loop
         Log_Debug
           ("Free Region: "
            & ASCII.LF
            & "  Virtual Address:  "
            & Curr_Region.all.Virtual_Address'Image
            & ASCII.LF
            & "  Physical Address: "
            & Curr_Region.all.Physical_Address'Image
            & ASCII.LF
            & "  Free Region Size: "
            & Curr_Region.all.Size'Image
            & ASCII.LF
            & "--------------",
            Logging_Tags);

         Curr_Region := Curr_Region.all.Next_Region;
      end loop;

      Curr_Index := Memory_Heap.Memory_Regions_Head;
      Log_Debug ("Heap Total Memory Regions:", Logging_Tags);
      while Curr_Index /= Null_Memory_Region_Index loop
         Log_Debug
           ("Total Memory Region: "
            & ASCII.LF
            & "  Virtual Address: "
            & Memory_Heap.Memory_Regions (Curr_Index).Virtual_Address'Image
            & ASCII.LF
            & "  Physical Address: "
            & Memory_Heap.Memory_Regions (Curr_Index).Physical_Address'Image
            & ASCII.LF
            & "  Mem Region Size:  "
            & Memory_Heap.Memory_Regions (Curr_Index).Size'Image
            & ASCII.LF
            & "--------------",
            Logging_Tags);

         Curr_Index := Memory_Heap.Memory_Regions (Curr_Index).Next_Region;
      end loop;

   exception
      when Constraint_Error =>
         null;
   end Print_Memory_Regions;

end Memory.Allocators.Heap;
