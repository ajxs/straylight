-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Memory.Physical; use Memory.Physical;

package body Memory.Virtual.Paging is
   procedure Allocate_And_Initialise_New_Page_Table
     (Table_Physical_Address : out Physical_Address_T;
      Result                 : out Function_Result) is
   begin
      Log_Debug ("Creating new page table...", Logging_Tags);

      --  Allocate the 4KB of physical memory for the new page table.
      Allocate_Physical_Memory
        (RISCV.Paging.Small_Page_Size, Table_Physical_Address, Result);
      if Is_Error (Result) then
         return;
      end if;

      Log_Debug
        ("Allocated new page table at physical address"
         & Table_Physical_Address'Image,
         Logging_Tags);

      Initialise_New_Table :
      declare
         --  The new page table, with the physical ad`dress
         --  mapped into the kernel address space.
         Page_Table : Page_Table_T
         with
           Import,
           Alignment => 1,
           Address   =>
             Get_Physical_Address_Virtual_Mapping (Table_Physical_Address);
      begin
         --  Initialise the new page table entries.
         for Table_Entry of Page_Table loop
            Table_Entry := Null_Page_Table_Entry;
         end loop;
      end Initialise_New_Table;

      Log_Debug ("Created new page table.", Logging_Tags);

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error
           ("Constraint_Error: Allocate_And_Initialise_New_Page_Table");
         Table_Physical_Address := Null_Physical_Address;
         Result := Constraint_Exception;
   end Allocate_And_Initialise_New_Page_Table;

   function Get_Largest_Page_Size_For_Remaining_Region
     (Virtual_Address  : Virtual_Address_T;
      Physical_Address : Physical_Address_T;
      Remaining_Size   : Storage_Offset) return RISCV_Page_Size_T is
   begin
      --  Ensure the virtual and physical addresses are properly page aligned.
      --  Section 4.4.1 of the privileged spec specifies that 'megapages',
      --  and 'gigapages' must be virtually and physically aligned to
      --  their boundary.
      if Remaining_Size >= RISCV.Paging.Huge_Page_Size
        and then Is_Address_Aligned
                   (Virtual_Address, RISCV.Paging.Huge_Page_Size)
        and then Is_Address_Aligned
                   (Address (Physical_Address), RISCV.Paging.Huge_Page_Size)
      then
         return Huge;
      elsif Remaining_Size >= RISCV.Paging.Large_Page_Size
        and then Is_Address_Aligned
                   (Virtual_Address, RISCV.Paging.Large_Page_Size)
        and then Is_Address_Aligned
                   (Address (Physical_Address), RISCV.Paging.Large_Page_Size)
      then
         return Large;
      end if;

      return Small;
   end Get_Largest_Page_Size_For_Remaining_Region;

   procedure Map
     (Base_Page_Table_Address : Physical_Address_T;
      Virtual_Address         : Virtual_Address_T;
      Physical_Address        : Physical_Address_T;
      Size                    : Memory_Region_Size;
      Region_Flags            : Memory_Region_Flags_T;
      Result                  : out Function_Result)
   is
      --  Offset from the current address being mapped.
      --  This is used in a loop to iteratively map the specified range
      --  of memory.
      Addr_Offset : Storage_Offset := 0;
   begin
      --  Ensure that we're mapping to page aligned addresses.
      if not Is_Address_Page_Aligned (Virtual_Address)
        or else not Is_Address_Page_Aligned (Address (Physical_Address))
      then
         Log_Error ("Invalid non-aligned address");
         Result := Invalid_Non_Aligned_Address;
         return;
      end if;

      --  If the specified region size is not page aligned, exit.
      if Size mod RISCV.Paging.Small_Page_Size /= 0 then
         Log_Error ("Invalid physical memory size");
         Result := Invalid_Physical_Memory_Size;
         return;
      end if;

      --  Loop over the full region, mapping the largest page size that fits
      --  into the remaining amount of memory yet to be mapped with each
      --  iteration.
      loop
         --  The address of the current page being mapped.
         Curr_Map_Addr : constant Address := Virtual_Address + Addr_Offset;
         --  The current physical address being mapped.
         Curr_Phys_Addr : constant Physical_Address_T :=
           Physical_Address_T (Address (Physical_Address) + Addr_Offset);
         --  The amount of memory yet to be mapped.
         Remaining_Size : constant Storage_Offset :=
           Storage_Offset (Size) - Addr_Offset;
         --  The size of the current region being mapped.
         Region_Size : constant RISCV_Page_Size_T :=
           Get_Largest_Page_Size_For_Remaining_Region
             (Curr_Map_Addr, Curr_Phys_Addr, Remaining_Size);

         --  Increment the address offset by the size of the region.
         --  This can be performed safely at this point, since the
         --  virtual, and physical mapping addresses have already
         --  been calculated at this point.
         Addr_Offset := Addr_Offset + Page_Size_In_Bytes (Region_Size);

         --  Perform the region mapping.
         Map_Region
           (Base_Page_Table_Address,
            Curr_Map_Addr,
            Curr_Phys_Addr,
            Region_Size,
            Region_Flags,
            Result);
         if Is_Error (Result) then
            return;
         end if;

         exit when Addr_Offset >= Storage_Offset (Size);
      end loop;

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Exception: Memory.Virtual.Paging.Map");
         Result := Constraint_Exception;
   end Map;

   procedure Map_Region
     (Base_Page_Table_Address : Physical_Address_T;
      Virtual_Address         : Virtual_Address_T;
      Physical_Address        : Physical_Address_T;
      Region_Size             : RISCV_Page_Size_T;
      Region_Flags            : Memory_Region_Flags_T;
      Result                  : out Function_Result)
   is
      Curr_Table_Virtual_Addr  : System.Address := System.Null_Address;
      Curr_Table_Physical_Addr : Physical_Address_T := Null_Physical_Address;
   begin
      Log_Debug
        ("Mapping virtual memory region:"
         & ASCII.LF
         & "  Base Page Table Addr: "
         & Base_Page_Table_Address'Image
         & ASCII.LF
         & "  Virtual Address: "
         & Virtual_Address'Image
         & ASCII.LF
         & "  Physical Address: "
         & Physical_Address'Image
         & ASCII.LF
         & "  Region Size: "
         & Region_Size'Image,
         Logging_Tags_Paging);

      Curr_Table_Physical_Addr := Base_Page_Table_Address;
      Curr_Table_Virtual_Addr :=
        Get_Physical_Address_Virtual_Mapping (Base_Page_Table_Address);

      Walk_Page_Tables :
      for Table_Level in reverse 0 .. 2 loop
         Read_Table_Entry :
         declare
            --  The current level page table.
            Page_Table          : Page_Table_T
            with Import, Alignment => 1, Address => Curr_Table_Virtual_Addr;
            --  The index into the current level page table.
            Table_Idx           : Page_Table_Index_T := 0;
            --  Address of newly created tables, if needed.
            New_Table_Phys_Addr : Physical_Address_T := Null_Physical_Address;
         begin
            Table_Idx :=
              To_Virtual_Address (Virtual_Address).VPN (Table_Level);

            Log_Debug
              ("Walking PT level"
               & Table_Level'Image
               & ", index"
               & Table_Idx'Image
               & ", address"
               & Curr_Table_Virtual_Addr'Image
               & ", Physical address"
               & Curr_Table_Physical_Addr'Image,
               Logging_Tags_Paging);

            --  If this region is already mapped, then return.
            if Is_Leaf_Entry (Page_Table (Table_Idx)) then
               Log_Error ("Region not free");
               Result := Region_Not_Free;
               return;
            end if;

            --  If we're at the correct level for this region size, set
            --  the leaf page table entry, then return.
            if (Region_Size = Huge and then Table_Level = 2)
              or else (Region_Size = Large and then Table_Level = 1)
              or else (Region_Size = Small and then Table_Level = 0)
            then
               Page_Table (Table_Idx).PPN :=
                 To_Page_Aligned_Address (Address (Physical_Address));
               Page_Table (Table_Idx).V := True;
               Page_Table (Table_Idx).R := Region_Flags.Read;
               Page_Table (Table_Idx).W := Region_Flags.Write;
               Page_Table (Table_Idx).X := Region_Flags.Execute;
               Page_Table (Table_Idx).U := Region_Flags.User;

               Log_Debug
                 ("Mapped new virtual memory region at table index "
                  & Table_Idx'Image,
                  Logging_Tags_Paging);

               Result := Success;
               return;
            end if;

            --  If the entry at this level isn't a leaf entry, and
            --  we're not at the final level for this region size, check
            --  if we need to physically allocate the next table.
            if not Page_Table (Table_Idx).V then
               Log_Debug
                 ("Non-leaf entry found. Allocating new page table at index "
                  & Table_Idx'Image
                  & "...",
                  Logging_Tags_Paging);

               Allocate_And_Initialise_New_Page_Table
                 (New_Table_Phys_Addr, Result);
               if Is_Error (Result) then
                  return;
               end if;

               --  Set up the newly allocated page table at this level.
               Page_Table (Table_Idx).V := True;
               Page_Table (Table_Idx).PPN :=
                 To_Page_Aligned_Address (Address (New_Table_Phys_Addr));
            end if;

            Curr_Table_Physical_Addr :=
              Physical_Address_T
                (To_System_Address (Page_Table (Table_Idx).PPN));
            Curr_Table_Virtual_Addr :=
              Get_Physical_Address_Virtual_Mapping (Curr_Table_Physical_Addr);

         end Read_Table_Entry;
      end loop Walk_Page_Tables;

      --  This point should not be reached, consider it an unhandled
      --  exceptional scenario.
      Result := Unhandled_Exception;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Memory.Virtual.Paging.Map_Region");
         Result := Constraint_Exception;
   end Map_Region;

   procedure Unmap
     (Base_Page_Table_Address : Physical_Address_T;
      Virtual_Address         : Virtual_Address_T;
      Size                    : Memory_Region_Size;
      Result                  : out Function_Result)
   is
      --  Offset from the start of the region being unmapped.
      --  This is used in a loop to iteratively unmap the specified range
      --  of memory.
      Addr_Offset : Storage_Offset := 0;
      --  The size of the current region being unmapped.
      Region_Size : RISCV_Page_Size_T := Huge;
   begin
      --  Ensure that we're unmapping a page aligned addresses.
      if not Is_Address_Page_Aligned (Virtual_Address) then
         Log_Error ("Invalid non-aligned address");
         Result := Invalid_Non_Aligned_Address;
         return;
      end if;
      --  If the specified region size is not page aligned, exit.
      if Size mod RISCV.Paging.Small_Page_Size /= 0 then
         Log_Error ("Invalid memory size");
         Result := Invalid_Physical_Memory_Size;
         return;
      end if;

      Log_Debug
        ("Unmapping region:"
         & ASCII.LF
         & "  Virtual Address: "
         & Virtual_Address'Image
         & ASCII.LF
         & "  Size: "
         & Size'Image,
         Logging_Tags_Paging);

      --  Loop over the table, freeing entries until we've freed the
      --  specified size.
      while Addr_Offset < Storage_Offset (Size) loop
         --  Unmap the region specified at the current address, returning the
         --  size of the region unmapped. This region size is then added to the
         --  current address offset.
         Unmap_Region
           (Base_Page_Table_Address,
            Virtual_Address + Addr_Offset,
            Region_Size,
            Result);
         if Is_Error (Result) then
            return;
         end if;

         --  Increment the address offset by the size of the unmapped region.
         Addr_Offset := Addr_Offset + Page_Size_In_Bytes (Region_Size);
      end loop;

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error ("Constaint_error: Unmap");
         Result := Constraint_Exception;
   end Unmap;

   procedure Unmap_Region
     (Base_Page_Table_Address : Physical_Address_T;
      Virtual_Address         : Virtual_Address_T;
      Region_Size             : out RISCV_Page_Size_T;
      Result                  : out Function_Result)
   is
      --  The address of the current table being searched.
      Curr_Table_Virtual_Addr  : Virtual_Address_T := Null_Address;
      Curr_Table_Physical_Addr : Physical_Address_T := Null_Physical_Address;
   begin
      Curr_Table_Physical_Addr := Base_Page_Table_Address;
      Curr_Table_Virtual_Addr :=
        Get_Physical_Address_Virtual_Mapping (Base_Page_Table_Address);

      Walk_Page_Tables :
      for Table_Level in reverse 0 .. 2 loop
         Read_Table_Entry :
         declare
            --  The current level page table.
            Page_Table : Page_Table_T
            with Import, Alignment => 1, Address => Curr_Table_Virtual_Addr;
            --  The index into the current level page table.
            Table_Idx  : Page_Table_Index_T := 0;
         begin
            --  Set the size of the current entry.
            --  This is used to return the size of the entry that was freed.
            case Table_Level is
               when 2 =>
                  Region_Size := Huge;

               when 1 =>
                  Region_Size := Large;

               when 0 =>
                  Region_Size := Small;
            end case;

            Log_Debug
              ("Walking PT level"
               & Table_Level'Image
               & ", index"
               & Table_Idx'Image
               & ", mapped address"
               & Curr_Table_Virtual_Addr'Image
               & ", physical address"
               & Curr_Table_Physical_Addr'Image,
               Logging_Tags_Paging);

            Table_Idx :=
              To_Virtual_Address (Virtual_Address).VPN (Table_Level);

            --  If there is no entry at this virtual address, exit.
            if not Page_Table (Table_Idx).V then
               Log_Error ("Region not mapped");
               Result := Region_Not_Mapped;
               return;
            end if;

            --  If we've reached the leaf entry at this address,
            --  free it, and then check if this leaves the current page
            --  table empty.
            if Is_Leaf_Entry (Page_Table (Table_Idx)) then
               Page_Table (Table_Idx).V := False;

               Log_Debug
                 ("Unmapped level"
                  & Table_Level'Image
                  & " page, mapped address"
                  & Curr_Table_Virtual_Addr'Image
                  & ", index"
                  & Table_Idx'Image,
                  Logging_Tags_Paging);

               --  If this table is now fully free, and it is not a top-level
               --  page table, free the page allocation used to store it.
               --  Only free the page table memory if it's not the top-level.
               --  Only level 1 page tables are automatically created or
               --  destroyed by the kernel.
               if Is_Table_Free (Page_Table) and then Table_Level = 1 then
                  Log_Debug
                    ("Freeing empty level"
                     & Table_Level'Image
                     & " page table at address"
                     & Curr_Table_Virtual_Addr'Image,
                     Logging_Tags_Paging);

                  Free_Physical_Memory (Curr_Table_Physical_Addr, Result);
                  if Is_Error (Result) then
                     return;
                  end if;
               end if;

               Result := Success;
               return;
            end if;

            Curr_Table_Physical_Addr :=
              Physical_Address_T
                (To_System_Address (Page_Table (Table_Idx).PPN));
            Curr_Table_Virtual_Addr :=
              Get_Physical_Address_Virtual_Mapping (Curr_Table_Physical_Addr);

         end Read_Table_Entry;
      end loop Walk_Page_Tables;

      --  This point should not be reached, consider it an unhandled
      --  exceptional scenario. This means we could neither find an
      --  unmapped region, nor a mapped one.
      Log_Error ("Unhandled_Exception");
      Result := Unhandled_Exception;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Unmap_Region");
         Result := Constraint_Exception;
   end Unmap_Region;

end Memory.Virtual.Paging;
