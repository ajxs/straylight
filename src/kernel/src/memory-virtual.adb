-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Memory.Allocators;      use Memory.Allocators;
with Memory.Allocators.Heap; use Memory.Allocators.Heap;
with Memory.Allocators.Page; use Memory.Allocators.Page;
with Memory.Virtual.Paging;
with Memory.Physical;        use Memory.Physical;
with RISCV;                  use RISCV;
with RISCV.Paging;           use RISCV.Paging;
with System_State;           use System_State;

package body Memory.Virtual is
   procedure Copy_Kernel_Memory_Mappings_Into_Address_Space
     (Source_Addr_Space : Virtual_Memory_Space_T;
      Dest_Addr_Space   : in out Virtual_Memory_Space_T;
      Result            : out Function_Result)
   is
      Source_Base_Page_Table : Page_Table_T
      with
        Import,
        Alignment => 1,
        Address   =>
          Get_Physical_Address_Virtual_Mapping
            (Source_Addr_Space.Base_Page_Table_Addr);

      Destination_Base_Page_Table : Page_Table_T
      with
        Import,
        Alignment => 1,
        Address   =>
          Get_Physical_Address_Virtual_Mapping
            (Dest_Addr_Space.Base_Page_Table_Addr);
   begin
      Acquire_Spinlock (Dest_Addr_Space.Spinlock);

      --  The starting page table index for kernel mappings.
      Start_Table_Idx : constant Page_Table_Index_T :=
        To_Virtual_Address (Kernel_Address_Space_Start).VPN (2);

      for I in Start_Table_Idx .. Page_Table_Index_T'Last loop
         Destination_Base_Page_Table (I) := Source_Base_Page_Table (I);
      end loop;

      Result := Success;

      Release_Spinlock (Dest_Addr_Space.Spinlock);
   end Copy_Kernel_Memory_Mappings_Into_Address_Space;

   procedure Copy_Canonical_Kernel_Memory_Mappings_Into_Address_Space
     (Dest_Addr_Space : in out Virtual_Memory_Space_T;
      Result          : out Function_Result) is
   begin
      Copy_Kernel_Memory_Mappings_Into_Address_Space
        (Kernel_Address_Space, Dest_Addr_Space, Result);
   end Copy_Canonical_Kernel_Memory_Mappings_Into_Address_Space;

   procedure Create_New_Process_Memory_Space
     (New_Memory_Space : out Virtual_Memory_Space_T;
      Result           : out Function_Result) is
   begin
      Log_Debug ("Creating new process memory space...", Logging_Tags);

      --  Allocate the process' base page table.
      Memory.Virtual.Paging.Allocate_And_Initialise_New_Page_Table
        (New_Memory_Space.Base_Page_Table_Addr, Result);
      if Is_Error (Result) then
         return;
      end if;

      Log_Debug ("Created new process memory space.", Logging_Tags);
   end Create_New_Process_Memory_Space;

   procedure Deallocate_Memory_Space
     (Virt_Memory_Space : in out Virtual_Memory_Space_T;
      Result            : out Function_Result) is
   begin
      Acquire_Spinlock (Virt_Memory_Space.Spinlock);

      Deallocate_Memory_Space_Unlocked (Virt_Memory_Space, Result);

      Release_Spinlock (Virt_Memory_Space.Spinlock);
   end Deallocate_Memory_Space;

   procedure Deallocate_Memory_Space_Unlocked
     (Virt_Memory_Space : in out Virtual_Memory_Space_T;
      Result            : out Function_Result) is
   begin
      Log_Debug
        ("Deallocating virtual memory space with base page table address: "
         & Virt_Memory_Space.Base_Page_Table_Addr'Image,
         Logging_Tags);

      --  Deallocate all virtual memory mappings.
      --  This will free any allocated physical memory pages as well.
      while Virt_Memory_Space.Memory_Map_List_Head /= null loop
         Memory.Virtual.Unmap_Unlocked
           (Virt_Memory_Space,
            Virt_Memory_Space.Memory_Map_List_Head.all.Virtual_Addr,
            Result);
         if Is_Error (Result) then
            return;
         end if;
      end loop;

      --  Deallocate the base page table.
      Free_Physical_Memory (Virt_Memory_Space.Base_Page_Table_Addr, Result);
      if Is_Error (Result) then
         return;
      end if;

      Log_Debug ("Deallocated virtual memory space.", Logging_Tags);

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error
           ("Constraint_Error: Deallocate_Memory_Space_Unlocked",
            Logging_Tags);
         Result := Constraint_Exception;
   end Deallocate_Memory_Space_Unlocked;

   procedure Find_Unused_List_Entry_Index
     (Addr_Space : Virtual_Memory_Space_T;
      Free_Index : out Positive;
      Result     : out Function_Result) is
   begin
      --  Find the first non-allocated mapping entry.
      for I in 1 .. Maximum_Virtual_Memory_Mapping_Entries loop
         if not Addr_Space.Memory_Map (I).Entry_Used then
            Free_Index := I;
            Result := Success;
            return;
         end if;
      end loop;

      --  If we reach this statement, we can infer that the mapping
      --  array entries are exhausted.
      Free_Index := 1;
      Result := Memory_Map_Array_Exhausted;
      Log_Error ("Memory map exhausted", Logging_Tags);
   end Find_Unused_List_Entry_Index;

   function Get_Real_Region_Size
     (Size : Memory_Region_Size) return Memory_Region_Size is
   begin
      Modulo : constant Memory_Region_Size := Size mod Small_Page_Size;
      if Modulo /= 0 then
         return Size - Modulo + Small_Page_Size;
      end if;

      return Size;
   exception
      when Constraint_Error =>
         return 1;
   end Get_Real_Region_Size;

   function Is_Region_Intersecting
     (Region     : Virtual_Memory_Mapping_T;
      Start_Addr : Virtual_Address_T;
      Size       : Memory_Region_Size) return Boolean
   is
      Maximum_Start : Virtual_Address_T := Null_Address;
      Minimum_End   : Virtual_Address_T := Null_Address;
   begin
      Test_Region_End : constant Virtual_Address_T := Start_Addr + Size;
      Region_End : constant Virtual_Address_T :=
        Region.Virtual_Addr + Region.Size;

      if Region.Virtual_Addr > Start_Addr then
         Maximum_Start := Region.Virtual_Addr;
      else
         Maximum_Start := Start_Addr;
      end if;

      if Region_End < Test_Region_End then
         Minimum_End := Region_End;
      else
         Minimum_End := Test_Region_End;
      end if;

      return Minimum_End > Maximum_Start;
   end Is_Region_Intersecting;

   procedure Map
     (Virt_Memory_Space : in out Virtual_Memory_Space_T;
      Virtual_Address   : Virtual_Address_T;
      Physical_Address  : Physical_Address_T;
      Size              : Memory_Region_Size;
      Region_Flags      : Memory_Region_Flags_T;
      Result            : out Function_Result) is
   begin
      Acquire_Spinlock (Virt_Memory_Space.Spinlock);

      Memory.Virtual.Map_Unlocked
        (Virt_Memory_Space,
         Virtual_Address,
         Physical_Address,
         Size,
         Region_Flags,
         Result);

      Release_Spinlock (Virt_Memory_Space.Spinlock);
   end Map;

   procedure Map_Unlocked
     (Virt_Memory_Space : in out Virtual_Memory_Space_T;
      Virtual_Address   : Virtual_Address_T;
      Physical_Address  : Physical_Address_T;
      Size              : Memory_Region_Size;
      Region_Flags      : Memory_Region_Flags_T;
      Result            : out Function_Result)
   is
      Current_Region  : Mapping_Access := null;
      Previous_Region : Mapping_Access := null;
      Real_Size       : Memory_Region_Size := 1;
      New_Index       : Positive := 1;
   begin
      if Virt_Memory_Space.User_Address_Space
        and then not Is_Userspace_Address (Virtual_Address)
      then
         Log_Error ("Invalid non-userspace address", Logging_Tags);
         Result := Invalid_Address_Argument;
         return;
      end if;

      if not Is_Valid_SV39_Virtual_Address (Virtual_Address) then
         Log_Error ("Invalid virtual address", Logging_Tags);
         Result := Invalid_Address_Argument;
         return;
      end if;

      --  Check that the provided addresses are page aligned.
      if not Is_Address_Page_Aligned (Virtual_Address)
        or else not Is_Address_Page_Aligned (Address (Physical_Address))
      then
         Log_Error ("Invalid non-aligned address", Logging_Tags);
         Result := Invalid_Non_Aligned_Address;
         return;
      end if;

      if Size = 0 then
         Log_Error ("Invalid memory size", Logging_Tags);
         Result := Invalid_Physical_Memory_Size;
         return;
      end if;

      if not Validate_Memory_Region_Permissions (Region_Flags) then
         Log_Error ("Invalid memory permissions", Logging_Tags);
         Result := Illegal_Region_Permissions;
         return;
      end if;

      --  Ensures the size of the virtual memory mapping is page aligned.
      Real_Size := Get_Real_Region_Size (Size);

      --  Find the first non-allocated mapping entry and use this for the
      --  new mapping.
      --  Running this first has the benefit of checking whether the list
      --  entries are exhausted.
      Find_Unused_List_Entry_Index (Virt_Memory_Space, New_Index, Result);
      if Is_Error (Result) then
         return;
      end if;

      --  Iterate through until the end of the mapping list is reached,
      --  testing whether the current mapping would intersect with an
      --  existing mapping.
      Current_Region := Virt_Memory_Space.Memory_Map_List_Head;
      while Current_Region /= null loop
         if Is_Region_Intersecting (Current_Region.all, Virtual_Address, Size)
         then
            Log_Error
              ("Regions Intersect: "
               & ASCII.LF
               & "  New VAddr: "
               & Virtual_Address'Image
               & ASCII.LF
               & "  Intersecting Region VAddr: "
               & Current_Region.all.Virtual_Addr'Image);
            Result := Region_Not_Free;
            return;
         end if;

         Previous_Region := Current_Region;
         Current_Region := Current_Region.all.Next_Region;
      end loop;

      Virt_Memory_Space.Memory_Map (New_Index) :=
        (Virtual_Addr  => Virtual_Address,
         Physical_Addr => Physical_Address,
         Size          => Real_Size,
         Flags         => Region_Flags,
         Next_Region   => null,
         Entry_Used    => True);

      Log_Debug
        ("Creating virtual memory mapping:"
         & ASCII.LF
         & "  Base Page Table Address: "
         & Virt_Memory_Space.Base_Page_Table_Addr'Image
         & ASCII.LF
         & "  Virtual Address: "
         & Virtual_Address'Image
         & ASCII.LF
         & "  Physical Address: "
         & Physical_Address'Image
         & ASCII.LF
         & "  Real Size: "
         & Real_Size'Image,
         Logging_Tags);

      --  Create the corresponding page table entries.
      Memory.Virtual.Paging.Map
        (Virt_Memory_Space.Base_Page_Table_Addr,
         Virtual_Address,
         Physical_Address,
         Real_Size,
         Region_Flags,
         Result);
      if Is_Error (Result) then
         return;
      end if;

      if Virt_Memory_Space.Memory_Map_List_Head = null then
         Virt_Memory_Space.Memory_Map_List_Head :=
           Virt_Memory_Space.Memory_Map (New_Index)'Unchecked_Access;
      elsif Previous_Region /= null then
         --  Previous_Region will already be pointing to the last allocated
         --  entry in the list from the loop above.
         Previous_Region.all.Next_Region :=
           Virt_Memory_Space.Memory_Map (New_Index)'Unchecked_Access;
      end if;

      Log_Debug ("Created virtual memory mapping.", Logging_Tags);

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Map_Unlocked", Logging_Tags);
         Result := Constraint_Exception;
   end Map_Unlocked;

   procedure Unmap
     (Virt_Memory_Space : in out Virtual_Memory_Space_T;
      Virt_Addr         : Virtual_Address_T;
      Result            : out Function_Result) is
   begin
      Acquire_Spinlock (Virt_Memory_Space.Spinlock);

      Memory.Virtual.Unmap_Unlocked (Virt_Memory_Space, Virt_Addr, Result);

      Release_Spinlock (Virt_Memory_Space.Spinlock);
   end Unmap;

   procedure Unmap_Unlocked
     (Virt_Memory_Space : in out Virtual_Memory_Space_T;
      Virt_Addr         : Virtual_Address_T;
      Result            : out Function_Result)
   is
      Current_Region  : Mapping_Access := null;
      Previous_Region : Mapping_Access := null;
   begin
      Log_Debug
        ("Unmapping virtual memory address " & Virt_Addr'Image, Logging_Tags);

      Current_Region := Virt_Memory_Space.Memory_Map_List_Head;

      --  Iterate through the linked list of memory regions
      --  until we find one matching the specified address.
      while Current_Region /= null loop
         if Current_Region.all.Virtual_Addr = Virt_Addr then
            Log_Debug
              ("Unmapping virtual memory region:"
               & ASCII.LF
               & "  Base Page Table Address: "
               & Virt_Memory_Space.Base_Page_Table_Addr'Image
               & ASCII.LF
               & "  Virtual Address: "
               & Virt_Addr'Image
               & ASCII.LF
               & "  Physical Address: "
               & Current_Region.all.Physical_Addr'Image
               & ASCII.LF
               & "  Real Size: "
               & Current_Region.all.Size'Image,
               Logging_Tags);

            --  Unmap the page table entries.
            Memory.Virtual.Paging.Unmap
              (Virt_Memory_Space.Base_Page_Table_Addr,
               Virt_Addr,
               Current_Region.all.Size,
               Result);
            if Is_Error (Result) then
               return;
            end if;

            if Previous_Region /= null then
               Previous_Region.all.Next_Region :=
                 Current_Region.all.Next_Region;
            else
               Virt_Memory_Space.Memory_Map_List_Head :=
                 Current_Region.all.Next_Region;
            end if;

            Current_Region.all.Entry_Used := False;
            Result := Success;
            return;
         end if;

         Previous_Region := Current_Region;
         Current_Region := Current_Region.all.Next_Region;
      end loop;

      Log_Error ("Memory block not found", Logging_Tags);
      Result := Memory_Block_Not_Found;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Unmap_Unlocked", Logging_Tags);
         Result := Constraint_Exception;
   end Unmap_Unlocked;

   function Validate_Memory_Region_Permissions
     (Region_Flags : Memory_Region_Flags_T) return Boolean is
   begin
      --  Disallow W^X pages.
      if Region_Flags.Execute and then Region_Flags.Write then
         return False;
      end if;

      return True;
   end Validate_Memory_Region_Permissions;

   procedure Map_Kernel_Memory
     (Virtual_Addr  : Virtual_Address_T;
      Physical_Addr : Physical_Address_T;
      Size          : Memory_Region_Size;
      Region_Flags  : Memory_Region_Flags_T;
      Result        : out Function_Result) is
   begin
      Kernel_Address_Space.Map
        (Virtual_Addr, Physical_Addr, Size, Region_Flags, Result);
   end Map_Kernel_Memory;

   procedure Unmap_Kernel_Memory
     (Addr : Virtual_Address_T; Result : out Function_Result) is
   begin
      Kernel_Address_Space.Unmap (Addr, Result);
   end Unmap_Kernel_Memory;

   procedure Initialise_Kernel_Address_Space is
      --  These definitions are used to get the start and end addresses
      --  of the various kernel sections defined in the linker script.
      Text_Section_Start_Marker : constant Integer
      with Import, External_Name => "__text_start";

      Text_Section_End_Marker : constant Integer
      with Import, External_Name => "__text_end";

      Rodata_Section_Start_Marker : constant Integer
      with Import, External_Name => "__rodata_start";

      Rodata_Section_End_Marker : constant Integer
      with Import, External_Name => "__rodata_end";

      Data_Section_Start_Marker : constant Integer
      with Import, External_Name => "__data_start";

      Data_Section_End_Marker : constant Integer
      with Import, External_Name => "__data_end";

      Bss_Section_Start_Marker : constant Integer
      with Import, External_Name => "__bss_start";

      Bss_Section_End_Marker : constant Integer
      with Import, External_Name => "__bss_end";

      Region_Size : Memory_Region_Size := 0;

      Heap_Region_Index : Heap_Memory_Region_Index_T :=
        Null_Memory_Region_Index;

      Pool_Region_Index : Positive := 1;
      Kernel_Page_Pool_Regions renames
        Current_System_State.Kernel_Page_Pool.Page_Pool_Regions;

      Kernel_Heap renames Current_System_State.Kernel_Heap;

      Result : Function_Result := Unset;
   begin
      Log_Debug ("Initialising kernel address space...", Logging_Tags);

      Create_New_Process_Memory_Space (Kernel_Address_Space, Result);
      if Is_Error (Result) then
         --  Error already printed.
         Panic;
      end if;

      Kernel_Address_Space.User_Address_Space := False;

      Region_Size :=
        Text_Section_End_Marker'Address - Text_Section_Start_Marker'Address;

      Map_Kernel_Memory
        (Text_Section_Start_Marker'Address,
         Get_Lower_Physical_Address (Text_Section_Start_Marker'Address),
         Region_Size,
         (True, False, True, False),
         Result);
      if Is_Error (Result) then
         --  Error already printed.
         Panic;
      end if;

      Region_Size :=
        Rodata_Section_End_Marker'Address
        - Rodata_Section_Start_Marker'Address;

      if Region_Size > 0 then
         Map_Kernel_Memory
           (Rodata_Section_Start_Marker'Address,
            Get_Lower_Physical_Address (Rodata_Section_Start_Marker'Address),
            Region_Size,
            (True, False, False, False),
            Result);
         if Is_Error (Result) then
            --  Error already printed.
            Panic;
         end if;
      end if;

      Region_Size :=
        Data_Section_End_Marker'Address - Data_Section_Start_Marker'Address;

      if Region_Size > 0 then
         Map_Kernel_Memory
           (Data_Section_Start_Marker'Address,
            Get_Lower_Physical_Address (Data_Section_Start_Marker'Address),
            Region_Size,
            (True, True, False, False),
            Result);
         if Is_Error (Result) then
            --  Error already printed.
            Panic;
         end if;
      end if;

      Region_Size :=
        Bss_Section_End_Marker'Address - Bss_Section_Start_Marker'Address;

      if Region_Size > 0 then
         Map_Kernel_Memory
           (Bss_Section_Start_Marker'Address,
            Get_Lower_Physical_Address (Bss_Section_Start_Marker'Address),
            Region_Size,
            (True, True, False, False),
            Result);
         if Is_Error (Result) then
            --  Error already printed.
            Panic;
         end if;
      end if;

      --  Map all physical memory into the kernel's address space.
      Map_Kernel_Memory
        (To_Address (Physical_Memory_Map_Address),
         Physical_Address_T (To_Address (0)),
         Physical_Memory_Map_Limit,
         (True, True, False, False),
         Result);
      if Is_Error (Result) then
         --  Error already printed.
         Panic;
      end if;

      Log_Debug ("Mapping kernel heap regions...", Logging_Tags);

      --  Map all kernel heap regions.
      Heap_Region_Index := Kernel_Heap.Memory_Regions_Head;
      while Heap_Region_Index /= Null_Memory_Region_Index loop
         Map_Kernel_Memory
           (Kernel_Heap.Memory_Regions (Heap_Region_Index).Virtual_Address,
            Kernel_Heap.Memory_Regions (Heap_Region_Index).Physical_Address,
            Kernel_Heap.Memory_Regions (Heap_Region_Index).Size,
            (True, True, False, False),
            Result);
         if Is_Error (Result) then
            --  Error already printed.
            Panic;
         end if;

         Heap_Region_Index :=
           Kernel_Heap.Memory_Regions (Heap_Region_Index).Next_Region;
      end loop;

      Log_Debug ("Mapping kernel page pool regions...", Logging_Tags);

      --  Map kernel page pool.
      while Pool_Region_Index <= Max_Page_Pool_Regions loop
         if Kernel_Page_Pool_Regions (Pool_Region_Index).Allocated then
            Log_Debug
              ("Mapping kernel page pool region..."
               & Kernel_Page_Pool_Regions (Pool_Region_Index)
                   .Virtual_Address'Image,
               Logging_Tags);

            Map_Kernel_Memory
              (Kernel_Page_Pool_Regions (Pool_Region_Index).Virtual_Address,
               Kernel_Page_Pool_Regions (Pool_Region_Index).Physical_Address,
               Page_Pool_Region_Size_In_Bytes,
               (True, True, False, False),
               Result);
            if Is_Error (Result) then
               --  Error already printed.
               Panic;
            end if;
         end if;

         Pool_Region_Index := Pool_Region_Index + 1;
      end loop;

      Log_Debug ("Initialised kernel address space.", Logging_Tags);
   exception
      when Constraint_Error =>
         Panic ("Constraint_Error: Initialise_Kernel_Address_Space");
   end Initialise_Kernel_Address_Space;

   function Get_Kernel_Address_Space_SATP return Unsigned_64 is
   begin
      return
        Create_SATP
          (Address (Kernel_Address_Space.Base_Page_Table_Addr),
           Kernel_Address_Space.Address_Space_ID);
   end Get_Kernel_Address_Space_SATP;

end Memory.Virtual;
