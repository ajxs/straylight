-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

package body Boot.Early is
   procedure Allocate_New_Boot_Page_Table
     (Page_Table_Addr : out Physical_Address_T; Result : out Function_Result)
   is
      --  An array of page tables that are used to map the boot page tables.
      --  The space for the boot page tables is allocated in the linker script.
      Boot_Page_Tables : array (1 .. 16) of Page_Table_T
      with Import, External_Name => "__boot_page_tables";
   begin
      for I in Boot_Page_Table_Pool_Status'Range loop
         if Boot_Page_Table_Pool_Status (I) then
            Page_Table_Addr :=
              Physical_Address_T (Boot_Page_Tables (I)'Address);
            Boot_Page_Table_Pool_Status (I) := False;

            --  Initialise the new page table entries.
            for Table_Entry of Boot_Page_Tables (I) loop
               Table_Entry := Null_Page_Table_Entry;
            end loop;

            Early_Debug_Console_Print (Str_Allocated_Boot_Page_Table);

            Result := Success;
            return;
         end if;
      end loop;

      Result := No_Free_Entries;

      Early_Debug_Console_Print (Str_Error_No_Boot_Table_Entries);
   end Allocate_New_Boot_Page_Table;

   procedure Create_Virtual_Memory_Mapping
     (Base_Page_Table_Addr : Physical_Address_T;
      Virtual_Address      : Virtual_Address_T;
      Physical_Address     : Physical_Address_T;
      Region_Size          : RISCV_Page_Size_T;
      Region_Flags         : Memory_Region_Flags_T;
      Result               : out Function_Result)
   is
      function To_System_Address
        (Addr : Page_Aligned_Address_T) return System.Address
      is (Unsigned_64_To_Address (Shift_Left (Unsigned_64 (Addr), 12)))
      with Inline, Pure_Function;

      function To_Page_Aligned_Address
        (Addr : System.Address) return Page_Aligned_Address_T is
      begin
         return
           Page_Aligned_Address_T
             (Shift_Right (Unsigned_64 (To_Integer (Addr)), 12));
      exception
         when Constraint_Error =>
            return 0;
      end To_Page_Aligned_Address;

      Curr_Table_Addr : Physical_Address_T := Base_Page_Table_Addr;
   begin
      Test_Region_Alignment :
      declare
      begin
         Region_Boundary : constant Integer_Address :=
           Integer_Address (Early_Page_Size_In_Bytes (Region_Size));

         if not Is_Address_Aligned (Virtual_Address, Region_Boundary)
           or else not Is_Address_Aligned
                         (Address (Physical_Address), Region_Boundary)
         then
            Early_Debug_Console_Print (Str_Error_Address_Non_Aligned);

            Result := Invalid_Non_Aligned_Address;
            return;
         end if;
      end Test_Region_Alignment;

      Walk_Page_Tables :
      for Table_Level in reverse 0 .. 2 loop
         Read_Table_Entry :
         declare
            --  The current level page table.
            Page_Table : Page_Table_T
            with Import, Alignment => 1, Address => Address (Curr_Table_Addr);

            --  The index into the current level page table.
            Table_Idx      : Page_Table_Index_T := 0;
            --  Address of newly created tables, if needed.
            New_Table_Addr : Physical_Address_T := Null_Physical_Address;
         begin
            Table_Idx :=
              To_Virtual_Address (Virtual_Address).VPN (Table_Level);

            --  If this region is already mapped, then return.
            if Page_Table (Table_Idx).R
              or else Page_Table (Table_Idx).W
              or else Page_Table (Table_Idx).X
            then
               Early_Debug_Console_Print (Str_Error_Region_Not_Free);

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

               Result := Success;
               return;
            end if;

            --  If the entry at this level isn't a leaf entry, and
            --  we're not at the final level for this region size, check
            --  if we need to physically allocate the next table.
            if not Page_Table (Table_Idx).V then
               Allocate_New_Boot_Page_Table (New_Table_Addr, Result);
               if Is_Error (Result) then
                  return;
               end if;

               --  Set up the newly allocated page table at this level.
               Page_Table (Table_Idx).V := True;
               Page_Table (Table_Idx).PPN :=
                 To_Page_Aligned_Address (Address (New_Table_Addr));
            end if;

            Curr_Table_Addr :=
              Physical_Address_T
                (To_System_Address (Page_Table (Table_Idx).PPN));

         end Read_Table_Entry;
      end loop Walk_Page_Tables;

      --  This point should not be reached, consider it an unhandled
      --  exceptional scenario.
      Result := Unhandled_Exception;
   exception
      when Constraint_Error =>
         Early_Debug_Console_Print (Str_Error_Unhandled);

         Result := Constraint_Exception;
   end Create_Virtual_Memory_Mapping;

   procedure Early_Debug_Console_Print (Str : String) is
      procedure Debug_Console_Print_Internal
        (String_Length : Unsigned_64; String_Addr : Address)
      with
        Import,
        Convention    => Assembler,
        External_Name => "boot_early_debug_console_print";
   begin
      Debug_Console_Print_Internal (Unsigned_64 (Str'Length), Str'Address);
   exception
      when Constraint_Error =>
         null;
   end Early_Debug_Console_Print;

   function Get_Largest_Page_Size_For_Remaining_Region
     (Virtual_Address  : Virtual_Address_T;
      Physical_Address : Physical_Address_T;
      Remaining_Size   : Storage_Offset) return RISCV_Page_Size_T is
   begin
      --  Ensure the virtual and physical addresses are properly page aligned.
      --  Section 4.4.1 of the privileged spec specifies that 'megapages',
      --  and 'gigapages' must be virtually and physically aligned to
      --  their boundary.
      if Remaining_Size >= Huge_Page_Size
        and then Is_Address_Aligned (Virtual_Address, Huge_Page_Size)
        and then Is_Address_Aligned
                   (Address (Physical_Address), Huge_Page_Size)
      then
         return Huge;
      elsif Remaining_Size >= Large_Page_Size
        and then Is_Address_Aligned (Virtual_Address, Large_Page_Size)
        and then Is_Address_Aligned
                   (Address (Physical_Address), Large_Page_Size)
      then
         return Large;
      end if;

      return Small;
   end Get_Largest_Page_Size_For_Remaining_Region;

   function Initialise_Boot_Page_Tables return Unsigned_64 is
      Boot_Text_Section_Start_Marker : constant Integer
      with Import, External_Name => "__boot_text_start";

      Boot_Text_Section_End_Marker : constant Integer
      with Import, External_Name => "__boot_text_end";

      Boot_Data_Section_Start_Marker : constant Integer
      with Import, External_Name => "__boot_data_start";

      Boot_Data_Section_End_Marker : constant Integer
      with Import, External_Name => "__boot_data_end";

      Boot_Rodata_Section_Start_Marker : constant Integer
      with Import, External_Name => "__boot_rodata_start";

      Boot_Rodata_Section_End_Marker : constant Integer
      with Import, External_Name => "__boot_rodata_end";

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

      --  All harts require an initial boot stack to use prior to jumping into
      --  the 'higher-half' kernel address space. The stacks for each hart are
      --  statically allocated in the linker script in one block, and mapped
      --  altogether at a fixed address.
      --  These are loaded by each hart in the boot-entry code, e.g.
      --  SP := __boot_stacks_bottom +
      --    (Hart_ID * BOOT_STACK_SIZE) + BOOT_STACK_SIZE
      Boot_Stacks_Start_Marker : constant Integer
      with Import, External_Name => "__boot_stacks_bottom";

      Boot_Stacks_End_Marker : constant Integer
      with Import, External_Name => "__boot_stacks_top";

      --  This is the address at which the kernel's boot stacks are mapped into
      --  the kernel's higher half address space.
      Kernel_Boot_Stacks_Base_Virtual_Address : constant Address :=
        To_Address (16#FFFF_FFFF_FF00_0000#);

      Boot_Stacks_Size : Storage_Offset := 0;

      Result : Function_Result := Unset;
   begin
      Early_Debug_Console_Print (Str_Initialising_Page_Tables);

      --  Allocate the root boot page table. This will be reused by other
      --  harts when they're initialised.
      Allocate_New_Boot_Page_Table (Boot_Root_Page_Table_Addr, Result);
      --  In the case this was not successful, the error message will
      --  have already been printed by the function.
      if Is_Error (Result) then
         return 0;
      end if;

      Map_Boot_Memory_Section
        (Boot_Root_Page_Table_Addr,
         Boot_Text_Section_Start_Marker'Address,
         Boot_Text_Section_End_Marker'Address,
         Physical_Address_T (Boot_Text_Section_Start_Marker'Address),
         (True, False, True, False),
         Result);
      if Is_Error (Result) then
         return 0;
      end if;

      Map_Boot_Memory_Section
        (Boot_Root_Page_Table_Addr,
         Boot_Data_Section_Start_Marker'Address,
         Boot_Data_Section_End_Marker'Address,
         Physical_Address_T (Boot_Data_Section_Start_Marker'Address),
         (True, True, False, False),
         Result);
      if Is_Error (Result) then
         return 0;
      end if;

      Map_Boot_Memory_Section
        (Boot_Root_Page_Table_Addr,
         Boot_Rodata_Section_Start_Marker'Address,
         Boot_Rodata_Section_End_Marker'Address,
         Physical_Address_T (Boot_Rodata_Section_Start_Marker'Address),
         (True, False, False, False),
         Result);
      if Is_Error (Result) then
         return 0;
      end if;

      Map_Boot_Memory_Section
        (Boot_Root_Page_Table_Addr,
         Text_Section_Start_Marker'Address,
         Text_Section_End_Marker'Address,
         Physical_Address_T
           (Text_Section_Start_Marker'Address - Higher_Half_Offset),
         (True, False, True, False),
         Result);
      if Is_Error (Result) then
         return 0;
      end if;

      if Rodata_Section_End_Marker'Address
        > Rodata_Section_Start_Marker'Address
      then
         Map_Boot_Memory_Section
           (Boot_Root_Page_Table_Addr,
            Rodata_Section_Start_Marker'Address,
            Rodata_Section_End_Marker'Address,
            Physical_Address_T
              (Rodata_Section_Start_Marker'Address - Higher_Half_Offset),
            (True, False, False, False),
            Result);
         if Is_Error (Result) then
            return 0;
         end if;

      end if;

      if Data_Section_End_Marker'Address > Data_Section_Start_Marker'Address
      then
         Map_Boot_Memory_Section
           (Boot_Root_Page_Table_Addr,
            Data_Section_Start_Marker'Address,
            Data_Section_End_Marker'Address,
            Physical_Address_T
              (Data_Section_Start_Marker'Address - Higher_Half_Offset),
            (True, True, False, False),
            Result);
         if Is_Error (Result) then
            return 0;
         end if;

      end if;

      if Bss_Section_End_Marker'Address > Bss_Section_Start_Marker'Address then
         Map_Boot_Memory_Section
           (Boot_Root_Page_Table_Addr,
            Bss_Section_Start_Marker'Address,
            Bss_Section_End_Marker'Address,
            Physical_Address_T
              (Bss_Section_Start_Marker'Address - Higher_Half_Offset),
            (True, True, False, False),
            Result);
         if Is_Error (Result) then
            return 0;
         end if;

      end if;

      --  Map all physical memory into the kernel's address space.
      Map_Boot_Memory_Section
        (Boot_Root_Page_Table_Addr,
         To_Address (Physical_Memory_Map_Address),
         To_Address (Physical_Memory_Map_Address)
         + Memory.Physical_Memory_Map_Limit,
         Physical_Address_T (To_Address (0)),
         (True, True, False, False),
         Result);
      if Is_Error (Result) then
         return 0;
      end if;

      --  Map the boot stacks into the higher-half boot memory space.
      Boot_Stacks_Size :=
        Boot_Stacks_End_Marker'Address - Boot_Stacks_Start_Marker'Address;

      Map_Boot_Memory_Section
        (Boot_Root_Page_Table_Addr,
         Kernel_Boot_Stacks_Base_Virtual_Address,
         Kernel_Boot_Stacks_Base_Virtual_Address + Boot_Stacks_Size,
         Physical_Address_T (Boot_Stacks_Start_Marker'Address),
         (True, True, False, False),
         Result);
      if Is_Error (Result) then
         return 0;
      end if;

      return RISCV.Create_SATP (Address (Boot_Root_Page_Table_Addr), 0);
   exception
      when Constraint_Error =>
         return 0;
   end Initialise_Boot_Page_Tables;

   procedure Map_Boot_Memory_Section
     (Base_Page_Table_Addr  : Physical_Address_T;
      Virtual_Address_Start : Virtual_Address_T;
      Virtual_Address_End   : Virtual_Address_T;
      Base_Physical_Address : Physical_Address_T;
      Region_Flags          : Memory_Region_Flags_T;
      Result                : out Function_Result)
   is
      Curr_Addr_Virt : Virtual_Address_T := Virtual_Address_Start;
      Curr_Addr_Phys : Physical_Address_T := Base_Physical_Address;

      Next_Region_Size          : RISCV_Page_Size_T := Huge;
      Next_Region_Size_In_Bytes : Storage_Offset := 0;
   begin
      while Curr_Addr_Virt < Virtual_Address_End loop
         Next_Region_Size :=
           Get_Largest_Page_Size_For_Remaining_Region
             (Curr_Addr_Virt,
              Curr_Addr_Phys,
              Virtual_Address_End - Curr_Addr_Virt);

         Create_Virtual_Memory_Mapping
           (Base_Page_Table_Addr,
            Curr_Addr_Virt,
            Curr_Addr_Phys,
            Next_Region_Size,
            Region_Flags,
            Result);
         if Is_Error (Result) then
            return;
         end if;

         Next_Region_Size_In_Bytes :=
           Early_Page_Size_In_Bytes (Next_Region_Size);

         Curr_Addr_Virt := Curr_Addr_Virt + Next_Region_Size_In_Bytes;
         Curr_Addr_Phys := Curr_Addr_Phys + Next_Region_Size_In_Bytes;

      end loop;
   exception
      when Constraint_Error =>
         Early_Debug_Console_Print (Str_Error_Unhandled);

         Result := Constraint_Exception;
   end Map_Boot_Memory_Section;
end Boot.Early;
