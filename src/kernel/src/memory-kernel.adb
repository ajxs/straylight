with Memory.Physical; use Memory.Physical;
with Memory.Virtual;  use Memory.Virtual;
with System_State;    use System_State;

package body Memory.Kernel is
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
      Result            : out Function_Result) is
   begin
      Kernel_Page_Pool.Allocate (Number_of_Pages, Allocation_Result, Result);
   end Allocate_Pages;

   procedure Free_Kernel_Memory
     (Allocated_Virtual_Address : Virtual_Address_T;
      Result                    : out Function_Result) is
   begin
      Kernel_Heap.Free (Allocated_Virtual_Address, Result);
   end Free_Kernel_Memory;

   procedure Free_Pages
     (Virtual_Address : Virtual_Address_T;
      Page_Count      : Positive;
      Result          : out Function_Result) is
   begin
      Kernel_Page_Pool.Free (Page_Count, Virtual_Address, Result);
   end Free_Pages;

   procedure Initialise_Kernel_Heap is
      Allocated_Physical_Address : Physical_Address_T := Null_Physical_Address;

      Heap_Region_Index : Heap_Memory_Region_Index_T :=
        Null_Memory_Region_Index;

      Result : Function_Result := Unset;
   begin
      Allocate_Physical_Memory
        (Kernel_Heap_Initial_Size, Allocated_Physical_Address, Result);
      if Is_Error (Result) then
         --  Error already printed.
         Panic;
      end if;

      Kernel_Heap.Add_Memory_Region_To_Heap
        (Kernel_Heap_Virtual_Address,
         Allocated_Physical_Address,
         Storage_Offset (Kernel_Heap_Initial_Size),
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

      Log_Debug ("Initialised kernel heap.", Logging_Tags);
   exception
      when Constraint_Error =>
         Panic ("Constraint_Error: Initialise_Kernel_Heap");
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
      end loop;

      Log_Debug ("Initialised kernel page pool.", Logging_Tags);
   exception
      when Constraint_Error =>
         Panic ("Constraint_Error: Initialise_Kernel_Page_Pool");
   end Initialise_Kernel_Page_Pool;

end Memory.Kernel;
