package body Memory.Allocators.Heap.Test_Utils is
   function Get_Block_Count_In_Heap_Memory_Region
     (Heap_Region : Heap_Memory_Region_T) return Natural
   is
      Header_Count : Natural := 0;

      Current_Block_Address : Virtual_Address_T :=
        Heap_Region.Heap_Region_Virt_Addr + Region_Header_Size;
   begin
      while Is_Valid_Header_Address (Heap_Region, Current_Block_Address) loop
         Current_Block : Allocation_Header_T
         with Import, Alignment => 1, Address => Current_Block_Address;

         --  If we reach an invalid heap region, raise an exception.
         if not Is_Valid_Header
                  (Current_Block.Block_Checksum,
                   Current_Block_Address,
                   Current_Block.Block_Size)
         then
            raise Program_Error with "Invalid heap region detected";
         end if;

         Header_Count := Header_Count + 1;
         --  Move to check the next block in the heap.
         Current_Block_Address :=
           Current_Block_Address + Header_Size + Current_Block.Block_Size;
      end loop;

      return Header_Count;
   end Get_Block_Count_In_Heap_Memory_Region;

   function Get_Heap_Memory_Region_Blocks
     (Heap_Region : Heap_Memory_Region_T) return Heap_Block_Info_Array_T
   is
      Current_Block_Index        : Natural := 0;
      Current_Block_Address      : Virtual_Address_T :=
        Heap_Region.Heap_Region_Virt_Addr + Region_Header_Size;
      Current_Block_Phys_Address : Physical_Address_T :=
        Heap_Region.Heap_Region_Phys_Addr + Region_Header_Size;
   begin
      Header_Count : constant Natural :=
        Get_Block_Count_In_Heap_Memory_Region (Heap_Region);

      Block_Info : Heap_Block_Info_Array_T (0 .. Header_Count - 1);

      while Is_Valid_Header_Address (Heap_Region, Current_Block_Address) loop
         Current_Block : Allocation_Header_T
         with Import, Alignment => 1, Address => Current_Block_Address;

         --  If we reach an invalid heap region, raise an exception.
         if not Is_Valid_Header
                  (Current_Block.Block_Checksum,
                   Current_Block_Address,
                   Current_Block.Block_Size)
         then
            raise Program_Error with "Invalid heap region detected";
         end if;

         Block_Info (Current_Block_Index).Block_Virt_Address :=
           Current_Block_Address;
         Block_Info (Current_Block_Index).Block_Phys_Address :=
           Current_Block_Phys_Address;
         Block_Info (Current_Block_Index).Block_Size :=
           Current_Block.Block_Size;
         Block_Info (Current_Block_Index).Block_Is_Allocated :=
           (Is_Block_Allocated
              (Current_Block.Block_Checksum,
               Current_Block_Address,
               Current_Block.Block_Size));

         Current_Block_Index := Current_Block_Index + 1;

         --  Move to check the next block in the heap.
         Current_Block_Address :=
           Current_Block_Address + Header_Size + Current_Block.Block_Size;
         Current_Block_Phys_Address :=
           Current_Block_Phys_Address + Header_Size + Current_Block.Block_Size;
      end loop;

      return Block_Info;
   end Get_Heap_Memory_Region_Blocks;

   function Get_Heap_Region_Count (Memory_Heap : Memory_Heap_T) return Natural
   is
      Region_Count : Natural := 0;

      Curr_Region : Heap_Memory_Region_Access :=
        Memory_Heap.Memory_Regions_List_Head;
   begin
      while Curr_Region /= null loop
         Region_Count := Region_Count + 1;
         Curr_Region := Curr_Region.all.Next_Region;
      end loop;

      return Region_Count;
   end Get_Heap_Region_Count;

   function Get_Heap_Region
     (Memory_Heap : Memory_Heap_T; Region_Index : Natural)
      return Heap_Memory_Region_T
   is
      Current_Index : Natural := 0;

      Curr_Region : Heap_Memory_Region_Access :=
        Memory_Heap.Memory_Regions_List_Head;
   begin
      while Curr_Region /= null loop
         if Current_Index = Region_Index then
            return Curr_Region.all;
         end if;

         Current_Index := Current_Index + 1;
         Curr_Region := Curr_Region.all.Next_Region;
      end loop;

      raise Program_Error with "Heap memory region index out of range";
   end Get_Heap_Region;

   procedure Initialise_Test_Heap
     (Memory_Heap : in out Memory_Heap_T;
      Window_Base : Virtual_Address_T;
      Window_Size : Storage_Offset) is
   begin
      Memory_Heap.Memory_Regions_List_Head := null;
      Memory_Heap.Window_Base := Window_Base;
      Memory_Heap.Window_Size := Window_Size;
   end Initialise_Test_Heap;

end Memory.Allocators.Heap.Test_Utils;
