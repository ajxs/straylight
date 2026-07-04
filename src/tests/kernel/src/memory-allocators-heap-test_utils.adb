package body Memory.Allocators.Heap.Test_Utils is
   function Get_Block_Count_In_Heap_Memory_Region
     (Heap_Region : Heap_Memory_Region_T) return Natural
   is
      Header_Count : Natural := 0;

      Current_Block_Address : Virtual_Address_T :=
        Heap_Region.Heap_Region_Virt_Addr;
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
        Heap_Region.Heap_Region_Virt_Addr;
      Current_Block_Phys_Address : Physical_Address_T :=
        Heap_Region.Heap_Region_Phys_Addr;
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

end Memory.Allocators.Heap.Test_Utils;
