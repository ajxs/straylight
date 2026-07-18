with AUnit.Assertions; use AUnit.Assertions;

package body Memory.Allocators.Page.Test_Cases is
   --  The page pool performs no accesses to the memory it manages, so the
   --  regions used in these tests are backed by fabricated addresses.
   Test_Region_Virt_Address : constant Virtual_Address_T :=
     To_Address (16#4000_0000#);

   Test_Region_Phys_Address : constant Physical_Address_T :=
     Physical_Address_T (To_Address (16#8000_0000#));

   function Page_Offset (Page_Number : Natural) return Storage_Offset
   is (Storage_Offset (Page_Number) * Page_Pool_Page_Size);

   overriding
   procedure Run_Test (T : in out Test_Add_Region) is
      Test_Pool : Page_Pool_T;

      Result : Function_Result := Unset;
   begin
      Add_Region_To_Page_Pool
        (Test_Pool,
         Test_Region_Virt_Address,
         Test_Region_Phys_Address,
         8,
         Result);
      Assert (Result = Success, "Region added successfully");
      Assert
        (Test_Pool.Free_Page_Count = 8,
         "Free page count matches added region size");
      Assert
        (Test_Pool.Page_Pool_Regions (1).Allocated,
         "First region slot is marked as allocated");
      Assert
        (Test_Pool.Page_Pool_Regions (1).Page_Count = 8,
         "Region page count is recorded");

      Add_Region_To_Page_Pool
        (Test_Pool,
         Test_Region_Virt_Address,
         Test_Region_Phys_Address,
         Max_Page_Pool_Region_Size + 1,
         Result);
      Assert (Result = Invalid_Argument, "Oversized region is rejected");

      --  Fill the remaining region slots, then verify that adding a
      --  further region fails.
      for I in 2 .. Max_Page_Pool_Regions loop
         Add_Region_To_Page_Pool
           (Test_Pool,
            Test_Region_Virt_Address + Page_Offset (I * 8),
            Test_Region_Phys_Address + Page_Offset (I * 8),
            8,
            Result);
         Assert (Result = Success, "Region added successfully");
      end loop;

      Add_Region_To_Page_Pool
        (Test_Pool,
         Test_Region_Virt_Address,
         Test_Region_Phys_Address,
         8,
         Result);
      Assert
        (Result = Region_Array_Exhausted,
         "Adding a region to a full pool fails");
   end Run_Test;

   overriding
   procedure Run_Test (T : in out Test_Allocate) is
      Test_Pool : Page_Pool_T;

      Result : Function_Result := Unset;

      Allocation_Result : Memory_Allocation_Result :=
        (Virtual_Address  => Null_Address,
         Physical_Address => Null_Physical_Address);
   begin
      Add_Region_To_Page_Pool
        (Test_Pool,
         Test_Region_Virt_Address,
         Test_Region_Phys_Address,
         8,
         Result);
      Assert (Result = Success, "Region added successfully");

      Allocate (Test_Pool, 1, Allocation_Result, Result);
      Assert (Result = Success, "Single page allocation succeeded");
      Assert
        (Allocation_Result.Virtual_Address = Test_Region_Virt_Address,
         "First allocation is at the region's base virtual address");
      Assert
        (Allocation_Result.Physical_Address = Test_Region_Phys_Address,
         "First allocation is at the region's base physical address");
      Assert
        (Test_Pool.Page_Pool_Regions (1).Page_Statuses (1) = Run_Start,
         "Single page allocation is marked as a run start");
      Assert (Test_Pool.Free_Page_Count = 7, "Free page count is decremented");

      Allocate (Test_Pool, 3, Allocation_Result, Result);
      Assert (Result = Success, "Multi-page allocation succeeded");
      Assert
        (Allocation_Result.Virtual_Address
         = Test_Region_Virt_Address + Page_Offset (1),
         "Second allocation follows the first");
      Assert
        (Allocation_Result.Physical_Address
         = Test_Region_Phys_Address + Page_Offset (1),
         "Second allocation's physical address follows the first");
      Assert
        (Test_Pool.Page_Pool_Regions (1).Page_Statuses (2) = Run_Start,
         "Multi-page allocation starts with a run start");
      Assert
        (Test_Pool.Page_Pool_Regions (1).Page_Statuses (3) = Run_Continuation
         and then
           Test_Pool.Page_Pool_Regions (1).Page_Statuses (4)
           = Run_Continuation,
         "Multi-page allocation continues with run continuations");
      Assert
        (Test_Pool.Page_Pool_Regions (1).Page_Statuses (5) = Free,
         "Pages after the allocated run remain free");
      Assert
        (Test_Pool.Free_Page_Count = 4,
         "Free page count reflects both allocations");

      Allocate (Test_Pool, 5, Allocation_Result, Result);
      Assert
        (Result = Not_Enough_Memory_Available,
         "Allocation larger than the remaining free pages fails");

      Allocate (Test_Pool, 4, Allocation_Result, Result);
      Assert (Result = Success, "Allocation of all remaining pages succeeds");
      Assert (Test_Pool.Free_Page_Count = 0, "Free page count reaches zero");
   end Run_Test;

   overriding
   procedure Run_Test (T : in out Test_Allocate_Multiple_Regions) is
      Test_Pool : Page_Pool_T;

      Result : Function_Result := Unset;

      Allocation_Result : Memory_Allocation_Result :=
        (Virtual_Address  => Null_Address,
         Physical_Address => Null_Physical_Address);
   begin
      --  Two virtually adjacent regions of four pages each.
      --  Allocations must never span a region boundary, even when the
      --  regions are adjacent in virtual memory.
      Add_Region_To_Page_Pool
        (Test_Pool,
         Test_Region_Virt_Address,
         Test_Region_Phys_Address,
         4,
         Result);
      Assert (Result = Success, "First region added successfully");

      Add_Region_To_Page_Pool
        (Test_Pool,
         Test_Region_Virt_Address + Page_Offset (4),
         Test_Region_Phys_Address + Page_Offset (4),
         4,
         Result);
      Assert (Result = Success, "Second region added successfully");

      Allocate (Test_Pool, 6, Allocation_Result, Result);
      Assert
        (Result = Not_Enough_Memory_Available,
         "Allocation spanning adjacent regions is not permitted");

      Allocate (Test_Pool, 2, Allocation_Result, Result);
      Assert (Result = Success, "Allocation within first region succeeded");
      Assert
        (Allocation_Result.Virtual_Address = Test_Region_Virt_Address,
         "Allocation is placed in the first region");

      Allocate (Test_Pool, 3, Allocation_Result, Result);
      Assert
        (Result = Success,
         "Allocation too large for the remainder of "
         & "the first region succeeded");
      Assert
        (Allocation_Result.Virtual_Address
         = Test_Region_Virt_Address + Page_Offset (4),
         "Allocation falls through to the second region");
      Assert
        (Allocation_Result.Physical_Address
         = Test_Region_Phys_Address + Page_Offset (4),
         "Allocation uses the second region's physical address");
   end Run_Test;

   overriding
   procedure Run_Test (T : in out Test_Free) is
      Test_Pool : Page_Pool_T;

      Result : Function_Result := Unset;

      Allocation_Result : Memory_Allocation_Result :=
        (Virtual_Address  => Null_Address,
         Physical_Address => Null_Physical_Address);

      First_Allocation_Address : Virtual_Address_T := Null_Address;
   begin
      Add_Region_To_Page_Pool
        (Test_Pool,
         Test_Region_Virt_Address,
         Test_Region_Phys_Address,
         8,
         Result);
      Assert (Result = Success, "Region added successfully");

      Allocate (Test_Pool, 3, Allocation_Result, Result);
      Assert (Result = Success, "Allocation succeeded");
      First_Allocation_Address := Allocation_Result.Virtual_Address;

      Allocate (Test_Pool, 2, Allocation_Result, Result);
      Assert (Result = Success, "Second allocation succeeded");
      Assert (Test_Pool.Free_Page_Count = 3, "Free page count is correct");

      --  The length of the run is recovered from the page statuses:
      --  no page count is passed to Free.
      Free (Test_Pool, First_Allocation_Address, Result);
      Assert (Result = Success, "Free succeeded");
      Assert
        (Test_Pool.Free_Page_Count = 6, "All pages in the run were freed");
      Assert
        (Test_Pool.Page_Pool_Regions (1).Page_Statuses (1) = Free
         and then Test_Pool.Page_Pool_Regions (1).Page_Statuses (2) = Free
         and then Test_Pool.Page_Pool_Regions (1).Page_Statuses (3) = Free,
         "Freed run's pages are marked as free");
      Assert
        (Test_Pool.Page_Pool_Regions (1).Page_Statuses (4) = Run_Start,
         "Following allocation is unaffected by the free");

      --  The freed run should be reused by the next allocation.
      Allocate (Test_Pool, 3, Allocation_Result, Result);
      Assert (Result = Success, "Allocation after free succeeded");
      Assert
        (Allocation_Result.Virtual_Address = First_Allocation_Address,
         "Freed run is reused by a subsequent allocation");
   end Run_Test;

   overriding
   procedure Run_Test (T : in out Test_Free_Invalid) is
      Test_Pool : Page_Pool_T;

      Result : Function_Result := Unset;

      Allocation_Result : Memory_Allocation_Result :=
        (Virtual_Address  => Null_Address,
         Physical_Address => Null_Physical_Address);
   begin
      Add_Region_To_Page_Pool
        (Test_Pool,
         Test_Region_Virt_Address,
         Test_Region_Phys_Address,
         8,
         Result);
      Assert (Result = Success, "Region added successfully");

      Allocate (Test_Pool, 3, Allocation_Result, Result);
      Assert (Result = Success, "Allocation succeeded");

      Free (Test_Pool, To_Address (16#5000_0000#), Result);
      Assert
        (Result = Invalid_Argument,
         "Freeing an address outside any region fails");

      Free (Test_Pool, Test_Region_Virt_Address + 1, Result);
      Assert
        (Result = Invalid_Argument,
         "Freeing an address which is not page aligned fails");

      Free (Test_Pool, Test_Region_Virt_Address + Page_Offset (1), Result);
      Assert
        (Result = Invalid_Argument, "Freeing an address inside a run fails");

      Free (Test_Pool, Test_Region_Virt_Address + Page_Offset (4), Result);
      Assert
        (Result = Invalid_Argument,
         "Freeing an address which was never allocated fails");

      Free (Test_Pool, Test_Region_Virt_Address, Result);
      Assert (Result = Success, "Freeing the allocated run succeeds");

      Free (Test_Pool, Test_Region_Virt_Address, Result);
      Assert (Result = Invalid_Argument, "Double free fails");

      Assert
        (Test_Pool.Free_Page_Count = 8,
         "Free page count is unaffected by rejected frees");
   end Run_Test;

end Memory.Allocators.Page.Test_Cases;
