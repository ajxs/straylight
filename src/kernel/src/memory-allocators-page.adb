-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

package body Memory.Allocators.Page is
   procedure Add_Region_To_Page_Pool
     (Page_Pool        : in out Page_Pool_T;
      Virtual_Address  : Virtual_Address_T;
      Physical_Address : Physical_Address_T;
      Page_Count       : Positive;
      Result           : out Function_Result)
   is
      Regions renames Page_Pool.Page_Pool_Regions;
   begin
      if Page_Count > Max_Page_Pool_Region_Size then
         Result := Invalid_Argument;
         return;
      end if;

      for Curr_Region in Regions'Range loop
         if not Regions (Curr_Region).Allocated then
            Regions (Curr_Region) :=
              (Virtual_Address  => Virtual_Address,
               Physical_Address => Physical_Address,
               Page_Statuses    => [others => Free],
               Page_Count       => Page_Count,
               Allocated        => True);

            Page_Pool.Free_Page_Count :=
              Page_Pool.Free_Page_Count + Page_Count;

            Result := Success;
            return;
         end if;
      end loop;

      Result := Region_Array_Exhausted;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Add_Region_To_Page_Pool");
         Result := Constraint_Exception;
   end Add_Region_To_Page_Pool;

   procedure Allocate_Unlocked
     (Page_Pool         : in out Page_Pool_T;
      Page_Count        : Positive;
      Allocation_Result : out Memory_Allocation_Result;
      Result            : out Function_Result)
   is
      Regions renames Page_Pool.Page_Pool_Regions;

      Contiguous_Free_Count : Natural := 0;
   begin
      for Curr_Region in Regions'Range loop
         Contiguous_Free_Count := 0;

         if Regions (Curr_Region).Allocated then
            for Curr_Page in 1 .. Regions (Curr_Region).Page_Count loop
               if Regions (Curr_Region).Page_Statuses (Curr_Page) = Free then
                  Contiguous_Free_Count := Contiguous_Free_Count + 1;
               else
                  Contiguous_Free_Count := 0;
               end if;

               if Contiguous_Free_Count = Page_Count then
                  Run_First_Page : constant Positive :=
                    Curr_Page - Page_Count + 1;

                  --  Mark the allocated run: the first page identifies the
                  --  start of the allocation, so that Free can recover the
                  --  run's length from its address alone.
                  Regions (Curr_Region).Page_Statuses (Run_First_Page) :=
                    Run_Start;

                  for K in Run_First_Page + 1 .. Curr_Page loop
                     Regions (Curr_Region).Page_Statuses (K) :=
                       Run_Continuation;
                  end loop;

                  Page_Pool.Free_Page_Count :=
                    Page_Pool.Free_Page_Count - Page_Count;

                  Offset_Within_Region : constant Storage_Offset :=
                    Storage_Offset
                      ((Run_First_Page - 1) * Page_Pool_Page_Size);

                  Allocation_Result.Virtual_Address :=
                    Regions (Curr_Region).Virtual_Address
                    + Offset_Within_Region;

                  Allocation_Result.Physical_Address :=
                    Regions (Curr_Region).Physical_Address
                    + Offset_Within_Region;

                  Log_Debug
                    ("Memory.Allocators.Page: Allocated "
                     & Page_Count'Image
                     & " pages at VAddr: "
                     & Allocation_Result.Virtual_Address'Image
                     & ", PAddr: "
                     & Allocation_Result.Physical_Address'Image,
                     Logging_Tags_Page_Pool);

                  Result := Success;
                  return;
               end if;
            end loop;
         end if;
      end loop;

      Result := Not_Enough_Memory_Available;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Allocate_Unlocked");
         Result := Constraint_Exception;
   end Allocate_Unlocked;

   procedure Allocate
     (Page_Pool         : in out Page_Pool_T;
      Page_Count        : Positive;
      Allocation_Result : out Memory_Allocation_Result;
      Result            : out Function_Result) is
   begin
      Acquire_Spinlock (Page_Pool.Spinlock);

      Allocate_Unlocked (Page_Pool, Page_Count, Allocation_Result, Result);

      Release_Spinlock (Page_Pool.Spinlock);
   end Allocate;

   procedure Free_Unlocked
     (Page_Pool       : in out Page_Pool_T;
      Virtual_Address : Virtual_Address_T;
      Result          : out Function_Result)
   is
      Regions renames Page_Pool.Page_Pool_Regions;
   begin
      --  If the address isn't page-aligned, we automatically know it isn't the
      --  start of a page pool allocation.
      if not Is_Address_Page_Aligned (Virtual_Address) then
         Result := Invalid_Argument;
         return;
      end if;

      for Curr_Region in Regions'Range loop
         if Regions (Curr_Region).Allocated
           and then
             Is_Virtual_Address_In_Region
               (Regions (Curr_Region), Virtual_Address)
         then
            --  The index within the region can be calculated by counting
            --  the number of pages that the address is offset from the
            --  region's beginning.
            First_Page : constant Positive :=
              (Integer
                 (Virtual_Address - Regions (Curr_Region).Virtual_Address)
               / Page_Pool_Page_Size)
              + 1;

            --  Only the first page of an allocated run can be freed.
            --  This rejects double-frees and addresses inside a run.
            if Regions (Curr_Region).Page_Statuses (First_Page) /= Run_Start
            then
               Log_Error ("Free: Address is not the start of an allocation");
               Result := Invalid_Argument;
               return;
            end if;

            Regions (Curr_Region).Page_Statuses (First_Page) := Free;

            Freed_Page_Count : Positive := 1;

            --  Free the remainder of the run.
            for Curr_Page in First_Page + 1 .. Regions (Curr_Region).Page_Count
            loop
               exit when
                 Regions (Curr_Region).Page_Statuses (Curr_Page)
                 /= Run_Continuation;

               Regions (Curr_Region).Page_Statuses (Curr_Page) := Free;
               Freed_Page_Count := Freed_Page_Count + 1;
            end loop;

            Page_Pool.Free_Page_Count :=
              Page_Pool.Free_Page_Count + Freed_Page_Count;

            Result := Success;
            return;
         end if;
      end loop;

      Log_Error ("Free: Virtual address not found in any allocated region");
      Result := Invalid_Argument;

   exception
      when Constraint_Error =>
         Result := Constraint_Exception;
   end Free_Unlocked;

   procedure Free
     (Page_Pool       : in out Page_Pool_T;
      Virtual_Address : Virtual_Address_T;
      Result          : out Function_Result) is
   begin
      Acquire_Spinlock (Page_Pool.Spinlock);

      Free_Unlocked (Page_Pool, Virtual_Address, Result);

      Release_Spinlock (Page_Pool.Spinlock);
   end Free;

   procedure Add_Region_To_Page_Pool_And_Allocate
     (Page_Pool             : in out Page_Pool_T;
      Virtual_Address       : Virtual_Address_T;
      Physical_Address      : Physical_Address_T;
      New_Page_Count        : Positive;
      Allocation_Page_Count : Positive;
      Allocation_Result     : out Memory_Allocation_Result;
      Result                : out Function_Result) is
   begin
      Acquire_Spinlock (Page_Pool.Spinlock);

      Add_Region_To_Page_Pool
        (Page_Pool, Virtual_Address, Physical_Address, New_Page_Count, Result);
      if Is_Error (Result) then
         Release_Spinlock (Page_Pool.Spinlock);
         return;
      end if;

      Allocate_Unlocked
        (Page_Pool, Allocation_Page_Count, Allocation_Result, Result);

      Release_Spinlock (Page_Pool.Spinlock);
   end Add_Region_To_Page_Pool_And_Allocate;

end Memory.Allocators.Page;
