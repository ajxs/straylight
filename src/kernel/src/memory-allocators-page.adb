-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Logging; use Logging;

package body Memory.Allocators.Page is
   procedure Add_Region_To_Page_Pool
     (Page_Pool        : in out Page_Pool_T;
      Virtual_Address  : Virtual_Address_T;
      Physical_Address : Physical_Address_T;
      Result           : out Function_Result)
   is
      Regions renames Page_Pool.Page_Pool_Regions;
   begin
      for Curr_Region in Regions'Range loop
         if not Regions (Curr_Region).Allocated then
            Regions (Curr_Region).Virtual_Address := Virtual_Address;
            Regions (Curr_Region).Physical_Address := Physical_Address;
            Regions (Curr_Region).Allocated := True;
            Result := Success;
            return;
         end if;
      end loop;

      Result := Region_Array_Exhausted;
   end Add_Region_To_Page_Pool;

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
         if Regions (Curr_Region).Allocated then
            for Curr_Page in Regions (Curr_Region).Page_Statuses'Range loop
               if Regions (Curr_Region).Page_Statuses (Curr_Page) = Free then
                  Contiguous_Free_Count := Contiguous_Free_Count + 1;
               else
                  Contiguous_Free_Count := 0;
               end if;

               if Contiguous_Free_Count = Page_Count then
                  Regions (Curr_Region).Page_Statuses (Curr_Page) := Allocated;

                  Offset_Within_Region : constant Storage_Offset :=
                    Storage_Offset ((Curr_Page - 1) * 16#1000#);

                  Allocation_Result.Virtual_Address :=
                    Regions (Curr_Region).Virtual_Address
                    + Offset_Within_Region;

                  Allocation_Result.Physical_Address :=
                    Regions (Curr_Region).Physical_Address
                    + Offset_Within_Region;

                  Result := Success;
                  return;
               end if;
            end loop;
         end if;
      end loop;

      Result := No_Free_Entries;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Allocate_Unlocked");
         Result := Constraint_Exception;
   end Allocate_Unlocked;

   procedure Free
     (Page_Pool       : in out Page_Pool_T;
      Page_Count      : Positive;
      Virtual_Address : Virtual_Address_T;
      Result          : out Function_Result) is
   begin
      Acquire_Spinlock (Page_Pool.Spinlock);

      Free_Unlocked (Page_Pool, Page_Count, Virtual_Address, Result);

      Release_Spinlock (Page_Pool.Spinlock);
   end Free;

   procedure Free_Unlocked
     (Page_Pool       : in out Page_Pool_T;
      Page_Count      : Positive;
      Virtual_Address : Virtual_Address_T;
      Result          : out Function_Result)
   is
      Regions renames Page_Pool.Page_Pool_Regions;
   begin
      for Curr_Region in Regions'Range loop
         if Regions (Curr_Region).Allocated
           and then Is_Virtual_Address_In_Region
                      (Regions (Curr_Region), Virtual_Address)
         then
            --  The index within the region can be calculated by counting
            --  the number of pages that the address is offset from the
            --  region's beginning.
            Region_Index : constant Natural :=
              Integer (Virtual_Address - Regions (Curr_Region).Virtual_Address)
              / 16#1000#;

            --  If the address has been found in an allocated region,
            --  but the start of the address + the count of pages extends
            --  beyond the edge of the region, then return an error.
            if (Region_Index + Page_Count) > Page_Pool_Region_Size then
               Result := Invalid_Address_Argument;
               return;
            end if;

            for K in 1 .. Page_Count loop
               Regions (Curr_Region).Page_Statuses (Region_Index + K) := Free;
               Result := Success;
            end loop;

            return;
         end if;
      end loop;

      Result := Invalid_Address_Argument;

   exception
      when Constraint_Error =>
         Result := Constraint_Exception;
   end Free_Unlocked;

end Memory.Allocators.Page;
