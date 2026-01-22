-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Function_Results; use Function_Results;
with Locks;            use Locks;
with RISCV.Paging;     use RISCV.Paging;

package Memory.Allocators.Page is
   pragma Preelaborate;

   Page_Pool_Region_Size : constant := 1024;

   Page_Pool_Region_Size_In_Bytes : constant :=
     Page_Pool_Region_Size * Small_Page_Size;

   type Page_Status_T is (Free, Allocated) with Size => 1;

   type Page_Status_Array_T is
     array (1 .. Page_Pool_Region_Size) of Page_Status_T
   with Pack;

   type Page_Pool_Region_T is record
      Virtual_Address  : Virtual_Address_T;
      Physical_Address : Physical_Address_T;
      Page_Statuses    : Page_Status_Array_T;
      Allocated        : Boolean := False;
   end record;

   Max_Page_Pool_Regions : constant := 1024;

   type Page_Pool_Region_Array_T is
     array (1 .. Max_Page_Pool_Regions) of Page_Pool_Region_T;

   type Page_Pool_T is record
      Page_Pool_Regions : Page_Pool_Region_Array_T;
      Spinlock          : Spinlock_T;
   end record;

   procedure Allocate
     (Page_Pool         : in out Page_Pool_T;
      Page_Count        : Positive;
      Allocation_Result : out Memory_Allocation_Result;
      Result            : out Function_Result);

   procedure Add_Region_To_Page_Pool
     (Page_Pool        : in out Page_Pool_T;
      Virtual_Address  : Virtual_Address_T;
      Physical_Address : Physical_Address_T;
      Result           : out Function_Result);

   procedure Free
     (Page_Pool       : in out Page_Pool_T;
      Page_Count      : Positive;
      Virtual_Address : Virtual_Address_T;
      Result          : out Function_Result);

   function Is_Virtual_Address_In_Region
     (Region : Page_Pool_Region_T; Virtual_Address : Virtual_Address_T)
      return Boolean
   is (Virtual_Address >= Region.Virtual_Address
       and then
         Virtual_Address
         < Region.Virtual_Address + (Page_Pool_Region_Size * Small_Page_Size));

private
   ----------------------------------------------------------------------------
   --  The following methods are the 'unlocked' versions of the above methods
   --  which are called once the spinlock has been acquired.
   --  These functions are only called from the 'locked' versions above.
   --  They are structured this way so that all happy/unhappy paths all lead to
   --  the same exit point, making it easier to ensure the spinlock is always
   --  released.
   ----------------------------------------------------------------------------
   procedure Allocate_Unlocked
     (Page_Pool         : in out Page_Pool_T;
      Page_Count        : Positive;
      Allocation_Result : out Memory_Allocation_Result;
      Result            : out Function_Result);

   procedure Free_Unlocked
     (Page_Pool       : in out Page_Pool_T;
      Page_Count      : Positive;
      Virtual_Address : Virtual_Address_T;
      Result          : out Function_Result);

end Memory.Allocators.Page;
