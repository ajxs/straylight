-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Function_Results; use Function_Results;
with Locks;            use Locks;
with Logging;          use Logging;
with RISCV.Paging;     use RISCV.Paging;

package Memory.Allocators.Page
  with Preelaborate
is
   Page_Pool_Page_Size : constant := Small_Page_Size;

   --  The maximum number of pages in a page pool region.
   --  Regions added to the pool at runtime may have fewer pages, because the
   --  physical memory backing a region must be contiguous, and a full-sized
   --  physically contiguous block may not always be available.
   Max_Page_Pool_Region_Size : constant := 1024;

   Page_Pool_Region_Size_In_Bytes : constant :=
     Max_Page_Pool_Region_Size * Page_Pool_Page_Size;

   --  Each allocation is recorded as a run of pages: the first page is
   --  marked Run_Start, and any subsequent pages Run_Continuation.
   --  This allows Free to determine the length of an allocation from its
   --  address alone, and to reject any address that doesn't mark the start of
   --  a run of pages.
   type Page_Status_T is (Free, Run_Start, Run_Continuation) with Size => 2;

   type Page_Status_Array_T is
     array (1 .. Max_Page_Pool_Region_Size) of Page_Status_T
   with Pack;

   subtype Region_Page_Count_T is Natural range 0 .. Max_Page_Pool_Region_Size;

   type Page_Pool_Region_T is record
      Virtual_Address  : Virtual_Address_T;
      Physical_Address : Physical_Address_T;
      Page_Statuses    : Page_Status_Array_T;
      --  The number of usable pages in this region. Only the first
      --  Page_Count entries in Page_Statuses are meaningful.
      Page_Count       : Region_Page_Count_T := 0;
      Allocated        : Boolean := False;
   end record;

   Max_Page_Pool_Regions : constant := 128;

   type Page_Pool_Region_Array_T is
     array (1 .. Max_Page_Pool_Regions) of Page_Pool_Region_T;

   type Page_Pool_T is record
      Page_Pool_Regions : Page_Pool_Region_Array_T;
      --  The total number of free pages across all regions in the pool.
      Free_Page_Count   : Natural := 0;
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
      Page_Count       : Positive;
      Result           : out Function_Result);

   procedure Free
     (Page_Pool       : in out Page_Pool_T;
      Virtual_Address : Virtual_Address_T;
      Result          : out Function_Result);

   function Is_Virtual_Address_In_Region
     (Region : Page_Pool_Region_T; Virtual_Address : Virtual_Address_T)
      return Boolean
   is (Virtual_Address >= Region.Virtual_Address
       and then
         Virtual_Address
         < Region.Virtual_Address
           + Storage_Offset (Region.Page_Count * Page_Pool_Page_Size));

private
   Logging_Tags_Page_Pool : constant Log_Tags :=
     [Log_Tag_Page_Pool, Log_Tag_Memory];

end Memory.Allocators.Page;
