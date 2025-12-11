-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Addresses;        use Addresses;
with Function_Results; use Function_Results;
with Locks;            use Locks;
with Logging;          use Logging;

-------------------------------------------------------------------------------
--  Contains code and definitions for allocating and deallocating
--  physical memory in the system.
-------------------------------------------------------------------------------

package Memory.Physical is
   pragma Preelaborate;

   --  Forward declarations, to allow for recursive pointer.
   type Physical_Memory_Block_T is private;
   type Block_Access is access all Physical_Memory_Block_T;

   type Physical_Memory_Block_Array is private;

   type Physical_Memory_Space_T is record
      Physical_Memory_Blocks        : Physical_Memory_Block_Array;
      Physical_Memory_Map_List_Head : Block_Access := null;
      Spinlock                      : Spinlock_T;
   end record;

   procedure Allocate_Physical_Memory
     (Required_Size     : Positive;
      Allocated_Address : out Physical_Address_T;
      Result            : out Function_Result);

   procedure Free_Physical_Memory
     (Addr : Physical_Address_T; Result : out Function_Result);

   procedure Reallocate_Physical_Memory
     (Addr     : in out Physical_Address_T;
      New_Size : Positive;
      Result   : out Function_Result);

   procedure Create_Free_Physical_Memory_Region
     (Region_Start  : Physical_Address_T;
      Region_Length : Positive;
      Result        : out Function_Result);

private
   Logging_Tags : constant Log_Tags :=
     [Log_Tag_Memory,
      Log_Tag_Physical_Memory_Allocation,
      Log_Tag_Physical_Memory_Manager];

   Maximum_Physical_Memory_Blocks : constant := 1024;

   Maximum_Block_Order : constant := 16;

   subtype Block_Order is Natural range 0 .. Maximum_Block_Order;

   ----------------------------------------------------------------------------
   --  Descriptor for a physical memory block managed by the allocator.
   ----------------------------------------------------------------------------
   type Physical_Memory_Block_T is record
      Address    : Physical_Address_T;
      Order      : Block_Order := Maximum_Block_Order;
      Free       : Boolean := False;
      Next_Block : Block_Access := null;
      Entry_Used : Boolean := False;
   end record;

   type Physical_Memory_Block_Array is
     array (1 .. Maximum_Physical_Memory_Blocks)
     of aliased Physical_Memory_Block_T;

   ----------------------------------------------------------------------------
   --  The base block size.
   --  4KB granularity.
   ----------------------------------------------------------------------------
   Base_Block_Size : constant := 16#1000#;

   ----------------------------------------------------------------------------
   --  The following methods are the 'unlocked' versions of the above methods
   --  which are called once the spinlock has been acquired.
   --  These functions are only called from the 'locked' versions above.
   --  They are structured this way so that all happy/unhappy paths all lead to
   --  the same exit point, making it easier to ensure the spinlock is always
   --  released.
   ----------------------------------------------------------------------------
   procedure Allocate_Physical_Memory_Unlocked
     (Required_Size     : Positive;
      Allocated_Address : out Physical_Address_T;
      Result            : out Function_Result);

   procedure Free_Physical_Memory_Unlocked
     (Addr : Physical_Address_T; Result : out Function_Result);

   procedure Reallocate_Unlocked
     (Addr     : in out Physical_Address_T;
      New_Size : Positive;
      Result   : out Function_Result);

   procedure Create_Free_Region_Unlocked
     (Region_Start  : Physical_Address_T;
      Region_Length : Positive;
      Result        : out Function_Result);

   procedure Get_Highest_Possible_Block_Order
     (Physical_Memory_Length       : Natural;
      Highest_Possible_Block_Order : out Natural;
      Result                       : out Function_Result);

   procedure Get_Smallest_Possible_Block_Order
     (Required_Size                 : Positive;
      Smallest_Possible_Block_Order : out Natural;
      Result                        : out Function_Result);

   function Find_Free_Entry return Block_Access;

   function Get_List_Tail return Block_Access;

   function Is_Region_Intersecting
     (Start  : Physical_Address_T;
      Length : Positive;
      Block  : Physical_Memory_Block_T) return Boolean
   with Pure_Function;

   function Get_Block_Size_In_Bytes (Order : Block_Order) return Natural
   with Inline, Pure_Function;

   procedure Divide_Physical_Memory_Block
     (Block : in out Physical_Memory_Block_T; Result : out Function_Result);

   procedure Divide_Block_To_Specified_Order
     (Block          : in out Physical_Memory_Block_T;
      Required_Order : Block_Order;
      Result         : out Function_Result);

   procedure Consolidate_Adjacent_Memory_Blocks
     (Block : in out Physical_Memory_Block_T; Result : out Function_Result);

   procedure Consolidate_Free_Physical_Memory_Blocks
     (Result : out Function_Result);

   function Find_Block_With_Address
     (Addr : Physical_Address_T) return Block_Access;

   function Is_List_Empty return Boolean
   with Inline;

end Memory.Physical;
