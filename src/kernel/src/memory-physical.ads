-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Function_Results; use Function_Results;
with Locks;            use Locks;
with Logging;          use Logging;

-------------------------------------------------------------------------------
--  Contains code and definitions for allocating and deallocating
--  physical memory in the system.
-------------------------------------------------------------------------------

package Memory.Physical is
   pragma Preelaborate;

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

   type Block_Index is range 1 .. Maximum_Physical_Memory_Blocks + 1;

   No_Block : constant Block_Index := Block_Index'Last;

   subtype Valid_Block_Index is
     Block_Index range 1 .. Maximum_Physical_Memory_Blocks;

   ----------------------------------------------------------------------------
   --  Descriptor for a physical memory block managed by the allocator.
   ----------------------------------------------------------------------------
   type Physical_Memory_Block_T is record
      Address    : Physical_Address_T;
      Order      : Block_Order := Maximum_Block_Order;
      Free       : Boolean := False;
      Next_Block : Block_Index := No_Block;
      Entry_Used : Boolean := False;
   end record;

   type Physical_Memory_Block_Array is
     array (Valid_Block_Index) of Physical_Memory_Block_T;

   type Physical_Memory_Space_T is record
      Physical_Memory_Blocks        : Physical_Memory_Block_Array;
      Physical_Memory_Map_List_Head : Block_Index := No_Block;
      Spinlock                      : Spinlock_T;
   end record;

   Phys_Memory_Space : Physical_Memory_Space_T :=
     (Physical_Memory_Blocks        =>
        [others =>
           (Address    => Physical_Address_T (System'To_Address (0)),
            Order      => Maximum_Block_Order,
            Free       => False,
            Next_Block => No_Block,
            Entry_Used => False)],
      Physical_Memory_Map_List_Head => No_Block,
      Spinlock                      =>
        (Locked        => 0,
         Time_Acquired => 0,
         Hart_Id       => No_Hart_Id,
         Lock_Id       => Lock_Id_Physical_Memory));

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

   function Find_Free_Entry return Block_Index;

   function Get_List_Tail return Block_Index;

   --  Checks if the specified region intersects with the given block.
   --  This is used to determine if a block can be allocated for a requested
   --  region of memory.
   function Is_Region_Intersecting
     (Start  : Physical_Address_T;
      Length : Positive;
      Block  : Physical_Memory_Block_T) return Boolean
   with Pure_Function;

   procedure Check_For_Intersecting_Blocks
     (Region_Start  : Physical_Address_T;
      Region_Length : Positive;
      Result        : out Function_Result);

   function Get_Block_Size_In_Bytes (Order : Block_Order) return Natural
   with Inline, Pure_Function;

   procedure Divide_Physical_Memory_Block
     (Block : in out Physical_Memory_Block_T; Result : out Function_Result);

   procedure Divide_Block_To_Specified_Order
     (Block          : in out Physical_Memory_Block_T;
      Required_Order : Block_Order;
      Result         : out Function_Result);

   function Can_Blocks_Be_Consolidated
     (Block, Next_Block : Physical_Memory_Block_T) return Boolean
   is (
       --  Ensure both blocks are free.
       Block.Free
       and then Next_Block.Free
       --  Ensure both block entries are valid.
       and then Block.Entry_Used
       and then Next_Block.Entry_Used
       --  If this block is already the maximum size, it can't be consolidated.
       and then Block.Order /= Maximum_Block_Order
       --  For blocks to be consolidated, their orders need to match.
       --  This also ensures that the next block is not the maximum order.
       and then Block.Order = Next_Block.Order
       --  For blocks to be consolidated, they need to be adjacent in memory.
       and then
         Block.Address + Storage_Offset (Get_Block_Size_In_Bytes (Block.Order))
         = Next_Block.Address)
   with Pure_Function;

   procedure Consolidate_Adjacent_Memory_Blocks
     (Block : in out Physical_Memory_Block_T; Result : out Function_Result);

   procedure Consolidate_Free_Physical_Memory_Blocks
     (Result : out Function_Result);

   function Find_Block_With_Address
     (Addr : Physical_Address_T) return Block_Index;

   function Is_List_Empty return Boolean
   is (Phys_Memory_Space.Physical_Memory_Map_List_Head = No_Block)
   with Inline;

end Memory.Physical;
