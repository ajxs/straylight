-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Function_Results; use Function_Results;
with Locks;            use Locks;
with Logging;          use Logging;

-------------------------------------------------------------------------------
--  @TODO: Investigate whether it's worthwhile changing the free regions
--  linked-list to use an index rather than pointers.
--  Investigate what is less error-prone.
--  Investigate whether there's a way to prove the index-based implementation
--  is error-free.
--  @NOTE: In this system, free memory regions are always physically and
--  virtually contiguous. I don't see this as being a major design issue.
--  Larger contiguous regions can be allocated from the kernel page pool, so
--  the amount of fragmentation resulting from this design is manageable.
-------------------------------------------------------------------------------

package Memory.Allocators.Heap is
   pragma Preelaborate;

   Max_Free_Regions : constant := 1024;

   --  Forward declaration to allow for pointer-type used within type.
   type Free_Region_T;

   type Free_Region_Access is access all Free_Region_T;

   ----------------------------------------------------------------------------
   --  Represents a free region within the heap.
   ----------------------------------------------------------------------------
   type Free_Region_T is record
      Virtual_Address  : Virtual_Address_T := Null_Address;
      Physical_Address : Physical_Address_T := Null_Physical_Address;
      Size             : Storage_Offset := 0;
      Next_Region      : Free_Region_Access := null;
      Entry_Used       : Boolean := False;
   end record;

   --  @NOTE: These types are kept public so that the fields can be accessed
   --  for the purpose of creating virtual memory mappings for the regions.
   type Free_Region_List_T is
     array (1 .. Max_Free_Regions) of aliased Free_Region_T;

   type Heap_Memory_Region_Index_T is new Natural;

   Max_Memory_Regions : constant Heap_Memory_Region_Index_T := 64;

   --  Rather than using pointers to create the linked-list structure used by
   --  the memory region list, this list just stores the _index_ of the next
   --  entry. If this index is equal to the null index (0) then, like a null
   --  pointer in a linked list, it is treated as a non-existent link.
   Null_Memory_Region_Index : constant Heap_Memory_Region_Index_T := 0;

   ----------------------------------------------------------------------------
   --  This type represents a contiguous allocated memory region for use by
   --  the heap. This is used to check whether a particular address being
   --  freed belongs to this heap.
   ----------------------------------------------------------------------------
   type Heap_Memory_Region_T is record
      Virtual_Address  : Virtual_Address_T := Null_Address;
      Physical_Address : Physical_Address_T := Null_Physical_Address;
      Size             : Storage_Offset := 0;
      Entry_Used       : Boolean := False;
      Next_Region      : Heap_Memory_Region_Index_T :=
        Null_Memory_Region_Index;
   end record;

   type Heap_Memory_Region_List_T is
     array (1 .. Max_Memory_Regions) of Heap_Memory_Region_T;

   type Memory_Heap_T is record
      Free_Regions        : Free_Region_List_T;
      Free_Regions_Head   : Free_Region_Access := null;
      Memory_Regions      : Heap_Memory_Region_List_T;
      Memory_Regions_Head : Heap_Memory_Region_Index_T :=
        Null_Memory_Region_Index;
      Spinlock            : Spinlock_T;
   end record;

   procedure Allocate
     (Memory_Heap       : in out Memory_Heap_T;
      Size              : Positive;
      Allocation_Result : out Memory_Allocation_Result;
      Result            : out Function_Result;
      Alignment         : Storage_Offset := 1);

   procedure Free
     (Memory_Heap               : in out Memory_Heap_T;
      Allocated_Virtual_Address : Virtual_Address_T;
      Result                    : out Function_Result);

   --  @TODO: Investigate whether it's necessary to record a physical address.
   --  @NOTE: It's important that the virtual and physical addresses of a new
   --  region are both page aligned.
   procedure Add_Memory_Region_To_Heap
     (Memory_Heap      : in out Memory_Heap_T;
      Virtual_Address  : Virtual_Address_T;
      Physical_Address : Physical_Address_T;
      Size             : Storage_Offset;
      Result           : out Function_Result);

   ----------------------------------------------------------------------------
   --  Prints all of the free region entries, and heap memory regions.
   ----------------------------------------------------------------------------
   procedure Print_Memory_Regions (Memory_Heap : Memory_Heap_T);

private
   Logging_Tags : constant Log_Tags := [Log_Tag_Heap, Log_Tag_Memory];

   Allocation_Header_Magic_Number : constant := 16#ABCDABCD#;

   ----------------------------------------------------------------------------
   --  The following methods are the 'unlocked' versions of the above methods
   --  which are called once the spinlock has been acquired.
   --  These functions are only called from the 'locked' versions above.
   --  They are structured this way so that all happy/unhappy paths all lead to
   --  the same exit point, making it easier to ensure the spinlock is always
   --  released.
   ----------------------------------------------------------------------------
   procedure Allocate_Unlocked
     (Memory_Heap       : in out Memory_Heap_T;
      Size              : Positive;
      Allocation_Result : out Memory_Allocation_Result;
      Result            : out Function_Result;
      Alignment         : Storage_Offset := 1);

   procedure Add_Memory_Region_To_Heap_Unlocked
     (Memory_Heap      : in out Memory_Heap_T;
      Virtual_Address  : Virtual_Address_T;
      Physical_Address : Physical_Address_T;
      Size             : Storage_Offset;
      Result           : out Function_Result);

   procedure Free_Unlocked
     (Memory_Heap               : in out Memory_Heap_T;
      Allocated_Virtual_Address : Virtual_Address_T;
      Result                    : out Function_Result);

   ----------------------------------------------------------------------------
   --  Allocation Header type.
   --  This structure sits in memory beneath an allocated region to record
   --  certain relevant attributes.
   ----------------------------------------------------------------------------
   type Allocation_Header_T is record
      Identity : Unsigned_32;
      Size     : Storage_Offset;
   end record;

   procedure Find_And_Allocate_Free_Region
     (Memory_Heap       : in out Memory_Heap_T;
      Size              : Storage_Offset;
      Alignment         : Storage_Offset;
      Allocation_Result : out Memory_Allocation_Result;
      Result            : out Function_Result);

   procedure Insert_Free_Region
     (Memory_Heap      : in out Memory_Heap_T;
      Virtual_Address  : Virtual_Address_T;
      Physical_Address : Physical_Address_T;
      Size             : Storage_Offset;
      Result           : out Function_Result);

   ----------------------------------------------------------------------------
   --  Coalesces all of the adjacent entries in the free entries list.
   --  Note: Regions will only be coalesced if the virtual AND physical
   --  addresses are contiguous.
   ----------------------------------------------------------------------------
   procedure Coalesce_Free_Region_Entries (Memory_Heap : in out Memory_Heap_T);

   function Find_Unused_Free_Region_Entry
     (Memory_Heap : in out Memory_Heap_T) return Free_Region_Access;

   procedure Initialise_Allocated_Region_Header
     (Region_Address : Address; Size : Storage_Offset);

   function Is_Valid_Allocation_Header
     (Header : Allocation_Header_T) return Boolean
   is (Header.Identity = Allocation_Header_Magic_Number);

   function Is_Virtual_Address_In_Heap
     (Memory_Heap : Memory_Heap_T; Virtual_Address : Virtual_Address_T)
      return Boolean;

   procedure Find_Region_By_Virtual_Address
     (Memory_Heap     : Memory_Heap_T;
      Virtual_Address : Virtual_Address_T;
      Found_Index     : out Heap_Memory_Region_Index_T;
      Found           : out Boolean);

   procedure Find_Region_By_Physical_Address
     (Memory_Heap      : Memory_Heap_T;
      Physical_Address : Physical_Address_T;
      Found_Index      : out Heap_Memory_Region_Index_T;
      Found            : out Boolean);

   function Is_Virtual_Address_Within_Memory_Region
     (Memory_Region   : Heap_Memory_Region_T;
      Virtual_Address : Virtual_Address_T) return Boolean
   is ((Virtual_Address >= Memory_Region.Virtual_Address)
       and then
         (Virtual_Address
          < (Memory_Region.Virtual_Address + Memory_Region.Size)))
   with Pure_Function, Inline;

   function Is_Physical_Address_within_Memory_Region
     (Memory_Region    : Heap_Memory_Region_T;
      Physical_Address : Physical_Address_T) return Boolean
   is ((Physical_Address >= Memory_Region.Physical_Address)
       and then
         (Physical_Address
          < Memory_Region.Physical_Address + Memory_Region.Size))
   with Pure_Function, Inline;

   ----------------------------------------------------------------------------
   --  Tests if a given region is intersecting with a given virtual and
   --  physical address ranges.
   ----------------------------------------------------------------------------
   function Is_Region_Intersecting
     (Region           : Heap_Memory_Region_T;
      Virtual_Address  : Virtual_Address_T;
      Physical_Address : Physical_Address_T;
      Length           : Storage_Offset) return Boolean
   is ((Virtual_Address < (Region.Virtual_Address + Region.Size)
        and then (Virtual_Address + Length) > Region.Virtual_Address)
       or else
         (Physical_Address < (Region.Physical_Address + Region.Size)
          and then (Physical_Address + Length) > Region.Physical_Address))
   with Pure_Function;

   function Find_Unused_Memory_Region_Entry
     (Memory_Heap : Memory_Heap_T) return Heap_Memory_Region_Index_T;

   function Calculate_Region_Alignment_Offset
     (Start_Address : Virtual_Address_T; Alignment : Storage_Offset)
      return Storage_Offset;

end Memory.Allocators.Heap;
