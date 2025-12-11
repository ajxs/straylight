-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Interfaces;              use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;

with Addresses;        use Addresses;
with Function_Results; use Function_Results;
with Locks;            use Locks;
with Logging;          use Logging;

package Memory.Virtual is
   pragma Preelaborate;

   type Virtual_Memory_Map_T is private;

   --  Forward declarations, to allow for recursive pointer.
   type Virtual_Memory_Mapping_T is private;
   type Mapping_Access is access all Virtual_Memory_Mapping_T;

   subtype Memory_Region_Size is Storage_Offset;

   type Memory_Region_Flags_T is record
      Read    : Boolean;
      Write   : Boolean;
      Execute : Boolean;
      User    : Boolean;
   end record;

   ----------------------------------------------------------------------------
   --  Virtual Memory Space Type.
   --  Represents a process' virtual memory space.
   ----------------------------------------------------------------------------
   type Virtual_Memory_Space_T is record
      Base_Page_Table_Addr : Physical_Address_T;
      Memory_Map           : Virtual_Memory_Map_T;
      Memory_Map_List_Head : Mapping_Access := null;
      Address_Space_ID     : Unsigned_16 := 0;
      --  Virtual memory spaces for user processes share the same page table
      --  entries for the kernel's address space. This flag is used to restrict
      --  them from altering higher-half kernel address mappings.
      --  This system is non-ideal, and should be revisited at some point.
      --  The main issue is that it's ideal to share the PTEs for the
      --  higher-half kernel mappings _between_ all processes, but the issue
      --  arises of how to share the kernel mapping structures between
      --  processes.
      --  The temporary solution is to not share the kernel mapping structures,
      --  but to restrict them being altered via this flag.
      User_Address_Space   : Boolean := True;
      Spinlock             : Spinlock_T;
   end record;

   procedure Create_New_Process_Memory_Space
     (New_Memory_Space : out Virtual_Memory_Space_T;
      Result           : out Function_Result);

   procedure Deallocate_Memory_Space
     (Virt_Memory_Space : in out Virtual_Memory_Space_T;
      Result            : out Function_Result);

   procedure Map
     (Virt_Memory_Space : in out Virtual_Memory_Space_T;
      Virtual_Address   : Virtual_Address_T;
      Physical_Address  : Physical_Address_T;
      Size              : Memory_Region_Size;
      Region_Flags      : Memory_Region_Flags_T;
      Result            : out Function_Result);

   procedure Unmap
     (Virt_Memory_Space : in out Virtual_Memory_Space_T;
      Virt_Addr         : Virtual_Address_T;
      Result            : out Function_Result);

   ----------------------------------------------------------------------------
   --  Translates a PHYSICAL address to its corresponding address in the
   --  kernel's map of all physical memory in its virtual address space.
   ----------------------------------------------------------------------------
   function Get_Physical_Address_Virtual_Mapping
     (Physical_Address : Physical_Address_T) return Virtual_Address_T
   is (Address (Physical_Address) + Physical_Memory_Map_Address)
   with Pure_Function;

   procedure Copy_Kernel_Memory_Mappings_Into_Address_Space
     (Source_Addr_Space : Virtual_Memory_Space_T;
      Dest_Addr_Space   : in out Virtual_Memory_Space_T;
      Result            : out Function_Result);

private
   Logging_Tags : constant Log_Tags :=
     [Log_Tag_Memory, Log_Tag_Virtual_Memory_Manager];

   ----------------------------------------------------------------------------
   --  The following methods are the 'unlocked' versions of the above methods
   --  which are called once the spinlock has been acquired.
   --  These functions are only called from the 'locked' versions above.
   --  They are structured this way so that all happy/unhappy paths all lead to
   --  the same exit point, making it easier to ensure the spinlock is always
   --  released.
   ----------------------------------------------------------------------------
   procedure Map_Unlocked
     (Virt_Memory_Space : in out Virtual_Memory_Space_T;
      Virtual_Address   : Virtual_Address_T;
      Physical_Address  : Physical_Address_T;
      Size              : Memory_Region_Size;
      Region_Flags      : Memory_Region_Flags_T;
      Result            : out Function_Result);

   procedure Unmap_Unlocked
     (Virt_Memory_Space : in out Virtual_Memory_Space_T;
      Virt_Addr         : Virtual_Address_T;
      Result            : out Function_Result);

   procedure Deallocate_Memory_Space_Unlocked
     (Virt_Memory_Space : in out Virtual_Memory_Space_T;
      Result            : out Function_Result);

   Maximum_Virtual_Memory_Mapping_Entries : constant := 128;

   ----------------------------------------------------------------------------
   --  Descriptor for an allocated memory block.
   ----------------------------------------------------------------------------
   type Virtual_Memory_Mapping_T is record
      Virtual_Addr  : Virtual_Address_T := Null_Address;
      Physical_Addr : Physical_Address_T := Null_Physical_Address;
      Size          : Memory_Region_Size := 0;
      Flags         : Memory_Region_Flags_T := (False, False, False, False);
      Next_Region   : Mapping_Access := null;
      Entry_Used    : Boolean := False;
   end record;

   ----------------------------------------------------------------------------
   --  Virtual Memory Mapping array type.
   --  This represents a single address space's mapped regions.
   ----------------------------------------------------------------------------
   type Virtual_Memory_Map_T is
     array (1 .. Maximum_Virtual_Memory_Mapping_Entries)
     of aliased Virtual_Memory_Mapping_T;

   function Is_Region_Intersecting
     (Region     : Virtual_Memory_Mapping_T;
      Start_Addr : Virtual_Address_T;
      Size       : Memory_Region_Size) return Boolean
   with Pure_Function;

   function Is_Mapping_List_Empty
     (Addr_Space : Virtual_Memory_Space_T) return Boolean
   is (Addr_Space.Memory_Map_List_Head = null);

   function Get_Real_Region_Size
     (Size : Memory_Region_Size) return Memory_Region_Size
   with Pure_Function;

   procedure Find_Unused_List_Entry_Index
     (Addr_Space : Virtual_Memory_Space_T;
      Free_Index : out Positive;
      Result     : out Function_Result);

   function Validate_Memory_Region_Permissions
     (Region_Flags : Memory_Region_Flags_T) return Boolean
   with Pure_Function, Inline;

end Memory.Virtual;
