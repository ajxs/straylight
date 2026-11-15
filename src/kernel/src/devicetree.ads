with Interfaces;              use Interfaces;
with System;                  use System;
with System.Storage_Elements; use System.Storage_Elements;

with Function_Results; use Function_Results;
with Memory;           use Memory;

package Devicetree
  with Preelaborate
is
   --  The types of device recognised by the parser. The type of a device is
   --  determined by each node's 'compatible' property.
   type Devicetree_Device_Type_T is
     (Devicetree_Device_Type_None,
      Devicetree_Device_Type_PLIC,
      Devicetree_Device_Type_UART_NS16550A,
      Devicetree_Device_Type_Virtio_MMIO);

   --  A device described by the devicetree.
   type Devicetree_Device_T is record
      Device_Type    : Devicetree_Device_Type_T;
      --  The device's physical address base, as described by the device node's
      --  first 'reg' property.
      Base           : Physical_Address_T;
      Size           : Storage_Count;
      --  The first cell of the device's 'interrupts' property, or 0 if none.
      Interrupt_Line : Unsigned_32;
   end record;

   type Devicetree_Device_Array_T is
     array (Positive range <>) of Devicetree_Device_T;

   --  Memory_Regions returns an array of usable physical memory regions
   --  described by the devicetree, with the reserved regions and
   --  Excluded_Region removed. Boot passes the kernel image as
   --  Excluded_Region. Devices returns the recognised devices, in devicetree
   --  order.
   procedure Parse_Devicetree
     (DTB_Address         : Address;
      Excluded_Region     : Physical_Memory_Region_T;
      Memory_Regions      : out Physical_Memory_Region_Array_T;
      Memory_Region_Count : out Natural;
      Devices             : out Devicetree_Device_Array_T;
      Device_Count        : out Natural;
      Result              : out Function_Result);

   --  The header is a public definition, since it's used in the early
   --  boot stage when mapping the devicetree into virtual memory.
   type FDT_Header_T is record
      Magic             : Unsigned_32;
      Totalsize         : Unsigned_32;
      Off_DT_Struct     : Unsigned_32;
      Off_DT_Strings    : Unsigned_32;
      Off_Mem_Rsvmap    : Unsigned_32;
      Version           : Unsigned_32;
      Last_Comp_Version : Unsigned_32;
      Boot_Cpuid_Phys   : Unsigned_32;
      Size_DT_Strings   : Unsigned_32;
      Size_DT_Struct    : Unsigned_32;
   end record
   with Convention => C;

end Devicetree;
