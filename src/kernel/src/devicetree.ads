with Interfaces; use Interfaces;
with System;     use System;

with Function_Results; use Function_Results;
with Memory;           use Memory;

package Devicetree
  with Preelaborate
is
   --  Memory_Regions returns an array of usable physical memory regions
   --  described by the devicetree, with reserved regions and the kernel image
   --  removed. If they don't fit in Memory_Regions, Result is
   --  Region_Array_Exhausted.
   procedure Parse_Devicetree
     (DTB_Address         : Address;
      Memory_Regions      : out Physical_Memory_Region_Array_T;
      Memory_Region_Count : out Natural;
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
