with Interfaces; use Interfaces;
with System;     use System;

with Function_Results; use Function_Results;
with Utilities;        use Utilities;

package Devicetree
  with Preelaborate
is
   --  A range of physical memory described by a 'memory' node's 'reg'.
   type Memory_Region_T is record
      Base : Unsigned_64 := 0;
      Size : Unsigned_64 := 0;
   end record;

   Maximum_Memory_Regions : constant := 16;

   type Memory_Region_Array_T is
     array (1 .. Maximum_Memory_Regions) of Memory_Region_T;

   --  Memory_Regions returns an array of usable physical memory regions
   --  described by the devicetree, with reserved regions and the kernel image
   --  removed.
   procedure Parse_Devicetree
     (DTB_Address         : Address;
      Memory_Regions      : out Memory_Region_Array_T;
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

private
   Maximum_FDT_String_Length : constant := 256;

   subtype Devicetree_String_T is
     Fixed_Length_String_T (Maximum_FDT_String_Length);

   function Compare_Node_Name
     (Node_Name : Devicetree_String_T; Target_Name : String) return Boolean;

end Devicetree;
