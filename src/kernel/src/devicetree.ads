with Interfaces;              use Interfaces;
with System;                  use System;
with System.Storage_Elements; use System.Storage_Elements;

with Function_Results; use Function_Results;
with Logging;          use Logging;
with Utilities;        use Utilities;

package Devicetree
  with Preelaborate
is
   procedure Parse_Devicetree
     (DTB_Address : Address; Result : out Function_Result);

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
   Devicetree_Logging_Tags : constant Log_Tags := [Log_Tag_Devicetree];

   FDT_MAGIC_NUMBER : constant Unsigned_32 := 16#D00D_FEED#;

   subtype FDT_Token_T is Unsigned_32;

   type FDT_Property_T is record
      Length      : Unsigned_32;
      Name_Offset : Unsigned_32;
   end record
   with Convention => C;

   type Reserved_Memory_Block_T is record
      Addr : Unsigned_64;
      Size : Unsigned_64;
   end record
   with Convention => C;

   Maximum_FDT_String_Length : constant := 256;

   subtype Devicetree_String_T is
     Fixed_Length_String_T (Maximum_FDT_String_Length);

   procedure Parse_Structure_Block
     (Structure_Block_Address : Address;
      Structure_Block_Size    : Storage_Count;
      String_Table_Address    : Address;
      String_Table_Size       : Storage_Count;
      Result                  : out Function_Result);

   procedure Parse_Property
     (Structure_Block_Address : Address;
      Structure_Block_Size    : Storage_Count;
      String_Table_Address    : Address;
      String_Table_Size       : Storage_Count;
      Property_Name           : out Devicetree_String_T;
      Property_Length         : out Unsigned_32;
      Property_Address        : out Address;
      Curr_Offset             : in out Storage_Offset;
      Result                  : out Function_Result);

   function Compare_Node_Name
     (Node_Name : Devicetree_String_T; Target_Name : String) return Boolean;

end Devicetree;
