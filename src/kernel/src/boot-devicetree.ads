with System.Storage_Elements; use System.Storage_Elements;

with Function_Results; use Function_Results;

package Boot.Devicetree is
   pragma Preelaborate;

   procedure Parse_Devicetree
     (DTB_Address : Address; Result : out Function_Result);

   function Get_Devicetree_Size (DTB_Address : Address) return Storage_Offset;

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
   end record;
   for FDT_Header_T use
     record
       Magic             at 0  range 0 .. 31;
       Totalsize         at 4  range 0 .. 31;
       Off_DT_Struct     at 8  range 0 .. 31;
       Off_DT_Strings    at 12 range 0 .. 31;
       Off_Mem_Rsvmap    at 16 range 0 .. 31;
       Version           at 20 range 0 .. 31;
       Last_Comp_Version at 24 range 0 .. 31;
       Boot_Cpuid_Phys   at 28 range 0 .. 31;
       Size_DT_Strings   at 32 range 0 .. 31;
       Size_DT_Struct    at 36 range 0 .. 31;
     end record;

private
   Devicetree_Logging_Tags : constant Log_Tags := [Log_Tag_Devicetree];

   FDT_MAGIC_NUMBER : constant Unsigned_32 := 16#D00D_FEED#;

   subtype FDT_Token_T is Unsigned_32;

   type FDT_Property_T is record
      Length      : Unsigned_32;
      Name_Offset : Unsigned_32;
   end record;
   for FDT_Property_T use
     record
       Length      at 0 range 0 .. 31;
       Name_Offset at 4 range 0 .. 31;
     end record;

   type Reserved_Memory_Block_T is record
      Addr : Unsigned_64;
      Size : Unsigned_64;
   end record;

   for Reserved_Memory_Block_T use
     record
       Addr at 0 range 0 .. 63;
       Size at 8 range 0 .. 63;
     end record;

   Maximum_String_Length : constant := 256;

   subtype Devicetree_String_T is String (1 .. Maximum_String_Length);

   procedure Parse_Structure_Block
     (Structure_Block_Address : Address;
      String_Table_Address    : Address;
      Result                  : out Function_Result);

   procedure Parse_Property
     (Structure_Block_Address : Address;
      String_Table_Address    : Address;
      Property_Name           : out Devicetree_String_T;
      Property_Name_Length    : out Natural;
      Property_Length         : out Unsigned_32;
      Property_Address        : out Address;
      Curr_Offset             : in out Storage_Offset);

   procedure Read_Property_Name_String
     (String_Table_Address : Address;
      String_Table_Offset  : Storage_Offset;
      Property_Name        : out Devicetree_String_T;
      Property_Name_Length : out Natural);

   function Compare_Property_Name
     (Property_Name        : Devicetree_String_T;
      Property_Name_Length : Natural;
      Target_Name          : String) return Boolean;

   function Compare_Node_Name
     (Node_Name        : Devicetree_String_T;
      Node_Name_Length : Natural;
      Target_Name      : String) return Boolean;

   function Is_String_Value
     (Property_Name : Devicetree_String_T; Property_Name_Length : Natural)
      return Boolean;

   --  The current address and size cells context is captured in a stack, so
   --  that we can properly handle nested nodes that override the
   --  #address-cells and #size-cells properties.
   --  Procedures are contained in this package for pushing and popping the
   --  context as the devicetree is traversed.
   type FDT_Cells_Context_T is record
      Address_Cells : Unsigned_32;
      Size_Cells    : Unsigned_32;
   end record;

   type FDT_Cells_Context_Stack_T is
     array (Natural range <>) of FDT_Cells_Context_T;

   procedure Push_Cells_Context
     (Context_Stack     : in out FDT_Cells_Context_Stack_T;
      Context_Stack_Ptr : in out Natural;
      New_Context       : FDT_Cells_Context_T;
      Result            : out Function_Result);

   procedure Pop_Cells_Context
     (Context_Stack     : FDT_Cells_Context_Stack_T;
      Context_Stack_Ptr : in out Natural;
      Popped_Context    : out FDT_Cells_Context_T;
      Result            : out Function_Result);

end Boot.Devicetree;
