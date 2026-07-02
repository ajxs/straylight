with Function_Results; use Function_Results;
with Locks;            use Locks;
with Logging;          use Logging;

package Memory.Allocators.Heap
  with Preelaborate
is
   type New_Heap_Memory_Region_T is record
      Heap_Region_Virt_Addr : Virtual_Address_T := Null_Address;
      Heap_Region_Phys_Addr : Physical_Address_T := Null_Physical_Address;
      Heap_Region_Size      : Storage_Offset := 0;
   end record;

   New_Heap_Max_Memory_Regions : constant := 16;

   type New_Heap_Memory_Region_List_T is
     array (1 .. New_Heap_Max_Memory_Regions) of New_Heap_Memory_Region_T;

   type Memory_Heap_T is record
      Memory_Regions : New_Heap_Memory_Region_List_T;
      Spinlock       : Spinlock_T;
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

   --  @NOTE: It's important that the virtual and physical addresses of a new
   --  region are both page aligned.
   procedure Add_Memory_Region_To_Heap
     (Memory_Heap      : in out Memory_Heap_T;
      Virtual_Address  : Virtual_Address_T;
      Physical_Address : Physical_Address_T;
      Size             : Storage_Offset;
      Result           : out Function_Result);

private
   Logging_Tags_Heap : constant Log_Tags := [Log_Tag_Heap, Log_Tag_Memory];

   type Allocation_Header_T is record
      Identity   : Unsigned_32;
      Block_Size : Storage_Offset;
   end record
   with Size => 16 * 8;

   Identity_Marker_Free      : constant := 16#ABCDABCD#;
   Identity_Marker_Allocated : constant := 16#12345678#;

   Header_Size : constant Storage_Offset := Allocation_Header_T'Size / 8;

end Memory.Allocators.Heap;
