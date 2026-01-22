with Function_Results;       use Function_Results;
with Logging;                use Logging;
with Memory.Allocators;      use Memory.Allocators;
with Memory.Allocators.Heap; use Memory.Allocators.Heap;
with Memory.Allocators.Page; use Memory.Allocators.Page;

package Memory.Kernel is
   pragma Preelaborate;

   --  Allocates kernel heap memory, returning both the virtual and physical
   --  addresses of the allocated region.
   --  This is generally used for any kernel memory that needs DMA access
   --  by a device, like filesystem buffers.
   procedure Allocate_Kernel_Physical_Memory
     (Size              : Positive;
      Allocation_Result : out Memory_Allocation_Result;
      Result            : out Function_Result;
      Alignment         : Storage_Offset := 1);

   --  @TODO: Potentially this could be a front-end to both the heap and page
   --  allocators, depending on the size/alignment requested.
   procedure Allocate_Kernel_Memory
     (Size              : Positive;
      Allocated_Address : out Virtual_Address_T;
      Result            : out Function_Result;
      Alignment         : Storage_Offset := 1);

   procedure Free_Kernel_Memory
     (Allocated_Virtual_Address : Virtual_Address_T;
      Result                    : out Function_Result);

   procedure Allocate_Pages
     (Number_of_Pages   : Positive;
      Allocation_Result : out Memory_Allocation_Result;
      Result            : out Function_Result);

   procedure Free_Pages
     (Virtual_Address : Virtual_Address_T;
      Page_Count      : Positive;
      Result          : out Function_Result);

   procedure Initialise_Kernel_Heap;

   procedure Initialise_Kernel_Page_Pool;

private
   Logging_Tags : constant Log_Tags := [Log_Tag_Memory];

   Kernel_Heap      : Memory_Heap_T;
   Kernel_Page_Pool : Page_Pool_T;

end Memory.Kernel;
