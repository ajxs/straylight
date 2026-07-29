with Function_Results;       use Function_Results;
with Locks;                  use Locks;
with Logging;                use Logging;
with Memory.Allocators;      use Memory.Allocators;
with Memory.Allocators.Heap; use Memory.Allocators.Heap;
with Memory.Allocators.Page; use Memory.Allocators.Page;

package Memory.Kernel
  with Preelaborate
is
   Kernel_Page_Pool_Page_Size : constant := Page_Pool_Page_Size;

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
     (Virtual_Address : Virtual_Address_T; Result : out Function_Result);

private
   Logging_Tags : constant Log_Tags :=
     [Log_Tag_Memory, Log_Tag_Memory_Allocators];

   Kernel_Heap : Memory_Heap_T :=
     (Memory_Regions_List_Head => null,
      Window_Base              => System'To_Address (Kernel_Heap_Address),
      Window_Size              => Kernel_Heap_Address_Space_Size,
      Spinlock                 =>
        (Locked        => 0,
         Time_Acquired => 0,
         Hart_Id       => No_Hart_Id,
         Lock_Id       => Lock_Id_Kernel_Heap));

   Kernel_Page_Pool : Page_Pool_T :=
     (Page_Pool_Regions =>
        [others =>
           (Virtual_Address  => System'To_Address (0),
            Physical_Address => Physical_Address_T (System'To_Address (0)),
            Page_Statuses    => [others => Free],
            Page_Count       => 0,
            Allocated        => False)],
      Free_Page_Count   => 0,
      Spinlock          =>
        (Locked        => 0,
         Time_Acquired => 0,
         Hart_Id       => No_Hart_Id,
         Lock_Id       => Lock_Id_Kernel_Page_Pool));

end Memory.Kernel;
