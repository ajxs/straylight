with Function_Results; use Function_Results;

-------------------------------------------------------------------------------
--  This package provides a front-end for kernel memory allocation.
--  The actual memory allocation back-end implementation is encapsulated in
--  the package body.
-------------------------------------------------------------------------------

package Memory.Kernel
  with Preelaborate
is
   --  Allocates kernel heap memory, returning both the virtual and physical
   --  addresses of the allocated region.
   --  This is generally used for any kernel memory that needs DMA access
   --  by a device, like filesystem buffers.
   procedure Allocate_Kernel_Physical_Memory
     (Size              : Storage_Count;
      Allocation_Result : out Virtual_Physical_Address_Pair_T;
      Result            : out Function_Result;
      Alignment         : Storage_Count := 1);

   procedure Allocate_Kernel_Memory
     (Size              : Storage_Count;
      Allocated_Address : out Virtual_Address_T;
      Result            : out Function_Result;
      Alignment         : Storage_Count := 1);

   procedure Free_Kernel_Memory
     (Allocated_Virtual_Address : Virtual_Address_T;
      Result                    : out Function_Result);

   procedure Allocate_Pages
     (Number_of_Pages   : Positive;
      Allocation_Result : out Virtual_Physical_Address_Pair_T;
      Result            : out Function_Result);

   procedure Free_Pages
     (Virtual_Address : Virtual_Address_T; Result : out Function_Result);

end Memory.Kernel;
