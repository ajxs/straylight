-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Interfaces;              use Interfaces;
with System;                  use System;
with System.Storage_Elements; use System.Storage_Elements;

with Addresses;              use Addresses;
with Devices;                use Devices;
with Filesystems;            use Filesystems;
with Function_Results;       use Function_Results;
with Memory;                 use Memory;
with Memory.Allocators;      use Memory.Allocators;
with Memory.Allocators.Heap; use Memory.Allocators.Heap;
with Memory.Allocators.Page; use Memory.Allocators.Page;
with Processes;              use Processes;

package System_State is
   pragma Preelaborate;

   Maximum_Harts : constant := 8;
   subtype Hart_Index_T is Natural range 0 .. (Maximum_Harts - 1);

   subtype System_Time_T is Unsigned_64;

   ----------------------------------------------------------------------------
   --  An interesting long-term idea would be to allow the system to support
   --  multiple system states, which can be swapped between.
   ----------------------------------------------------------------------------
   type System_State_T is record
      Kernel_Heap      : Memory_Heap_T;
      Kernel_Page_Pool : Page_Pool_T;

      Devices : System_Device_Array;

      Root_Filesystem     : Filesystem_Access := null;
      Mounted_Filesystems : Mounted_Filesystem_Array;

      Open_Files : Process_File_Handle_Array;
   end record;

   type Hart_State_T is record
      Current_Process                         : Process_Control_Block_Access :=
        null;
      Interrupts_Off_Counter                  : Natural := 0;
      Were_Interrupts_Enabled_Before_Push_Off : Boolean := True;
   end record;

   type Hart_State_Access is access all Hart_State_T;

   type Hart_State_Array_T is array (Hart_Index_T) of aliased Hart_State_T;

   Hart_States : Hart_State_Array_T;

   Current_System_State : System_State_T;

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

   procedure Panic (Message : String := "Kernel Panic")
   with No_Return;

   procedure Create_New_Process
     (New_Process : out Process_Control_Block_Access;
      Result      : out Function_Result);

   procedure Idle
   with No_Return;

   --  @TODO: Implement.
   --  Currently the kernel only supports a single hart.
   function Get_Current_Hart_Id return Hart_Index_T
   is (0)
   with Inline, Volatile_Function;

   function Get_Current_Hart_State return Hart_State_Access
   with Volatile_Function;

   function Get_Process_Running_On_Current_Hart
      return Process_Control_Block_Access
   with Volatile_Function;

   --  These functions allow for turning interrupts on/off in nested critical
   --  sections.
   procedure Push_Interrupts_Off;

   procedure Pop_Interrupts_Off;

   function Get_Current_Hart_Supervisor_Interrupt_Context return Integer
   with Inline, Volatile_Function;

   procedure Find_File_Handle
     (Process_Id     : Process_Id_T;
      File_Handle_Id : Unsigned_64;
      File_Handle    : out Process_File_Handle_Access;
      Result         : out Function_Result);

private
   procedure Cleanup_Stopped_Processes (Result : out Function_Result);

end System_State;
