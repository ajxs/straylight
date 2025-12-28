-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Ada.Unchecked_Conversion;
with System;                  use System;
with System.Storage_Elements; use System.Storage_Elements;
with Interfaces;              use Interfaces;

with Addresses;              use Addresses;
with Function_Results;       use Function_Results;
with Locks;                  use Locks;
with Logging;                use Logging;
with Memory;                 use Memory;
with Memory.Allocators.Heap; use Memory.Allocators.Heap;
with Memory.Virtual;         use Memory.Virtual;
with RISCV;

package Processes is
   pragma Preelaborate;

   type Process_Id_T is new Natural;

   type Process_Control_Block_T;
   type Process_Control_Block_Access is access all Process_Control_Block_T
   with Convention => C;

   --  Used to save a process' register state during a trap.
   type GP_Register_Array_T is
     array (RISCV.GP_Register_List_T) of aliased Unsigned_64
   with Convention => C;

   type Saved_Register_Array_T is
     array (Natural range <>) of aliased Unsigned_64
   with Convention => C;

   type Kernel_Context_Register_List_T is
     (ra, sp, s0, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11);

   type Kernel_Context_T is
     array (Kernel_Context_Register_List_T) of aliased Unsigned_64
   with Convention => C;

   ----------------------------------------------------------------------------
   --  Represents the context of a particular process.
   ----------------------------------------------------------------------------
   type Process_Context_T is record
      Gp_Registers      : GP_Register_Array_T;
      Fp_Registers      : Saved_Register_Array_T (0 .. 31);
      Sepc              : Virtual_Address_T;
      Sstatus           : Unsigned_64;
      Scause            : Unsigned_64;
      Trap_Context_Addr : Virtual_Address_T;
   end record
   with Convention => C;

   type Process_Status_T is
     (Process_Ready,
      Process_Running,
      Process_Blocked_Waiting_For_Response,
      Process_Stopped);

   subtype Blocking_Channel_T is Unsigned_64;

   ----------------------------------------------------------------------------
   --  System Process Control Block type.
   --  Represents a system process.
   ----------------------------------------------------------------------------
   type Process_Control_Block_T is record
      Kernel_Context      : Kernel_Context_T;
      Trap_Context_Addr   : Virtual_Address_T := Null_Address;
      User_Trap_Stack_Ptr : Virtual_Address_T := Null_Address;
      Process_Id          : Process_Id_T := 0;
      Status              : Process_Status_T := Process_Ready;
      Blocked_By_Channel  : Blocking_Channel_T := 0;
      --  @TODO: In future, investigate the possibility of storing a process'
      --  memory space on disk, rather than in the process control block.
      Memory_Space        : Virtual_Memory_Space_T;

      Stack_Phys_Addr : Physical_Address_T := Null_Physical_Address;
      Stack_Size      : Storage_Offset := 0;

      Kernel_Stack_Phys_Addr : Physical_Address_T := Null_Physical_Address;
      Kernel_Stack_Virt_Addr : Virtual_Address_T := Null_Address;
      Kernel_Stack_Size      : Storage_Offset := 0;

      Has_Process_Started : Boolean := False;

      Process_Entry_Point : Virtual_Address_T := Null_Address;

      Heap     : Memory_Heap_T;
      Spinlock : Spinlock_T;

      Next_Process : Process_Control_Block_Access := null;
   end record;

   Process_Queue : Process_Control_Block_Access := null;
   Idle_Process  : Process_Control_Block_Access := null;

   ----------------------------------------------------------------------------
   --  Allocates all of the physical memory required for a new process.
   --  This initialises the process' memory space, heap, and stack.
   --  This also allocates the new process' id.
   ----------------------------------------------------------------------------
   procedure Allocate_And_Map_New_Process_Memory
     (New_Process : out Process_Control_Block_T; Result : out Function_Result);

   --  This needs to stay in this package to avoid issues re: strict aliasing.
   --  This automatically suppresses the aliasing optimisations.
   --  refer to: https://gcc.gnu.org/onlinedocs/gcc-9.4.0/
   --    gnat_ugn/Optimization-and-Strict-Aliasing.html
   function Convert_Address_To_Process_Control_Block_Access is new
     Ada.Unchecked_Conversion
       (Virtual_Address_T,
        Process_Control_Block_Access);

   procedure Deallocate_Process
     (Process : in out Process_Control_Block_T; Result : out Function_Result);

   function Get_Process_SATP
     (Process : Process_Control_Block_T) return Unsigned_64
   is (RISCV.Create_SATP
         (Address (Process.Memory_Space.Base_Page_Table_Addr),
          Process.Memory_Space.Address_Space_ID));

   procedure Exit_Process
     (Process : in out Process_Control_Block_T; Result : out Function_Result);

   procedure Find_Process_With_Id
     (Process_Id : Process_Id_T;
      Process    : out Process_Control_Block_Access;
      Result     : out Function_Result);

   procedure Allocate_Process_Id
     (New_Id : out Process_Id_T; Result : out Function_Result);

private
   Logging_Tags : constant Log_Tags := [Log_Tag_Processes];

   Next_Process_Id     : Process_Id_T := 1;
   Process_Id_Spinlock : Spinlock_T;

   --  16KiB process kernel stack starting size.
   Process_Kernel_Stack_Size   : constant := 4 * 16#1000#;
   --  16KiB process user stack starting size.
   Process_Stack_Starting_Size : constant := 4 * 16#1000#;
   --  4MiB process heap starting size.
   Process_Heap_Starting_Size  : constant := 1024 * 16#1000#;

   procedure Allocate_And_Map_New_Process_Stack
     (New_Process : out Process_Control_Block_T; Result : out Function_Result);

   procedure Allocate_And_Map_New_Process_Kernel_Stack
     (New_Process : in out Process_Control_Block_T;
      Result      : out Function_Result);

   procedure Allocate_And_Map_New_Process_Heap
     (New_Process : out Process_Control_Block_T; Result : out Function_Result);

   procedure Deallocate_Process_Resources
     (Process : in out Process_Control_Block_T; Result : out Function_Result);

   procedure Deallocate_Process_Heap
     (Process : in out Process_Control_Block_T; Result : out Function_Result);

   procedure Get_Process_Kernel_Stack_Virtual_Address
     (Process_Id             : Process_Id_T;
      Kernel_Stack_Virt_Addr : out Virtual_Address_T;
      Result                 : out Function_Result);

end Processes;
