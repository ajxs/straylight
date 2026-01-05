-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Interfaces; use Interfaces;
with System;     use System;

with Addresses;  use Addresses;
with Hart_State; use Hart_State;
with Logging;    use Logging;
with Processes;  use Processes;

package Boot is
   pragma Preelaborate;
   ----------------------------------------------------------------------------
   --  Main Ada entry point for the kernel.
   --  This procedure is jumped to from the boot assembly code, once the
   --  initial boot page tables have been set up, and supervisor address
   --  translation (paging) has been enabled.
   ----------------------------------------------------------------------------
   procedure Kernel_Main (Hart_Id : Integer; DTB_Address : Address)
   with
     No_Return,
     Export,
     Convention    => Assembler,
     External_Name => "boot_initialise_kernel_memory";

private
   Logging_Tags : constant Log_Tags := [Log_Tag_Boot];

   procedure Initialise_Kernel_Services
   with
     Export,
     Convention    => Assembler,
     External_Name => "boot_initialise_kernel_services";

   procedure Initialise_Physical_Memory_Manager;

   procedure Free_Boot_Memory;

   --  This function needs to be called before any kernel-level logging can be
   --  performed, as the logging spinlock relies on being able to determine the
   --  current hart id.
   procedure Initialise_Hart (Hart_Id : Integer);

   procedure Initialise_Filesystem;

   procedure Initialise_Graphics;

   ----------------------------------------------------------------------------
   --  Initialise the system's devices.
   --  Currently the kernel is 'hardcoded' to support QEMU's 'virt' device.
   --  In future this will be replaced with proper device discovery based upon
   --  the devicetree.
   ----------------------------------------------------------------------------
   procedure Initialise_Devices;

   procedure Initialise_Init_Process;

   procedure Start_Init_Process;

   procedure Start_Non_Boot_Harts;

   procedure Non_Boot_Hart_Entry (Hart_Id : Hart_Index_T)
   with
     Export,
     Convention    => Assembler,
     External_Name => "boot_non_boot_hart_entry";

   --  This is a one-page 'secondary stack' mapped into kernel space.
   --  This is used so that there's a mapped stack to switch to when enabling
   --  paging and jumping into the kernel's higher-half address space.
   --  It's required because a large number of early internal processes, such
   --  as allocating new process resources, require the kernel allocators to
   --  be functional, which in turn requires paging to be enabled.
   --  Ideally in the long term there will be a better solution for this.
   Boot_Secondary_Stack_Phys_Address_Base : Physical_Address_T;

   --  This is the 'System Init Process', the first userspace program which is
   --  launched by the kernel once initialisation is complete.
   Init_Process : Process_Control_Block_Access := null;

   Boot_Secondary_Stack_Virtual_Address_Base : constant Address :=
     System'To_Address (16#FFFF_FFE8_0000_0000#);

   Boot_Secondary_Stack_Size : constant := 16#1000#;

   function Get_Boot_Secondary_Stack_Virtual_Address
     (Hart_Id : Integer) return Virtual_Address_T
   with Inline, Pure_Function;

   function Get_Boot_Secondary_Stack_Physical_Address
     (Hart_Id : Integer) return Physical_Address_T
   with Inline, Pure_Function;

   --  Loads the provided SATP, stack pointer, and jumps to the specified
   --  return address. This is used to switch into the kernel's address space
   --  from the boot code.
   procedure Switch_To_Kernel_Address_Space
     (SATP           : Unsigned_64;
      Stack_Pointer  : Virtual_Address_T;
      Return_Address : Virtual_Address_T)
   with
     No_Return,
     Import,
     Convention    => Assembler,
     External_Name => "boot_switch_to_kernel_address_space";

   procedure Park_Non_Boot_Hart
   with
     No_Return,
     Import,
     Convention    => Assembler,
     External_Name => "cpu_halt";

   procedure Wait_For_All_Harts_To_Start;

end Boot;
