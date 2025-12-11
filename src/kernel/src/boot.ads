-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with System; use System;

with Addresses; use Addresses;
with Logging;   use Logging;
with Memory;    use Memory;
with Processes; use Processes;

package Boot is
   pragma Preelaborate;
   ----------------------------------------------------------------------------
   --  Main Ada entry point for the kernel.
   --  This procedure is jumped to from the boot assembly code, once the
   --  initial boot page tables have been set up, and supervisor address
   --  translation (paging) has been enabled.
   ----------------------------------------------------------------------------
   procedure Initialise_Kernel_Memory
     (Hart_Id : Integer; DTB_Address : Address)
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

   procedure Initialise_Hart (Hart_Id : Integer);

   procedure Initialise_Kernel_Address_Space;

   procedure Allocate_Kernel_Heap;

   procedure Allocate_Kernel_Page_Pool;

   procedure Initialise_Filesystem;

   procedure Initialise_Block_Cache;

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

   --  This is a one-page 'secondary stack' mapped into kernel space.
   --  This is used so that there's a mapped stack to switch to when enabling
   --  paging and jumping into the kernel's higher-half address space.
   --  It's required because a large number of early internal processes, such
   --  as allocating new process resources, require the kernel allocators to
   --  be functional, which in turn requires paging to be enabled.
   --  Ideally in the long term there will be a better solution for this.
   Boot_Secondary_Stack_Phys_Address : Physical_Address_T;

   --  This is the 'System Init Process', the first userspace program which is
   --  launched by the kernel once initialisation is complete.
   Init_Process : Process_Control_Block_Access := null;

   Boot_Secondary_Stack_Virtual_Address : constant Address :=
     System'To_Address (16#FFFF_FFE8_0000_0000#);

   Boot_Secondary_Stack_Size : constant := 16#1000#;
end Boot;
