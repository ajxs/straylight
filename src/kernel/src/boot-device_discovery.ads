-------------------------------------------------------------------------------
--  Copyright (c) 2026, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Devices.Attached_System_Devices; use Devices.Attached_System_Devices;
with Devicetree;                      use Devicetree;

-------------------------------------------------------------------------------
--  This private package creates the system's devices from the devices found
--  in the Devicetree, then maps and initialises them.
-------------------------------------------------------------------------------

private package Boot.Device_Discovery
  with Preelaborate
is
   Maximum_Devicetree_Devices : constant := 16;

   --  The devices found by parsing the Devicetree blob received at boot.
   --  Kernel_Main fills this list from Parse_Devicetree. These values are
   --  copied out of the DTB by the parser, so they stay valid after the DTB's
   --  memory is freed.
   Devicetree_Devices      :
     Devicetree_Device_Array_T (1 .. Maximum_Devicetree_Devices);
   Devicetree_Device_Count : Natural := 0;

   Maximum_Devicetree_Harts : constant := 16;

   --  Information about the available harts in the Devicetree.
   --  Kernel_Main fills this list.
   --  Start_Non_Boot_Harts starts only these harts, and Create_PLIC_Device
   --  records each one's supervisor-mode PLIC context.
   Devicetree_Harts : Devicetree_Hart_Array_T (1 .. Maximum_Devicetree_Harts);

   Devicetree_Hart_Count : Natural := 0;

   --  The devices that the rest of Boot uses, set by Initialise_Devices. Each
   --  one is null until its device is created. The interrupt controller and
   --  the GPU are Interrupt_Controller and Graphics_Device in
   --  Devices.Attached_System_Devices, since code outside Boot uses them.
   UART_Device                   : Device_Access := null;
   Console_Device                : Device_Access := null;
   Root_Filesystem_Memory_Device : Device_Access := null;

   --  An array of attached block devices.
   --  The order of the array doesn't matter. What filesystems are mounted is
   --  determined at mount-time.
   --  Currently limited to two devices, pending a more comprehensive device
   --  discovery implementation.
   Block_Devices : array (1 .. 2) of Device_Access := [others => null];

   ----------------------------------------------------------------------------
   --  Creates a device for each device found in the Devicetree, then maps and
   --  initialises every device.
   ----------------------------------------------------------------------------
   procedure Initialise_Devices;

end Boot.Device_Discovery;
