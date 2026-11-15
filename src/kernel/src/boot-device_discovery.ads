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

   --  Each device's fixed slot in System_Devices.
   APIC_Device renames System_Devices (1);
   UART_Device renames System_Devices (2);
   Disk_Device renames System_Devices (3);
   Root_Filesystem_Memory_Device renames System_Devices (4);
   Disk_B_Device renames System_Devices (5);
   Graphics_Device renames System_Devices (6);
   Console_Device renames System_Devices (7);

   ----------------------------------------------------------------------------
   --  Creates a device for each device found in the Devicetree, then maps and
   --  initialises every device.
   ----------------------------------------------------------------------------
   procedure Initialise_Devices;

end Boot.Device_Discovery;
