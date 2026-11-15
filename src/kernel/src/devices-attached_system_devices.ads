-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Devices.Console;
with Devices.PLIC;
with Devices.UART;
with Devices.Virtio.Block;
with Devices.Virtio.Graphics;

-------------------------------------------------------------------------------
--  This package contains an in-memory list of all devices in the system.
--  The list tracks the individual working state of all of the devices.
-------------------------------------------------------------------------------

package Devices.Attached_System_Devices
  with Preelaborate
is
   ----------------------------------------------------------------------------
   --  This type represents a device attached to the system.
   --  It contains the common state and driver-specific state of the device.
   ----------------------------------------------------------------------------
   type Device_T;

   type Device_Access is access all Device_T;

   type Device_T (Driver_Type : Driver_Type_T := Driver_Type_None) is record
      Common : Device_Common_State_T;

      --  The interrupt controller that this device's interrupt line belongs
      --  to, or null if the device doesn't raise interrupts.
      Interrupt_Parent : Device_Access := null;

      case Driver_Type is
         when Driver_Type_PLIC =>
            PLIC : Devices.PLIC.PLIC_Device_State_T;

         when Driver_Type_UART =>
            UART : Devices.UART.UART_Device_State_T;

         when Driver_Type_Console =>
            Console : Devices.Console.Console_Device_State_T;

         when Driver_Type_Virtio_Block =>
            Virtio_Block : Devices.Virtio.Block.Virtio_Block_Device_State_T;

         when Driver_Type_Virtio_GPU =>
            Virtio_GPU : Devices.Virtio.Graphics.Virtio_GPU_Device_State_T;

         when Driver_Type_None | Driver_Type_Ramdisk =>
            null;
      end case;
   end record;

   type Device_Array_T is array (Positive range <>) of aliased Device_T;

   System_Devices : Device_Array_T (1 .. 16);

   --  How many System_Devices entries are in use. Entries are used in order,
   --  from System_Devices'First.
   --  Currently this isn't locked, which is only safe because the boot hart
   --  adds every device before the other harts start. If devices are ever
   --  added later, this will need a lock.
   Attached_System_Device_Count : Natural := 0;

   --  The interrupt controller used to claim external interrupts.
   --  Currently this interrupt serves every hart. At some point in the future,
   --  when compatibility for other systems is added, this will need to be
   --  updated to handle multiple interrupt controllers.
   Interrupt_Controller : Device_Access := null;

   --  The graphics device used to drive the display.
   --  Currently the system only supports a single output graphic device. At
   --  some point in the future, when compatibility for other systems is added,
   --  this implementation will need to be updated.
   Graphics_Device : Device_Access := null;

end Devices.Attached_System_Devices;
