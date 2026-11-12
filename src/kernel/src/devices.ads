-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Interfaces;              use Interfaces;
with System;                  use System;
with System.Storage_Elements; use System.Storage_Elements;

with Locks;  use Locks;
with Memory; use Memory;

package Devices
  with Preelaborate
is
   --  Identifies the driver that operates a device.
   --  Each driver owns the state record stored for its devices in
   --  Devices.Attached_System_Devices.
   type Driver_Type_T is
     (Driver_Type_None,
      Driver_Type_PLIC,
      Driver_Type_UART,
      Driver_Type_Console,
      Driver_Type_Ramdisk,
      Driver_Type_Virtio_Block,
      Driver_Type_Virtio_GPU);

   --  This type is used to represent the *kernel's* internal representation
   --  of a storage device sector. It's independent of any particular device's
   --  specific implementation details.
   --  This abstraction supports a single, consistent type used to index
   --  sectors across all devices, regardless of the device's actual internal
   --  addressing scheme, block size, etc.
   subtype Sector_Index_T is Unsigned_64;

   --  The fields common to every device, regardless of its driver.
   type Device_Common_State_T is record
      Virtual_Address    : Virtual_Address_T;
      Physical_Address   : Physical_Address_T;
      Memory_Size        : Storage_Offset := 0;
      Interrupt_Line     : Integer := 0;
      Interrupt_Priority : Integer := 0;
      Spinlock           : Spinlock_T;
   end record;

end Devices;
