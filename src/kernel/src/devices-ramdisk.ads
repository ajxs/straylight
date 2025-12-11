-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Function_Results; use Function_Results;

-------------------------------------------------------------------------------
--  The ramdisk 'device' provides an interface for reading/writing sectors
--  from/to a mapped memory region as though it were a block storage device.
--
--  To use it, you can create a device entry in the System_Devices array
--  as follows, and point to it with a mounted filesystem:
--
--      System_Devices (5) :=
--        (Device_Class       => Device_Class_Storage,
--         Device_Bus         => Device_Bus_Memory_Mapped,
--         Memory_Size        => Ramdisk_Region_Size,
--         Virtual_Address    => Device_Virtual_Address,
--         Physical_Address   => Ramdisk_Address,
--         Interrupt_Line     => 0,
--         Interrupt_Priority => 0,
--         Spinlock           => Locks.Null_Spinlock,
--         Record_Used        => True);
--
--      Current_System_State.Mounted_Filesystems (3) :=
--        (Filesystem_Type              => Filesystem_Type_UStar,
--         Filesystem_Meta_Info_Address => Null_Address,
--         Filesystem_Meta_Info_Size    => 0,
--         Device                       => Boot_Ramdisk_Device'Access);
--
-------------------------------------------------------------------------------

package Devices.Ramdisk is
   pragma Preelaborate;

   procedure Read_Sector
     (Device               : Device_T;
      Sector_Index         : Unsigned_64;
      Data_Virtual_Address : Virtual_Address_T;
      Result               : out Function_Result);

private
   Logging_Tags_Ramdisk : constant Log_Tags := [Log_Tag_Devices_Ramdisk];

end Devices.Ramdisk;
