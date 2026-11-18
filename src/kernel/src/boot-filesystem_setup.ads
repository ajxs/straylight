-------------------------------------------------------------------------------
--  Copyright (c) 2026, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
--  This private package builds the root filesystem at boot: the /Devices and
--  /Volumes directories, a node for each device, and a mount for each disk
--  that holds a recognised filesystem.
-------------------------------------------------------------------------------

private package Boot.Filesystem_Setup
  with Preelaborate
is
   procedure Initialise_Filesystem;

   ----------------------------------------------------------------------------
   --  Mounts each block device that holds a recognised filesystem at
   --  /Volumes/<N>, numbered from 0 in System_Devices order.
   --  The filesystem type is detected by reading the device's first sector.
   --  Reading from a block device needs a process context, so this is called
   --  from the init process, before anything is read from a disk.
   ----------------------------------------------------------------------------
   procedure Mount_Disk_Filesystems;

end Boot.Filesystem_Setup;
