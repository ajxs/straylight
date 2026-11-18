-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Function_Results; use Function_Results;

-------------------------------------------------------------------------------
--  The ramdisk 'device' provides an interface for reading/writing sectors
--  from/to a mapped memory region as though it were a block storage device.
--
--  To use it, create a ramdisk device with
--  Boot.Device_Discovery.Create_Ramdisk_Device, then attach a mounted
--  filesystem to it, in a free Mounted_Filesystems slot:
--
--      Create_Ramdisk_Device
--        (Ramdisk_Device,
--         Ramdisk_Address,
--         Ramdisk_Region_Size,
--         Device_Virtual_Address);
--
--      Mounted_Filesystems (Free_Index) :=
--        (Filesystem_Type              => Filesystem_Type_UStar,
--         Filesystem_Meta_Info_Address => Null_Address,
--         Filesystem_Meta_Info_Size    => 0,
--         Device                       => Ramdisk_Device);
--
-------------------------------------------------------------------------------

package Devices.Ramdisk
  with Preelaborate
is
   procedure Read_Sector
     (Device               : in out Device_Common_State_T;
      Sector_Index         : Sector_Index_T;
      Data_Virtual_Address : Virtual_Address_T;
      Result               : out Function_Result);

   procedure Write_Sector
     (Device               : in out Device_Common_State_T;
      Sector_Index         : Sector_Index_T;
      Data_Virtual_Address : Virtual_Address_T;
      Result               : out Function_Result);

   procedure Read_Sectors
     (Device               : in out Device_Common_State_T;
      Start_Sector         : Sector_Index_T;
      Sector_Count         : Natural;
      Data_Virtual_Address : Virtual_Address_T;
      Result               : out Function_Result);

   procedure Write_Sectors
     (Device               : in out Device_Common_State_T;
      Start_Sector         : Sector_Index_T;
      Sector_Count         : Natural;
      Data_Virtual_Address : Virtual_Address_T;
      Result               : out Function_Result);

end Devices.Ramdisk;
