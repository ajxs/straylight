-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Memory;

package body Devices.Ramdisk is
   function Is_Valid_Sector_Range
     (Device : Device_T; Start_Sector : Sector_Index_T; Sector_Count : Natural)
      return Boolean is
   begin
      Sector_Limit : constant Sector_Index_T :=
        Sector_Index_T (Device.Memory_Size / Ramdisk_Sector_Size);

      return
        Start_Sector <= Sector_Limit
        and then Sector_Index_T (Sector_Count) <= Sector_Limit - Start_Sector;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Is_Valid_Sector_Range");
         return False;
   end Is_Valid_Sector_Range;

   procedure Read_Sectors_Unlocked
     (Device               : Device_T;
      Start_Sector         : Sector_Index_T;
      Sector_Count         : Natural;
      Data_Virtual_Address : Virtual_Address_T;
      Result               : out Function_Result) is
   begin
      if Sector_Count = 0 then
         Log_Error ("Read_Sectors: Sector_Count is zero");
         Result := Invalid_Argument;
         return;
      end if;

      if not Is_Valid_Sector_Range (Device, Start_Sector, Sector_Count) then
         Log_Error
           ("Read_Sectors: Sector range out of bounds: " & Start_Sector'Image);

         Result := Sector_Out_Of_Bounds;
         return;
      end if;

      Start_Address : constant Virtual_Address_T :=
        Device.Virtual_Address
        + Storage_Offset (Start_Sector * Ramdisk_Sector_Size);

      Log_Debug
        ("Devices.Ramdisk.Read_Sectors: "
         & ASCII.LF
         & "  Device VA: "
         & Device.Virtual_Address'Image
         & ASCII.LF
         & "  Start Sector: "
         & Start_Sector'Image
         & ASCII.LF
         & "  Start Address: "
         & Start_Address'Image
         & ASCII.LF
         & "  Data VA: "
         & Data_Virtual_Address'Image,
         Logging_Tags_Ramdisk);

      Bytes_To_Copy : constant Natural := Ramdisk_Sector_Size * Sector_Count;

      Memory.Copy (Data_Virtual_Address, Start_Address, Bytes_To_Copy);

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Read_Sectors");
         Result := Constraint_Exception;
   end Read_Sectors_Unlocked;

   procedure Read_Sector
     (Device               : in out Device_T;
      Sector_Index         : Sector_Index_T;
      Data_Virtual_Address : Virtual_Address_T;
      Result               : out Function_Result) is
   begin
      Acquire_Spinlock (Device.Spinlock);
      Read_Sectors_Unlocked
        (Device, Sector_Index, 1, Data_Virtual_Address, Result);
      Release_Spinlock (Device.Spinlock);
   end Read_Sector;

   procedure Write_Sectors_Unlocked
     (Device               : Device_T;
      Start_Sector         : Sector_Index_T;
      Sector_Count         : Natural;
      Data_Virtual_Address : Virtual_Address_T;
      Result               : out Function_Result) is
   begin
      if Sector_Count = 0 then
         Log_Error ("Write_Sectors: Sector_Count is zero");
         Result := Invalid_Argument;
         return;
      end if;

      if not Is_Valid_Sector_Range (Device, Start_Sector, Sector_Count) then
         Log_Error
           ("Write_Sectors: Sector range out of bounds: "
            & Start_Sector'Image);

         Result := Sector_Out_Of_Bounds;
         return;
      end if;

      Start_Address : constant Virtual_Address_T :=
        Device.Virtual_Address
        + Storage_Offset (Start_Sector * Ramdisk_Sector_Size);

      Log_Debug
        ("Devices.Ramdisk.Write_Sectors: "
         & ASCII.LF
         & "  Device VA: "
         & Device.Virtual_Address'Image
         & ASCII.LF
         & "  Start Sector: "
         & Start_Sector'Image
         & ASCII.LF
         & "  Sector Address: "
         & Start_Address'Image
         & ASCII.LF
         & "  Data VA: "
         & Data_Virtual_Address'Image,
         Logging_Tags_Ramdisk);

      Bytes_To_Copy : constant Natural := Ramdisk_Sector_Size * Sector_Count;

      Memory.Copy (Start_Address, Data_Virtual_Address, Bytes_To_Copy);

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Write_Sectors");
         Result := Constraint_Exception;
   end Write_Sectors_Unlocked;

   procedure Write_Sector
     (Device               : in out Device_T;
      Sector_Index         : Sector_Index_T;
      Data_Virtual_Address : Virtual_Address_T;
      Result               : out Function_Result) is
   begin
      Acquire_Spinlock (Device.Spinlock);
      Write_Sectors_Unlocked
        (Device, Sector_Index, 1, Data_Virtual_Address, Result);
      Release_Spinlock (Device.Spinlock);
   end Write_Sector;

   procedure Read_Sectors
     (Device               : in out Device_T;
      Start_Sector         : Sector_Index_T;
      Sector_Count         : Natural;
      Data_Virtual_Address : Virtual_Address_T;
      Result               : out Function_Result) is
   begin
      Acquire_Spinlock (Device.Spinlock);
      Read_Sectors_Unlocked
        (Device, Start_Sector, Sector_Count, Data_Virtual_Address, Result);
      Release_Spinlock (Device.Spinlock);
   end Read_Sectors;

   procedure Write_Sectors
     (Device               : in out Device_T;
      Start_Sector         : Sector_Index_T;
      Sector_Count         : Natural;
      Data_Virtual_Address : Virtual_Address_T;
      Result               : out Function_Result) is
   begin
      Acquire_Spinlock (Device.Spinlock);
      Write_Sectors_Unlocked
        (Device, Start_Sector, Sector_Count, Data_Virtual_Address, Result);
      Release_Spinlock (Device.Spinlock);
   end Write_Sectors;

end Devices.Ramdisk;
