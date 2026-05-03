-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Memory;

package body Devices.Ramdisk is
   function Is_Valid_Sector_Index
     (Device : Device_T; Sector_Index : Unsigned_64) return Boolean is
   begin
      Sector_Limit : constant Unsigned_64 :=
        Unsigned_64 (Device.Memory_Size) / Ramdisk_Sector_Size;

      return Sector_Index < Sector_Limit;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Is_Valid_Sector_Index");
         return False;
   end Is_Valid_Sector_Index;

   procedure Read_Sector_Unlocked
     (Device               : Device_T;
      Sector_Index         : Unsigned_64;
      Data_Virtual_Address : Virtual_Address_T;
      Result               : out Function_Result) is
   begin
      if not Is_Valid_Sector_Index (Device, Sector_Index) then
         Log_Error
           ("Read_Sector: Sector index out of bounds: " & Sector_Index'Image);

         Result := Sector_Out_Of_Bounds;
         return;
      end if;

      Sector_Address : constant Virtual_Address_T :=
        Device.Virtual_Address
        + Storage_Offset (Sector_Index * Ramdisk_Sector_Size);

      Log_Debug
        ("Devices.Ramdisk.Read_Sector: "
         & ASCII.LF
         & "  Device VA: "
         & Device.Virtual_Address'Image
         & ASCII.LF
         & "  Sector Index: "
         & Sector_Index'Image
         & ASCII.LF
         & "  Sector Address: "
         & Sector_Address'Image
         & ASCII.LF
         & "  Data VA: "
         & Data_Virtual_Address'Image,
         Logging_Tags_Ramdisk);

      Memory.Copy (Data_Virtual_Address, Sector_Address, Ramdisk_Sector_Size);

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Read_Sector");
         Result := Constraint_Exception;
   end Read_Sector_Unlocked;

   procedure Read_Sector
     (Device               : in out Device_T;
      Sector_Index         : Unsigned_64;
      Data_Virtual_Address : Virtual_Address_T;
      Result               : out Function_Result) is
   begin
      Acquire_Spinlock (Device.Spinlock);
      Read_Sector_Unlocked
        (Device, Sector_Index, Data_Virtual_Address, Result);
      Release_Spinlock (Device.Spinlock);
   end Read_Sector;

   procedure Write_Sector_Unlocked
     (Device               : Device_T;
      Sector_Index         : Unsigned_64;
      Data_Virtual_Address : Virtual_Address_T;
      Result               : out Function_Result) is
   begin
      if not Is_Valid_Sector_Index (Device, Sector_Index) then
         Log_Error
           ("Write_Sector: Sector index out of bounds: " & Sector_Index'Image);

         Result := Sector_Out_Of_Bounds;
         return;
      end if;

      Sector_Address : constant Virtual_Address_T :=
        Device.Virtual_Address
        + Storage_Offset (Sector_Index * Ramdisk_Sector_Size);

      Log_Debug
        ("Devices.Ramdisk.Write_Sector: "
         & ASCII.LF
         & "  Device VA: "
         & Device.Virtual_Address'Image
         & ASCII.LF
         & "  Sector Index: "
         & Sector_Index'Image
         & ASCII.LF
         & "  Sector Address: "
         & Sector_Address'Image
         & ASCII.LF
         & "  Data VA: "
         & Data_Virtual_Address'Image,
         Logging_Tags_Ramdisk);

      Memory.Copy (Sector_Address, Data_Virtual_Address, Ramdisk_Sector_Size);

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Write_Sector");
         Result := Constraint_Exception;
   end Write_Sector_Unlocked;

   procedure Write_Sector
     (Device               : in out Device_T;
      Sector_Index         : Unsigned_64;
      Data_Virtual_Address : Virtual_Address_T;
      Result               : out Function_Result) is
   begin
      Acquire_Spinlock (Device.Spinlock);
      Write_Sector_Unlocked
        (Device, Sector_Index, Data_Virtual_Address, Result);
      Release_Spinlock (Device.Spinlock);
   end Write_Sector;

end Devices.Ramdisk;
