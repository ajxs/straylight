-------------------------------------------------------------------------------
--  Copyright (c) 2026, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Boot.Device_Discovery;           use Boot.Device_Discovery;
with Devices;                         use Devices;
with Devices.Attached_System_Devices; use Devices.Attached_System_Devices;
with Devices.Virtio.Block;
with Filesystems;                     use Filesystems;
with Filesystems.FAT;
with Filesystems.Root;                use Filesystems.Root;
with Filesystems.UStar;
with Function_Results;                use Function_Results;
with Logging;                         use Logging;
with Memory.Physical;                 use Memory.Physical;
with Memory.Virtual;                  use Memory.Virtual;

package body Boot.Filesystem_Setup is
   --  Set by Initialise_Filesystem. Mount_Disk_Filesystems adds a node under
   --  it for each volume.
   Volumes_Filesystem_Node_Index : Filesystem_Node_Index_T := 0;

   function Can_Device_Be_Mounted_In_Filesystem
     (Device : Device_T) return Boolean
   is (Device.Driver_Type
       in Driver_Type_UART
        | Driver_Type_Virtio_Block
        | Driver_Type_Virtio_GPU);

   ----------------------------------------------------------------------------
   --  Adds a node under /Devices for each device that userspace can open,
   --  grouped by the device class, e.g. /Devices/Serial/0.
   ----------------------------------------------------------------------------
   procedure Populate_Device_Nodes
     (Devices_Node_Index : Filesystem_Node_Index_T)
   is
      --  Each device type's directory node, or 0 until it's created.
      Directory_Node_Indexes :
        array (Driver_Type_T) of Filesystem_Node_Index_T := [others => 0];

      --  The number of each device type's next node.
      Next_Device_Numbers : array (Driver_Type_T) of Natural := [others => 0];

      Device_Node_Index : Filesystem_Node_Index_T := 0;

      Result : Function_Result := Unset;
   begin
      for Device of
        System_Devices (System_Devices'First .. Attached_System_Device_Count)
        when Can_Device_Be_Mounted_In_Filesystem (Device)
      loop
         Driver_Type : constant Driver_Type_T := Device.Driver_Type;

         Number_Image : constant String :=
           Next_Device_Numbers (Driver_Type)'Image;

         Device_Name : constant Filesystem_Path_T :=
           Number_Image (2 .. Number_Image'Last);

         Directory_Name : constant Filesystem_Path_T :=
           (case Driver_Type is
              when Driver_Type_UART         => "Serial",
              when Driver_Type_Virtio_Block => "Disks",
              when Driver_Type_Virtio_GPU   => "Graphics",
              when others                   => "");

         if Directory_Node_Indexes (Driver_Type) = 0 then
            Add_Filesystem_Node_To_Root_Filesystem
              (Filesystems.System_Root_Filesystem,
               Directory_Name,
               Devices_Node_Index,
               Directory_Node_Indexes (Driver_Type),
               Result,
               Filesystem_Node_Type_Directory);
            if Is_Error (Result) then
               return;
            end if;
         end if;

         Add_Filesystem_Node_To_Root_Filesystem
           (Filesystems.System_Root_Filesystem,
            Device_Name,
            Directory_Node_Indexes (Driver_Type),
            Device_Node_Index,
            Result,
            Filesystem_Node_Type_Device,
            Mounted_Device => Device'Access);
         if Is_Error (Result) then
            return;
         end if;

         pragma
           Debug
             (Debug_Boot,
              Log_Debug
                ("Added device node: path=/Devices/"
                 & Directory_Name
                 & "/"
                 & Device_Name));

         Next_Device_Numbers (Driver_Type) :=
           Next_Device_Numbers (Driver_Type) + 1;
      end loop;
   exception
      when Constraint_Error =>
         Panic_Constraint_Error;
   end Populate_Device_Nodes;

   procedure Initialise_Filesystem is
      Result : Function_Result := Unset;

      Root_Filesystem_Node_Index    : Filesystem_Node_Index_T := 0;
      Devices_Filesystem_Node_Index : Filesystem_Node_Index_T := 0;
      Console_Filesystem_Node_Index : Filesystem_Node_Index_T := 0;
   begin
      pragma Debug (Debug_Boot, Log_Debug ("Initialising filesystem..."));

      Filesystems.Mounted_Filesystems :=
        [others =>
           (Filesystem_Type              => Filesystem_Type_None,
            Filesystem_Meta_Info_Address => Null_Address,
            Filesystem_Meta_Info_Size    => 0,
            Device                       => null)];

      Filesystems.Mounted_Filesystems (1) :=
        (Filesystem_Type              => Filesystem_Type_Root,
         Filesystem_Meta_Info_Address => Null_Address,
         Filesystem_Meta_Info_Size    => 0,
         Device                       => Root_Filesystem_Memory_Device);

      Filesystems.System_Root_Filesystem :=
        Filesystems.Mounted_Filesystems (1)'Access;

      Initialise_Root_Filesystem (Filesystems.System_Root_Filesystem, Result);

      Add_Filesystem_Node_To_Root_Filesystem
        (Filesystems.System_Root_Filesystem,
         "/",
         0,
         Root_Filesystem_Node_Index,
         Result,
         Filesystem_Node_Type_Directory);
      if Is_Error (Result) then
         Panic;
      end if;

      Add_Filesystem_Node_To_Root_Filesystem
        (Filesystems.System_Root_Filesystem,
         "Devices",
         Root_Filesystem_Node_Index,
         Devices_Filesystem_Node_Index,
         Result,
         Filesystem_Node_Type_Directory);
      if Is_Error (Result) then
         Panic;
      end if;

      Add_Filesystem_Node_To_Root_Filesystem
        (Filesystems.System_Root_Filesystem,
         "Volumes",
         Root_Filesystem_Node_Index,
         Volumes_Filesystem_Node_Index,
         Result,
         Filesystem_Node_Type_Directory);
      if Is_Error (Result) then
         Panic;
      end if;

      Add_Filesystem_Node_To_Root_Filesystem
        (Filesystems.System_Root_Filesystem,
         "Console",
         Devices_Filesystem_Node_Index,
         Console_Filesystem_Node_Index,
         Result,
         Filesystem_Node_Type_Device,
         Mounted_Device => Console_Device);
      if Is_Error (Result) then
         Panic;
      end if;

      --  The console's node is added first, so that it exists even if the
      --  root filesystem runs out of nodes for the other devices.
      Populate_Device_Nodes (Devices_Filesystem_Node_Index);

      pragma Debug (Debug_Boot, Log_Debug ("Initialised filesystem."));
   end Initialise_Filesystem;

   ----------------------------------------------------------------------------
   --  Mounts a disk's filesystem in the next free mounted filesystem slot,
   --  and adds its node at /Volumes/<Volume_Number>.
   ----------------------------------------------------------------------------
   procedure Mount_Volume
     (Filesystem_Type : Filesystem_Type_T;
      Device          : Device_Access;
      Volume_Number   : Natural;
      Result          : out Function_Result)
   is
      Filesystem_Index  : Natural := 0;
      Volume_Node_Index : Filesystem_Node_Index_T := 0;
   begin
      for Index in
        Filesystems.Mounted_Filesystems'Range
        when Filesystems.Mounted_Filesystems (Index).Filesystem_Type
        = Filesystem_Type_None
      loop
         Filesystem_Index := Index;
         exit;
      end loop;

      if Filesystem_Index = 0 then
         Log_Error ("No free mounted filesystem slot available");
         Result := No_Free_Entries;
         return;
      end if;

      Number_Image : constant String := Volume_Number'Image;

      Volume_Name : constant Filesystem_Path_T :=
        Number_Image (2 .. Number_Image'Last);

      --  The node is added before the slot is filled, so that the slot stays
      --  free if the root filesystem has no free nodes.
      Add_Filesystem_Node_To_Root_Filesystem
        (Filesystems.System_Root_Filesystem,
         Volume_Name,
         Volumes_Filesystem_Node_Index,
         Volume_Node_Index,
         Result,
         Filesystem_Node_Type_Mounted_Filesystem,
         Mounted_Filesystem =>
           Filesystems.Mounted_Filesystems (Filesystem_Index)'Access);
      if Is_Error (Result) then
         return;
      end if;

      Filesystems.Mounted_Filesystems (Filesystem_Index) :=
        (Filesystem_Type => Filesystem_Type, Device => Device, others => <>);

      pragma
        Debug
          (Debug_Boot,
           Log_Debug ("Mounted volume: path=/Volumes/" & Volume_Name));

      Result := Success;
   exception
      when Constraint_Error =>
         Panic_Constraint_Error;
   end Mount_Volume;

   procedure Mount_Disk_Filesystems is
      --  The device writes the sector to a buffer by its physical address.
      --  The smallest physical allocation is a page.
      Sector_Buffer_Size : constant := 16#1000#;

      Sector_Buffer_Physical_Address : Physical_Address_T :=
        Null_Physical_Address;

      --  The number of volumes mounted so far, which is also the next
      --  volume's number.
      Volume_Count : Natural := 0;

      Result : Function_Result := Unset;
   begin
      Allocate_Physical_Memory
        (Sector_Buffer_Size, Sector_Buffer_Physical_Address, Result);
      if Is_Error (Result) then
         Panic ("Error allocating the disk sector buffer");
      end if;

      Sector_Buffer_Address : constant Virtual_Address_T :=
        Get_Physical_Address_Virtual_Mapping (Sector_Buffer_Physical_Address);

      for Device of
        System_Devices (System_Devices'First .. Attached_System_Device_Count)
        when Device.Driver_Type = Driver_Type_Virtio_Block
      loop
         Devices.Virtio.Block.Read_Sector
           (Init_Process.all,
            Device.Common,
            Device.Virtio_Block,
            Sector_Buffer_Physical_Address,
            0,
            Result);
         if Is_Error (Result) then
            Panic ("Error reading the first sector of a disk");
         end if;

         Filesystem_Type : constant Filesystem_Type_T :=
           (if Filesystems.FAT.Is_FAT_Boot_Sector (Sector_Buffer_Address)
            then Filesystem_Type_FAT
            elsif Filesystems.UStar.Is_UStar_Header (Sector_Buffer_Address)
            then Filesystem_Type_UStar
            else Filesystem_Type_None);

         if Filesystem_Type = Filesystem_Type_None then
            pragma
              Debug
                (Debug_Boot,
                 Log_Debug ("Disk has no known filesystem, so it's skipped."));
         else
            Mount_Volume
              (Filesystem_Type, Device'Access, Volume_Count, Result);
            exit when Is_Error (Result);

            Volume_Count := Volume_Count + 1;
         end if;
      end loop;

      Free_Physical_Memory (Sector_Buffer_Physical_Address, Result);
      if Is_Error (Result) then
         Panic ("Error freeing the disk sector buffer");
      end if;
   exception
      when Constraint_Error =>
         Panic_Constraint_Error;
   end Mount_Disk_Filesystems;

end Boot.Filesystem_Setup;
