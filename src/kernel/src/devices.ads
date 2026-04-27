-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Interfaces;              use Interfaces;
with System;                  use System;
with System.Storage_Elements; use System.Storage_Elements;

with Locks;     use Locks;
with Logging;   use Logging;
with Memory;    use Memory;
with Memory.Allocators;
with Processes; use Processes;

package Devices is
   pragma Preelaborate;

   type Device_Class_T is
     (Device_Class_None,
      Device_Class_Graphics,
      Device_Class_Network,
      Device_Class_Interrupt_Controller,
      Device_Class_Serial,
      Device_Class_Storage);

   type Device_Bus_T is (Device_Bus_Memory_Mapped, Device_Bus_Virtio_MMIO);

   type Virtio_Device_Type_T is
     (Virtio_Device_Type_Block,
      Virtio_Device_Type_Graphics,
      Virtio_Device_Type_Unknown);

   Maximum_Virtio_Queue_Length : constant := 256;

   subtype Virtio_Descriptor_Array_Index_T is Unsigned_16;

   type Virtio_Descriptor_Status_Array_T is
     array (Virtio_Descriptor_Array_Index_T) of Boolean;

   --  The kernel is mapped into the higher-half, and needs to operate on
   --  virtual addresses. However, devices operate on physical addresses, so
   --  the kernel needs to maintain both virtual and physical addresses for
   --  Virtio devices resources.
   subtype Virtio_Resource_Allocated_Addresses_T is
     Memory.Allocators.Memory_Allocation_Result;

   type Virtio_Device_Request_Info_T is record
      Channel : Blocking_Channel_T := 0;
   end record;

   type Request_Info_Array_T is
     array (Virtio_Descriptor_Array_Index_T) of Virtio_Device_Request_Info_T;

   --  Feature bits for Virtio devices are split into multiple 'pages' of
   --  32-bit feature bitmaps. The first page (Page 0) is accessed by writing
   --  0 to the DeviceFeaturesSel register, the second page (Page 1) is
   --  accessed by writing 1, and so on.
   --  Currently only two pages are defined in the Virtio specification.
   --  Refer to section 4.2.2 of the Virtio specification for more details.
   type Virtio_Device_Features_Pages_T is array (0 .. 1) of Unsigned_32;

   type Device_Bus_Info_Virtio_T
     (Device_Type : Virtio_Device_Type_T := Virtio_Device_Type_Unknown)
   is record
      --  Defines which features the driver supports.
      --  During feature negotiation, this field will be combined with the
      --  device's supported features to determine the final set of active
      --  features.
      Driver_Features        : Virtio_Device_Features_Pages_T := [others => 0];
      Request_Info           : Request_Info_Array_T :=
        [others => (Channel => 0)];
      Request_Status_Array   : Virtio_Resource_Allocated_Addresses_T;
      Request_Serviced_Index : Virtio_Descriptor_Array_Index_T := 0;
      Q_Used                 : Virtio_Resource_Allocated_Addresses_T;
      Q_Available            : Virtio_Resource_Allocated_Addresses_T;
      Q_Descriptor           : Virtio_Resource_Allocated_Addresses_T;
      Descriptor_Status      : Virtio_Descriptor_Status_Array_T;

      case Device_Type is
         when Virtio_Device_Type_Block =>
            --  This array holds the Virtio block device request structures.
            --  This needs to be allocated by the kernel at runtime, because
            --  it needs to have a physical address for the device to access.
            Block_Request_Array_Addresses :
              Virtio_Resource_Allocated_Addresses_T;

         when Virtio_Device_Type_Graphics =>
            Resource_Id           : Unsigned_32 := 0;
            Framebuffer_Addresses : Virtio_Resource_Allocated_Addresses_T;
            Framebuffer_Width     : Unsigned_32 := 0;
            Framebuffer_Height    : Unsigned_32 := 0;

         when others =>
            null;
      end case;
   end record;

   type Device_T (Device_Bus : Device_Bus_T := Device_Bus_Memory_Mapped) is
   record
      Device_Class       : Device_Class_T := Device_Class_None;
      Virtual_Address    : Virtual_Address_T;
      Physical_Address   : Physical_Address_T;
      Memory_Size        : Storage_Offset := 0;
      Record_Used        : Boolean := False;
      Interrupt_Line     : Integer := 0;
      Interrupt_Priority : Integer := 0;
      Spinlock           : Spinlock_T;

      case Device_Bus is
         when Device_Bus_Virtio_MMIO =>
            Bus_Info_Virtio : Device_Bus_Info_Virtio_T;

         when others =>
            null;
      end case;
   end record;

   type Device_Access is access all Device_T;

   System_Devices : array (1 .. 16) of aliased Device_T;

private
   Logging_Tags : constant Log_Tags := [Log_Tag_Devices];

end Devices;
