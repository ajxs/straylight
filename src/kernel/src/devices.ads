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

   type Device_Bus_T is (Device_Bus_Memory_Mapped, Device_Bus_VirtIO_MMIO);

   type VirtIO_Device_Type_T is
     (VirtIO_Device_Type_Block,
      VirtIO_Device_Type_Graphics,
      VirtIO_Device_Type_Unknown);

   Maximum_VirtIO_Queue_Length : constant := 256;

   subtype VirtIO_Descriptor_Array_Index_T is Unsigned_16;

   type VirtIO_Descriptor_Status_Array_T is
     array (VirtIO_Descriptor_Array_Index_T) of Boolean;

   subtype VirtIO_Resource_Allocated_Addresses_T is
     Memory.Allocators.Memory_Allocation_Result;

   type VirtIO_Device_Request_Info_T is record
      Channel : Blocking_Channel_T := 0;
   end record;

   type Request_Info_Array_T is
     array (VirtIO_Descriptor_Array_Index_T) of VirtIO_Device_Request_Info_T;

   --  Block device requests require both the virtual and physical addresses
   --  of the request descriptor to properly set up the descriptor table.
   --  This structure holds both addresses for each request.
   type Device_Request_Array_Addresses_T is
     array (VirtIO_Descriptor_Array_Index_T)
     of VirtIO_Resource_Allocated_Addresses_T;

   type Device_Bus_Info_VirtIO_T
     (Device_Type : VirtIO_Device_Type_T := VirtIO_Device_Type_Unknown)
   is record
      Request_Info           : Request_Info_Array_T :=
        [others => (Channel => 0)];
      Request_Status_Array   : VirtIO_Resource_Allocated_Addresses_T;
      Request_Serviced_Index : VirtIO_Descriptor_Array_Index_T := 0;
      Q_Used                 : VirtIO_Resource_Allocated_Addresses_T;
      Q_Available            : VirtIO_Resource_Allocated_Addresses_T;
      Q_Descriptor           : VirtIO_Resource_Allocated_Addresses_T;
      Descriptor_Status      : VirtIO_Descriptor_Status_Array_T;

      case Device_Type is
         when VirtIO_Device_Type_Block =>
            --  This array holds the VirtIO block device request structures.
            --  This needs to be allocated by the kernel at runtime, because
            --  it needs to have a physical address for the device to access.
            Block_Request_Array_Addresses :
              VirtIO_Resource_Allocated_Addresses_T;

         when VirtIO_Device_Type_Graphics =>
            Resource_Id           : Unsigned_32 := 0;
            Framebuffer_Addresses : VirtIO_Resource_Allocated_Addresses_T;
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
         when Device_Bus_VirtIO_MMIO =>
            Bus_Info_VirtIO : Device_Bus_Info_VirtIO_T;

         when others =>
            null;
      end case;
   end record;

   --  type System_Device_Array is array (1 .. 16) of aliased Device_T;

   System_Devices : array (1 .. 16) of aliased Device_T;

private
   Logging_Tags : constant Log_Tags := [Log_Tag_Devices];

end Devices;
