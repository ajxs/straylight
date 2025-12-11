-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Function_Results; use Function_Results;

package Devices.VirtIO is
   pragma Preelaborate;

   procedure Initialise_MMIO_Device
     (Device : in out Device_T; Result : out Function_Result);

   procedure Acknowledge_Interrupt
     (Device : in out Device_T; Result : out Function_Result);

   --  @TODO: Investigate whether this should be moved inside the
   --  Initialise_MMIO_Device procedure.
   procedure Allocate_VirtIO_Device_Resources
     (Device : in out Device_T; Result : out Function_Result);

private
   Logging_Tags_VirtIO : constant Log_Tags := [Log_Tag_Devices_VirtIO];

   VIRTQ_DESC_F_NEXT  : constant := 1; --  Chained with another descriptor.
   VIRTQ_DESC_F_WRITE : constant := 2; --  Device writes, as opposed to read.

   BLOCK_SIZE : constant := 512;

   type VirtIO_Status_Byte_Array_T is
     array (VirtIO_Descriptor_Array_Index_T) of Unsigned_8
   with Convention => C;

   subtype Descriptor_Index_T is Unsigned_16;

   type Virtqueue_Descriptor_T is record
      Address : Physical_Address_T := Null_Physical_Address;
      Length  : Unsigned_32 := 0;
      Flags   : Unsigned_16 := 0;
      Next    : Unsigned_16 := 0;
   end record;

   type Virtqueue_Descriptor_Array is
     array (VirtIO_Descriptor_Array_Index_T) of Virtqueue_Descriptor_T
   with Convention => C;

   type Virtqueue_Available_Descriptor_Array is
     array (VirtIO_Descriptor_Array_Index_T) of Descriptor_Index_T
   with Convention => C;

   type Virtqueue_Available_T is record
      Flags  : Unsigned_16;
      Index  : Unsigned_16;
      Ring   : Virtqueue_Available_Descriptor_Array;
      Unused : Unsigned_16;
   end record;

   --  Section 2.7.8 of VirtIO Spec 1.3.
   type Virtqueue_Used_Element is record
      --  Index of start of used descriptor chain.
      Id     : Unsigned_32;
      Length : Unsigned_32;
   end record;

   type Virtqueue_Used_Descriptor_Array is
     array (VirtIO_Descriptor_Array_Index_T) of Virtqueue_Used_Element
   with Convention => C;

   type Virtqueue_Used_T is record
      Flags : Unsigned_16;
      Index : Unsigned_16;
      Ring  : Virtqueue_Used_Descriptor_Array;
   end record;

   type VirtIO_Device_Status_T is record
      Acknowledge        : Boolean;
      Driver             : Boolean;
      Driver_OK          : Boolean;
      Features_OK        : Boolean;
      Device_Needs_Reset : Boolean;
      Failed             : Boolean;
   end record
   with Size => 32;
   for VirtIO_Device_Status_T use
     record
       Acknowledge at 0 range 0 .. 0;
       Driver at 0 range 1 .. 1;
       Driver_OK at 0 range 2 .. 2;
       Features_OK at 0 range 3 .. 3;
       Device_Needs_Reset at 0 range 6 .. 6;
       Failed at 0 range 7 .. 7;
     end record;

   type VirtIO_Device_Features_Block_T is record
      VIRTIO_BLK_F_SIZE_MAX       : Boolean;
      VIRTIO_BLK_F_SEG_MAX        : Boolean;
      VIRTIO_BLK_F_GEOMETRY       : Boolean;
      VIRTIO_BLK_F_RO             : Boolean;
      VIRTIO_BLK_F_BLK_SIZE       : Boolean;
      VIRTIO_BLK_F_SCSI           : Boolean;
      VIRTIO_BLK_F_FLUSH          : Boolean;
      VIRTIO_BLK_F_TOPOLOGY       : Boolean;
      VIRTIO_BLK_F_CONFIG_WCE     : Boolean;
      VIRTIO_BLK_F_MQ             : Boolean;
      VIRTIO_BLK_F_DISCARD        : Boolean;
      VIRTIO_BLK_F_WRITE_ZEROES   : Boolean;
      UNUSED_1                    : Boolean;
      VIRTIO_F_ANY_LAYOUT         : Boolean;
      VIRTIO_RING_F_INDIRECT_DESC : Boolean;
      VIRTIO_RING_F_EVENT_IDX     : Boolean;
   end record
   with Size => 32;
   for VirtIO_Device_Features_Block_T use
     record
       VIRTIO_BLK_F_SIZE_MAX at 0 range 1 .. 1;
       VIRTIO_BLK_F_SEG_MAX at 0 range 2 .. 2;
       VIRTIO_BLK_F_GEOMETRY at 0 range 4 .. 4;
       VIRTIO_BLK_F_RO at 0 range 5 .. 5;
       VIRTIO_BLK_F_BLK_SIZE at 0 range 6 .. 6;
       VIRTIO_BLK_F_SCSI at 0 range 7 .. 7;
       VIRTIO_BLK_F_FLUSH at 0 range 9 .. 9;
       VIRTIO_BLK_F_TOPOLOGY at 0 range 10 .. 10;
       VIRTIO_BLK_F_CONFIG_WCE at 0 range 11 .. 11;
       VIRTIO_BLK_F_MQ at 0 range 12 .. 12;
       VIRTIO_BLK_F_DISCARD at 0 range 13 .. 13;
       VIRTIO_BLK_F_WRITE_ZEROES at 0 range 14 .. 14;
       UNUSED_1 at 0 range 15 .. 26;
       VIRTIO_F_ANY_LAYOUT at 0 range 27 .. 27;
       VIRTIO_RING_F_INDIRECT_DESC at 0 range 28 .. 28;
       VIRTIO_RING_F_EVENT_IDX at 0 range 29 .. 29;
     end record;

   type VirtIO_MMIO_Device_Registers_T is record
      Magic_Value            : Unsigned_32;
      Version                : Unsigned_32;
      Device_ID              : Unsigned_32;
      Vendor_ID              : Unsigned_32;
      Device_Features        : VirtIO_Device_Features_Block_T;
      Device_Features_Select : Unsigned_32;
      Driver_Features        : VirtIO_Device_Features_Block_T;
      Driver_Features_Select : Unsigned_32;
      Queue_Select           : Unsigned_32;
      Queue_Size_Maximum     : Unsigned_32;
      Queue_Size             : Unsigned_32;
      Queue_Ready            : Unsigned_32;
      Queue_Notify           : Unsigned_32;
      Interrupt_Status       : Unsigned_32;
      Interrupt_Acknowledge  : Unsigned_32;
      Status                 : VirtIO_Device_Status_T;
      Queue_Descriptor_Low   : Unsigned_32;
      Queue_Descriptor_High  : Unsigned_32;
      Queue_Driver_Low       : Unsigned_32;
      Queue_Driver_High      : Unsigned_32;
      Queue_Device_Low       : Unsigned_32;
      Queue_Device_High      : Unsigned_32;
      --  Device-specific configuration space would follow here.
   end record
   with
     Volatile,
     Size                 => (168 * 8),
     Scalar_Storage_Order => System.Low_Order_First;
   for VirtIO_MMIO_Device_Registers_T use
     record
       Magic_Value at 16#000# range 0 .. 31;
       Version at 16#004# range 0 .. 31;
       Device_ID at 16#008# range 0 .. 31;
       Vendor_ID at 16#00c# range 0 .. 31;
       Device_Features at 16#010# range 0 .. 31;
       Device_Features_Select at 16#014# range 0 .. 31;
       Driver_Features at 16#020# range 0 .. 31;
       Driver_Features_Select at 16#024# range 0 .. 31;
       Queue_Select at 16#030# range 0 .. 31;
       Queue_Size_Maximum at 16#034# range 0 .. 31;
       Queue_Size at 16#038# range 0 .. 31;
       Queue_Ready at 16#044# range 0 .. 31;
       Queue_Notify at 16#050# range 0 .. 31;
       Interrupt_Status at 16#060# range 0 .. 31;
       Interrupt_Acknowledge at 16#064# range 0 .. 31;
       Status at 16#070# range 0 .. 31;
       Queue_Descriptor_Low at 16#080# range 0 .. 31;
       Queue_Descriptor_High at 16#084# range 0 .. 31;
       Queue_Driver_Low at 16#090# range 0 .. 31;
       Queue_Driver_High at 16#094# range 0 .. 31;
       Queue_Device_Low at 16#0a0# range 0 .. 31;
       Queue_Device_High at 16#0a4# range 0 .. 31;
     end record;

   function Is_MMIO_Device_Valid
     (Device_Registers : VirtIO_MMIO_Device_Registers_T) return Boolean
   is (Device_Registers.Magic_Value = 16#7472_6976#
       and then Device_Registers.Version = 2
       and then Device_Registers.Device_ID /= 0
       and then Device_Registers.Vendor_ID = 16#554d_4551#);

   procedure Allocate_Descriptor
     (Device : in out Device_T;
      Index  : out Descriptor_Index_T;
      Result : out Function_Result);

   procedure Free_Descriptor
     (Device : in out Device_T;
      Index  : Descriptor_Index_T;
      Result : out Function_Result);

   type Allocated_Descriptor_Array_T is
     array (Descriptor_Index_T range 0 .. 15) of Unsigned_16;

   procedure Allocate_Descriptors
     (Device             : in out Device_T;
      Num_Descriptors    : Positive;
      Descriptor_Indexes : out Allocated_Descriptor_Array_T;
      Result             : out Function_Result);

   procedure Free_Descriptor_Chain
     (Device : in out Device_T;
      Index  : Descriptor_Index_T;
      Result : out Function_Result);

   function Increment_Index (Index : Unsigned_16) return Unsigned_16;

end Devices.VirtIO;
