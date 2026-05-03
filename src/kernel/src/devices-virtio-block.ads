-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

package Devices.Virtio.Block is
   pragma Preelaborate;

   procedure Initialise_Block_Device
     (Device : in out Device_T; Result : out Function_Result);

   procedure Read_Sector
     (Reading_Process       : in out Process_Control_Block_T;
      Device                : in out Device_T;
      Data_Physical_Address : Physical_Address_T;
      Sector                : Unsigned_64;
      Result                : out Function_Result);

   procedure Write_Sector
     (Reading_Process       : in out Process_Control_Block_T;
      Device                : in out Device_T;
      Data_Physical_Address : Physical_Address_T;
      Sector                : Unsigned_64;
      Result                : out Function_Result);

   --  Block device feature bits.
   VIRTIO_BLK_F_SIZE_MAX     : constant := 2 ** 1;
   VIRTIO_BLK_F_SEG_MAX      : constant := 2 ** 2;
   VIRTIO_BLK_F_GEOMETRY     : constant := 2 ** 4;
   VIRTIO_BLK_F_RO           : constant := 2 ** 5;
   VIRTIO_BLK_F_BLK_SIZE     : constant := 2 ** 6;
   VIRTIO_BLK_F_SCSI         : constant := 2 ** 7;
   VIRTIO_BLK_F_FLUSH        : constant := 2 ** 9;
   VIRTIO_BLK_F_TOPOLOGY     : constant := 2 ** 10;
   VIRTIO_BLK_F_CONFIG_WCE   : constant := 2 ** 11;
   VIRTIO_BLK_F_MQ           : constant := 2 ** 12;
   VIRTIO_BLK_F_DISCARD      : constant := 2 ** 13;
   VIRTIO_BLK_F_WRITE_ZEROES : constant := 2 ** 14;
   VIRTIO_BLK_F_LIFETIME     : constant := 2 ** 15;
   VIRTIO_BLK_F_SECURE_ERASE : constant := 2 ** 16;
   VIRTIO_BLK_F_ZONED        : constant := 2 ** 17;

private
   procedure Read_Write
     (Reading_Process       : in out Process_Control_Block_T;
      Device                : in out Device_T;
      Data_Physical_Address : Physical_Address_T;
      Sector                : Unsigned_64;
      Write                 : Boolean;
      Result                : out Function_Result);

   ----------------------------------------------------------------------------
   --  The following methods are the 'unlocked' versions of the above methods
   --  which are called once the spinlock has been acquired.
   --  These functions are only called from the 'locked' versions above.
   --  They are structured this way so that all happy/unhappy paths all lead to
   --  the same exit point, making it easier to ensure the spinlock is always
   --  released.
   ----------------------------------------------------------------------------
   procedure Read_Write_Unlocked
     (Reading_Process       : in out Process_Control_Block_T;
      Device                : in out Device_T;
      Data_Physical_Address : Physical_Address_T;
      Sector                : Unsigned_64;
      Write                 : Boolean;
      Result                : out Function_Result);

   type Block_Request_Type_T is
     (VIRTIO_BLK_T_IN,
      VIRTIO_BLK_T_OUT,
      VIRTIO_BLK_T_FLUSH,
      VIRTIO_BLK_T_DISCARD,
      VIRTIO_BLK_T_WRITE_ZEROES)
   with Size => 32;
   for Block_Request_Type_T use
     (VIRTIO_BLK_T_IN           => 0,
      VIRTIO_BLK_T_OUT          => 1,
      VIRTIO_BLK_T_FLUSH        => 4,
      VIRTIO_BLK_T_DISCARD      => 11,
      VIRTIO_BLK_T_WRITE_ZEROES => 13);

   type Block_Request_T is record
      Request_Type : Block_Request_Type_T;
      Reserved     : Unsigned_32;
      Sector       : Unsigned_64;
   end record;
   for Block_Request_T use
     record
       Request_Type at 0 range 0 .. 31;
       Reserved     at 4 range 0 .. 31;
       Sector       at 8 range 0 .. 63;
     end record;
   type Block_Request_Array_T is
     array (Virtio_Descriptor_Array_Index_T) of Block_Request_T
   with Convention => C;

   --  Virtio block device configuration space layout (Virtio Spec 1.3 §5.2.4).
   --  Fields are only valid when the corresponding feature bit is negotiated.
   type Virtio_Block_Device_Configuration_Space_T is record
      Capacity                      : Unsigned_64;
      Size_Max                      : Unsigned_32;
      Seg_Max                       : Unsigned_32;
      Geometry_Cylinders            : Unsigned_16;
      Geometry_Heads                : Unsigned_8;
      Geometry_Sectors              : Unsigned_8;
      Blk_Size                      : Unsigned_32;
      Topology_Physical_Block_Exp   : Unsigned_8;
      Topology_Alignment_Offset     : Unsigned_8;
      Topology_Min_IO_Size          : Unsigned_16;
      Topology_Opt_IO_Size          : Unsigned_32;
      Writeback                     : Unsigned_8;
      Num_Queues                    : Unsigned_16;
      Max_Discard_Sectors           : Unsigned_32;
      Max_Discard_Seg               : Unsigned_32;
      Discard_Sector_Alignment      : Unsigned_32;
      Max_Write_Zeroes_Sectors      : Unsigned_32;
      Max_Write_Zeroes_Seg          : Unsigned_32;
      Write_Zeroes_May_Unmap        : Unsigned_8;
      Max_Secure_Erase_Sectors      : Unsigned_32;
      Max_Secure_Erase_Seg          : Unsigned_32;
      Secure_Erase_Sector_Alignment : Unsigned_32;
      Zoned_Zone_Sectors            : Unsigned_32;
      Zoned_Max_Open_Zones          : Unsigned_32;
      Zoned_Max_Active_Zones        : Unsigned_32;
      Zoned_Max_Append_Sectors      : Unsigned_32;
      Zoned_Write_Granularity       : Unsigned_32;
      Zoned_Model                   : Unsigned_8;
   end record
   with
     Volatile,
     Size                 => (96 * 8),
     Scalar_Storage_Order => System.Low_Order_First;
   for Virtio_Block_Device_Configuration_Space_T use
     record
       Capacity                      at 0  range 0 .. 63;
       Size_Max                      at 8  range 0 .. 31;
       Seg_Max                       at 12 range 0 .. 31;
       Geometry_Cylinders            at 16 range 0 .. 15;
       Geometry_Heads                at 18 range 0 .. 7;
       Geometry_Sectors              at 19 range 0 .. 7;
       Blk_Size                      at 20 range 0 .. 31;
       Topology_Physical_Block_Exp   at 24 range 0 .. 7;
       Topology_Alignment_Offset     at 25 range 0 .. 7;
       Topology_Min_IO_Size          at 26 range 0 .. 15;
       Topology_Opt_IO_Size          at 28 range 0 .. 31;
       Writeback                     at 32 range 0 .. 7;
       --  1 byte padding at offset 33.
       Num_Queues                    at 34 range 0 .. 15;
       Max_Discard_Sectors           at 36 range 0 .. 31;
       Max_Discard_Seg               at 40 range 0 .. 31;
       Discard_Sector_Alignment      at 44 range 0 .. 31;
       Max_Write_Zeroes_Sectors      at 48 range 0 .. 31;
       Max_Write_Zeroes_Seg          at 52 range 0 .. 31;
       Write_Zeroes_May_Unmap        at 56 range 0 .. 7;
       --  3 bytes padding at offset 57.
       Max_Secure_Erase_Sectors      at 60 range 0 .. 31;
       Max_Secure_Erase_Seg          at 64 range 0 .. 31;
       Secure_Erase_Sector_Alignment at 68 range 0 .. 31;
       Zoned_Zone_Sectors            at 72 range 0 .. 31;
       Zoned_Max_Open_Zones          at 76 range 0 .. 31;
       Zoned_Max_Active_Zones        at 80 range 0 .. 31;
       Zoned_Max_Append_Sectors      at 84 range 0 .. 31;
       Zoned_Write_Granularity       at 88 range 0 .. 31;
       Zoned_Model                   at 92 range 0 .. 7;
     end record;

   function Convert_Request_Block_Number_To_Sector
     (Block_Number : Unsigned_64) return Unsigned_64
   is (Block_Number * (BLOCK_SIZE / 512))
   with Pure_Function, Inline;

   function Get_Block_Request_Physical_Address
     (Device : Device_T; Index : Unsigned_16) return Physical_Address_T;

end Devices.Virtio.Block;
