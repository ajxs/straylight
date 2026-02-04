-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

package Devices.VirtIO.Block is
   pragma Preelaborate;

   procedure Initialise_Block_Device
     (Device : in out Device_T; Result : out Function_Result);

   procedure Read_Sector
     (Reading_Process       : in out Process_Control_Block_T;
      Device                : in out Device_T;
      Data_Physical_Address : Physical_Address_T;
      Sector                : Unsigned_64;
      Result                : out Function_Result);

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
     array (VirtIO_Descriptor_Array_Index_T) of Block_Request_T
   with Convention => C;

   function Convert_Request_Block_Number_To_Sector
     (Block_Number : Unsigned_64) return Unsigned_64
   is (Block_Number * (BLOCK_SIZE / 512))
   with Pure_Function, Inline;

   function Get_Block_Request_Physical_Address
     (Device : Device_T; Index : Unsigned_16) return Physical_Address_T;
end Devices.VirtIO.Block;
