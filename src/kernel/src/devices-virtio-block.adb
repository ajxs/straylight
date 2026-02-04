-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with RISCV.Atomics;       use RISCV.Atomics;
with Processes.Scheduler; use Processes.Scheduler;

package body Devices.VirtIO.Block is
   function Get_Block_Request_Physical_Address
     (Device : Device_T; Index : Unsigned_16) return Physical_Address_T is
   begin
      Block_Request_Size : constant Unsigned_64 := Block_Request_T'Size / 8;

      return
        Device.Bus_Info_VirtIO.Block_Request_Array_Addresses.Physical_Address
        + Physical_Address_T
            (Unsigned_64_To_Address
               (Unsigned_64 (Index) * Block_Request_Size));
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Get_Block_Request_Physical_Address");
         return Null_Physical_Address;
   end Get_Block_Request_Physical_Address;

   procedure Read_Sector
     (Reading_Process       : in out Process_Control_Block_T;
      Device                : in out Device_T;
      Data_Physical_Address : Physical_Address_T;
      Sector                : Unsigned_64;
      Result                : out Function_Result) is
   begin
      Read_Write
        (Reading_Process,
         Device,
         Data_Physical_Address,
         Sector,
         False,
         Result);
   end Read_Sector;

   procedure Read_Write
     (Reading_Process       : in out Process_Control_Block_T;
      Device                : in out Device_T;
      Data_Physical_Address : Physical_Address_T;
      Sector                : Unsigned_64;
      Write                 : Boolean;
      Result                : out Function_Result) is
   begin
      Acquire_Spinlock (Device.Spinlock);

      Read_Write_Unlocked
        (Reading_Process,
         Device,
         Data_Physical_Address,
         Sector,
         Write,
         Result);

      Release_Spinlock (Device.Spinlock);
   end Read_Write;

   procedure Read_Write_Unlocked
     (Reading_Process       : in out Process_Control_Block_T;
      Device                : in out Device_T;
      Data_Physical_Address : Physical_Address_T;
      Sector                : Unsigned_64;
      Write                 : Boolean;
      Result                : out Function_Result)
   is
      Descriptor_Indexes : Allocated_Descriptor_Array_T;

      Device_Registers : VirtIO_MMIO_Device_Registers_T
      with Import, Alignment => 1, Address => Device.Virtual_Address;
   begin
      --  If this is a write operation, ensure the device is not read-only.
      if Write and then Device_Registers.Device_Features.VIRTIO_BLK_F_RO then
         Log_Error ("Attempt to write to read-only VirtIO Block Device");
         Result := Operation_Unsupported;
         return;
      end if;

      Allocate_Descriptors (Device, 3, Descriptor_Indexes, Result);
      if Is_Error (Result) then
         return;
      end if;

      Blocking_Channel : constant Blocking_Channel_T :=
        Address_To_Unsigned_64 (Address (Data_Physical_Address));

      Device.Bus_Info_VirtIO.Request_Info (Descriptor_Indexes (0)).Channel :=
        Blocking_Channel;

      Initialise_Status : declare
         Request_Status_Array : VirtIO_Status_Byte_Array_T
         with
           Import,
           Address   =>
             Device.Bus_Info_VirtIO.Request_Status_Array.Virtual_Address,
           Alignment => 1;
      begin
         Request_Status_Array (Descriptor_Indexes (0)) := 255;
      end Initialise_Status;

      Setup_Queues : declare
         Descriptors : Virtqueue_Descriptor_Array
         with
           Import,
           Address   => Device.Bus_Info_VirtIO.Q_Descriptor.Virtual_Address,
           Alignment => 1;

         Q_Available : Virtqueue_Available_T
         with
           Import,
           Address   => Device.Bus_Info_VirtIO.Q_Available.Virtual_Address,
           Alignment => 1;

         Block_Device_Request_Array : Block_Request_Array_T
         with
           Import,
           Address   =>
             Device
               .Bus_Info_VirtIO
               .Block_Request_Array_Addresses
               .Virtual_Address,
           Alignment => 1;

      begin
         --  Set up the block device request structure.
         Block_Device_Request_Array (Descriptor_Indexes (0)).Sector := Sector;
         Block_Device_Request_Array (Descriptor_Indexes (0)).Reserved := 0;

         if Write then
            Block_Device_Request_Array (Descriptor_Indexes (0)).Request_Type :=
              VIRTIO_BLK_T_OUT;
         else
            Block_Device_Request_Array (Descriptor_Indexes (0)).Request_Type :=
              VIRTIO_BLK_T_IN;
         end if;

         Block_Request_Array_Addresses : constant Physical_Address_T :=
           Get_Block_Request_Physical_Address (Device, Descriptor_Indexes (0));
         --  Any exception will return a null address.
         if Block_Request_Array_Addresses = Null_Physical_Address then
            Free_Descriptor_Chain (Device, Descriptor_Indexes (0), Result);
            Result := Unhandled_Exception;
            return;
         end if;

         Descriptors (Descriptor_Indexes (0)) :=
           (Address => Block_Request_Array_Addresses,
            Length  => Block_Request_T'Size / 8,
            Flags   => VIRTQ_DESC_F_NEXT,
            Next    => Descriptor_Indexes (1));

         Descriptors (Descriptor_Indexes (1)) :=
           (Address => Data_Physical_Address,
            Length  => BLOCK_SIZE,
            Flags   => VIRTQ_DESC_F_NEXT,
            Next    => Descriptor_Indexes (2));

         if not Write then
            Descriptors (Descriptor_Indexes (1)).Flags :=
              (VIRTQ_DESC_F_WRITE or VIRTQ_DESC_F_NEXT);
         end if;

         Descriptors (Descriptor_Indexes (2)) :=
           (Address =>
              --  Get the address of the status byte for this request.
              Device.Bus_Info_VirtIO.Request_Status_Array.Physical_Address
              + Physical_Address_T
                  (Unsigned_64_To_Address
                     (Unsigned_64 (Descriptor_Indexes (0)))),
            Length  => 1,
            Flags   => VIRTQ_DESC_F_WRITE,
            Next    => 0);

         Q_Available.Ring
           (Q_Available.Index mod Maximum_VirtIO_Queue_Length) :=
           Descriptor_Indexes (0);

         --  Tell the processor to not move loads or stores past this point.
         Fence;

         Q_Available.Index := Q_Available.Index + 1;
      end Setup_Queues;

      Fence;

      --  This value is the queue index, which is always 0 for single queue.
      Device_Registers.Queue_Notify := 0;

      Lock_Process_Waiting_For_Channel
        (Blocking_Channel, Device.Spinlock, Reading_Process);

      --  Reached once the request is complete and control is returned to
      --  the calling process.
      Free_Descriptor_Chain (Device, Descriptor_Indexes (0), Result);
      if Is_Error (Result) then
         return;
      end if;

      Read_Request_Status : declare
         Request_Status_Array : VirtIO_Status_Byte_Array_T
         with
           Import,
           Address   =>
             Device.Bus_Info_VirtIO.Request_Status_Array.Virtual_Address,
           Alignment => 1;

         VIRTIO_BLK_S_OK     : constant := 0;
         VIRTIO_BLK_S_IOERR  : constant := 1;
         VIRTIO_BLK_S_UNSUPP : constant := 2;
      begin
         case Request_Status_Array (Descriptor_Indexes (0)) is
            when VIRTIO_BLK_S_OK     =>
               Result := Success;

            when VIRTIO_BLK_S_IOERR  =>
               Log_Error ("I/O Error on VirtIO Block Device");
               Result := Device_IO_Error;

            when VIRTIO_BLK_S_UNSUPP =>
               Log_Error ("Unsupported Operation on VirtIO Block Device");
               Result := Operation_Unsupported;

            when others              =>
               Log_Error ("Unknown Status on VirtIO Block Device");
               Result := Unhandled_Exception;
         end case;
      end Read_Request_Status;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Read_Write_Unlocked");
         Result := Constraint_Exception;
   end Read_Write_Unlocked;

end Devices.VirtIO.Block;
