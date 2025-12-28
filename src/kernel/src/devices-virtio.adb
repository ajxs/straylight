-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Devices.VirtIO.Block; use Devices.VirtIO.Block;
with Memory.Allocators;    use Memory.Allocators;
with Memory.Kernel;        use Memory.Kernel;
with RISCV.Atomics;        use RISCV.Atomics;
with Scheduler;            use Scheduler;

package body Devices.VirtIO is
   procedure Acknowledge_Interrupt
     (Device : in out Device_T; Result : out Function_Result)
   is
      Device_Registers : VirtIO_MMIO_Device_Registers_T
      with Import, Alignment => 1, Address => Device.Virtual_Address;

      Interrupt_Status : Unsigned_32 := 0;
   begin
      if Device.Device_Bus /= Device_Bus_VirtIO_MMIO then
         Log_Error ("Invalid device bus for VirtIO device");
         Result := Invalid_Argument;
         return;
      end if;

      Interrupt_Status := Device_Registers.Interrupt_Status;

      --  Acknowledge the interrupt and process any completed requests.
      --  Wakeup any processes waiting on the completed request.
      Acknowledge_Interrupt_Channel :
      declare
         Q_Used : constant Virtqueue_Used_T
         with
           Import,
           Address   => Device.Bus_Info_VirtIO.Q_Used.Virtual_Address,
           Alignment => 1;

         Descriptor_Index : Unsigned_32 := 0;

         Queue_Index : Unsigned_16 := 0;
      begin
         Log_Debug
           ("Q_Used.Index: " & Q_Used.Index'Image, Logging_Tags_VirtIO);

         while Device.Bus_Info_VirtIO.Request_Serviced_Index /= Q_Used.Index
         loop

            Queue_Index :=
              Device.Bus_Info_VirtIO.Request_Serviced_Index
              mod Maximum_VirtIO_Queue_Length;

            --  Verify that the status byte has been cleared by the device,
            --  indicating that the request was successful.
            --  This block has not been moved to its own function because the
            --  import can raise a Constraint_Error.
            Check_Request_Status :
            declare
               Request_Status_Array : VirtIO_Status_Byte_Array_T
               with
                 Import,
                 Address   =>
                   Device.Bus_Info_VirtIO.Request_Status_Array.Virtual_Address,
                 Alignment => 1;
            begin
               Status_Byte : constant Unsigned_8 :=
                 Request_Status_Array (Queue_Index);

               --  The status byte is cleared by the device. If a value other
               --  than zero is encountered here, an error has occurred.
               if Status_Byte /= 0 then
                  Log_Error ("Invalid status byte: " & Status_Byte'Image);
                  Result := Unhandled_Exception;
                  return;
               end if;
            end Check_Request_Status;

            Descriptor_Index := Q_Used.Ring (Queue_Index).Id;

            Log_Debug
              ("Used descriptor index: " & Descriptor_Index'Image,
               Logging_Tags_VirtIO);

            Channel : constant Unsigned_64 :=
              Device.Bus_Info_VirtIO.Request_Info
                (VirtIO_Descriptor_Array_Index_T (Descriptor_Index))
                .Channel;

            Log_Debug
              ("Successfully acknowledged driver Q entry: "
               & Device.Bus_Info_VirtIO.Request_Serviced_Index'Image,
               Logging_Tags_VirtIO);

            Wake_Processes_Waiting_For_Channel (Channel, Result);
            if Is_Error (Result) then
               return;
            end if;

            --  Tell the processor to not move loads or stores
            --  past this point, to ensure that the critical section's memory
            --  references happen strictly after the lock is acquired.
            Fence;

            Device.Bus_Info_VirtIO.Request_Serviced_Index :=
              Device.Bus_Info_VirtIO.Request_Serviced_Index + 1;
         end loop;
      end Acknowledge_Interrupt_Channel;

      Fence;

      Device_Registers.Interrupt_Acknowledge := (Interrupt_Status and 3);

      Log_Debug ("Acknowledged VirtIO Device Interrupt", Logging_Tags_VirtIO);

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Acknowledge_Interrupt");
         Result := Constraint_Exception;
   end Acknowledge_Interrupt;

   procedure Allocate_Descriptor
     (Device : in out Device_T;
      Index  : out Descriptor_Index_T;
      Result : out Function_Result) is
   begin
      if Device.Device_Bus /= Device_Bus_VirtIO_MMIO then
         Log_Error ("Device not VirtIO");
         Index := 0;
         Result := Invalid_Argument;
         return;
      end if;

      for I in VirtIO_Descriptor_Array_Index_T loop
         if Device.Bus_Info_VirtIO.Descriptor_Status (I) then
            Index := I;
            Device.Bus_Info_VirtIO.Descriptor_Status (I) := False;

            Log_Debug
              ("Allocated descriptor: " & Index'Image, Logging_Tags_VirtIO);

            Result := Success;
            return;
         end if;
      end loop;

      Log_Error ("No remaining VirtIO Descriptors");
      Result := No_Remaining_VirtIO_Descriptors;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint Error allocating descriptor");
         Result := Constraint_Exception;
   end Allocate_Descriptor;

   procedure Allocate_Descriptors
     (Device             : in out Device_T;
      Num_Descriptors    : Positive;
      Descriptor_Indexes : out Allocated_Descriptor_Array_T;
      Result             : out Function_Result)
   is
      Free_Descriptor_Result : Function_Result := Unset;
   begin
      if Num_Descriptors > Descriptor_Indexes'Length then
         Log_Error ("Requested number of descriptors exceeds array size");
         Result := Invalid_Argument;
         return;
      end if;

      for Index in
        Descriptor_Index_T range 0 .. Descriptor_Index_T (Num_Descriptors - 1)
      loop
         Allocate_Descriptor (Device, Descriptor_Indexes (Index), Result);
         if Is_Error (Result) then
            Log_Error ("Error allocating graphics request descriptors");

            --  @TODO: More comprehensive error handling strategy here.
            for Allocated_Index in
              Descriptor_Index_T
                range 0 .. Descriptor_Index_T (Num_Descriptors - 1)
            loop
               if Descriptor_Indexes (Allocated_Index) /= 0 then
                  --  Result can be ignored here.
                  Free_Descriptor
                    (Device, Allocated_Index, Free_Descriptor_Result);
               end if;
            end loop;
         end if;
      end loop;

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Allocate_Descriptors");
         Result := Constraint_Exception;
   end Allocate_Descriptors;

   procedure Allocate_VirtIO_Device_Resources
     (Device : in out Device_T; Result : out Function_Result)
   is
      Allocation_Result : Memory_Allocation_Result;
   begin
      --  @TODO: Revisit error handling.
      --  This is a remnant of a very early implementation.
      if Device.Device_Bus /= Device_Bus_VirtIO_MMIO then
         Result := Invalid_Argument;
         return;
      end if;

      Log_Debug
        ("Initialising VirtIO Device Resources...", Logging_Tags_VirtIO);
      Log_Debug ("Allocating Request Status Array...", Logging_Tags_VirtIO);

      Allocate_Kernel_Physical_Memory
        (Maximum_VirtIO_Queue_Length,
         Device.Bus_Info_VirtIO.Request_Status_Array,
         Result);
      if Is_Error (Result) then
         Log_Error ("Error allocating status array memory");
         return;
      end if;

      Log_Debug
        ("Allocated Request Status Array: "
         & Device.Bus_Info_VirtIO.Request_Status_Array.Virtual_Address'Image,
         Logging_Tags_VirtIO);

      Initialise_Request_Status_Array :
      declare
         Request_Status_Array : VirtIO_Status_Byte_Array_T
         with
           Import,
           Address   =>
             Device.Bus_Info_VirtIO.Request_Status_Array.Virtual_Address,
           Alignment => 1;
      begin
         for I in VirtIO_Descriptor_Array_Index_T loop
            Request_Status_Array (I) := 0;
         end loop;
      end Initialise_Request_Status_Array;

      Log_Debug ("Allocated Request Status Array.", Logging_Tags_VirtIO);
      Log_Debug ("Allocating descriptors queues...", Logging_Tags_VirtIO);

      Allocate_Pages (3, Allocation_Result, Result);
      if Is_Error (Result) then
         return;
      end if;

      Device.Bus_Info_VirtIO.Q_Descriptor.Physical_Address :=
        Allocation_Result.Physical_Address;
      Device.Bus_Info_VirtIO.Q_Descriptor.Virtual_Address :=
        Allocation_Result.Virtual_Address;

      Device.Bus_Info_VirtIO.Q_Used.Physical_Address :=
        Allocation_Result.Physical_Address + 16#1000#;
      Device.Bus_Info_VirtIO.Q_Used.Virtual_Address :=
        Allocation_Result.Virtual_Address + 16#1000#;

      Device.Bus_Info_VirtIO.Q_Available.Physical_Address :=
        Allocation_Result.Physical_Address + 16#2000#;
      Device.Bus_Info_VirtIO.Q_Available.Virtual_Address :=
        Allocation_Result.Virtual_Address + 16#2000#;

      Log_Debug
        ("Initialised Device Queues:"
         & ASCII.LF
         & "  Descriptor Q:         "
         & Device.Bus_Info_VirtIO.Q_Descriptor.Physical_Address'Image
         & " / "
         & Device.Bus_Info_VirtIO.Q_Descriptor.Virtual_Address'Image
         & ASCII.LF
         & "  Initialised Device Q: "
         & Device.Bus_Info_VirtIO.Q_Used.Physical_Address'Image
         & " / "
         & Device.Bus_Info_VirtIO.Q_Used.Virtual_Address'Image
         & ASCII.LF
         & "  Initialised Driver Q: "
         & Device.Bus_Info_VirtIO.Q_Available.Physical_Address'Image
         & " / "
         & Device.Bus_Info_VirtIO.Q_Available.Virtual_Address'Image,
         Logging_Tags_VirtIO);

      if Device.Bus_Info_VirtIO.Device_Type = VirtIO_Device_Type_Block then
         Log_Debug
           ("Allocating Block Device Request Array...", Logging_Tags_VirtIO);

         Allocate_Kernel_Physical_Memory
           ((Devices.VirtIO.Block.Block_Request_T'Size / 8)
            * Maximum_VirtIO_Queue_Length,
            Allocation_Result,
            Result);
         if Is_Error (Result) then
            Log_Error
              ("Error allocating block request memory: " & Result'Image);
            return;
         end if;

         Device.Bus_Info_VirtIO.Block_Request_Array_Addresses :=
           Allocation_Result;

         Log_Debug
           ("Allocated Block Device Request Array.", Logging_Tags_VirtIO);
      end if;

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Allocate_VirtIO_Device_Resources");
         Result := Constraint_Exception;
   end Allocate_VirtIO_Device_Resources;

   function Increment_Index (Index : Unsigned_16) return Unsigned_16 is
   begin
      if Index < (Maximum_VirtIO_Queue_Length - 1) then
         return Index + 1;
      end if;

      return 0;
   end Increment_Index;

   procedure Free_Descriptor
     (Device : in out Device_T;
      Index  : Descriptor_Index_T;
      Result : out Function_Result) is
   begin
      if Device.Device_Bus /= Device_Bus_VirtIO_MMIO then
         Log_Error ("Device not VirtIO MMIO");
         Result := Invalid_Argument;
         return;
      end if;

      if Index > Maximum_VirtIO_Queue_Length
        or else Device.Bus_Info_VirtIO.Descriptor_Status (Index)
      then
         Log_Error ("Invalid descriptor index to free: " & Index'Image);
         Result := Invalid_Argument;
         return;
      end if;

      Device.Bus_Info_VirtIO.Descriptor_Status (Index) := True;

      Log_Debug ("Freed descriptor: " & Index'Image, Logging_Tags_VirtIO);

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Exception: Free_Descriptor");
         Result := Constraint_Exception;
   end Free_Descriptor;

   procedure Free_Descriptor_Chain
     (Device : in out Device_T;
      Index  : Descriptor_Index_T;
      Result : out Function_Result)
   is
      Current_Index : Descriptor_Index_T := Index;
   begin
      --  This is declared in an inner-block because the import can raise a
      --  Constraint_Error.
      declare
         Descriptors : Virtqueue_Descriptor_Array
         with
           Import,
           Address   => Device.Bus_Info_VirtIO.Q_Descriptor.Virtual_Address,
           Alignment => 1;
      begin
         loop
            Free_Descriptor (Device, Current_Index, Result);
            if Is_Error (Result) then
               return;
            end if;

            if (Descriptors (Current_Index).Flags and VIRTQ_DESC_F_NEXT) = 0
            then
               exit;
            end if;

            Current_Index := Descriptors (Current_Index).Next;
         end loop;
      end;

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Exception: Free_Descriptor_Chain");
         Result := Constraint_Exception;
   end Free_Descriptor_Chain;

   procedure Initialise_MMIO_Device
     (Device : in out Device_T; Result : out Function_Result)
   is
      Device_Registers : VirtIO_MMIO_Device_Registers_T
      with Import, Alignment => 1, Address => Device.Virtual_Address;

      Queue_Size_Maximum : Unsigned_32 := 0;

      Status : VirtIO_Device_Status_T :=
        (Acknowledge        => False,
         Driver             => False,
         Driver_OK          => False,
         Features_OK        => False,
         Device_Needs_Reset => False,
         Failed             => False);

      Device_Features : VirtIO_Device_Features_Block_T;
   begin
      --  @TODO: Revisit error handling.
      --  This is a remnant of a very early implementation.
      if Device.Device_Bus /= Device_Bus_VirtIO_MMIO then
         Result := Invalid_Argument;
         return;
      end if;

      Log_Debug
        ("Initialising VirtIO MMIO Device:"
         & ASCII.LF
         & "  Virtual Addr:  "
         & Device.Virtual_Address'Image
         & ASCII.LF
         & "  Physical Addr: "
         & Device.Physical_Address'Image,
         Logging_Tags_VirtIO);

      --  4.2.3.1.1 Driver Requirements: Device Initialization states:
      --  The driver MUST start the device initialization by reading and
      --  checking values from MagicValue and Version.
      --  If both values are valid, it MUST read DeviceID and if its value
      --  is zero (0x0) MUST abort initialization and MUST NOT access
      --  any other register.
      if not Is_MMIO_Device_Valid (Device_Registers) then
         Log_Error ("Device Invalid!", Logging_Tags_VirtIO);
         Result := Invalid_Argument;
         return;
      end if;

      Log_Debug
        ("Found valid VirtIO MMIO device: "
         & ASCII.LF
         & "  Vendor ID:      "
         & Device_Registers.Vendor_ID'Image
         & ASCII.LF
         & "  Device Version: "
         & Device_Registers.Version'Image
         & ASCII.LF
         & "  Device ID:      "
         & Device_Registers.Device_ID'Image,
         Logging_Tags_VirtIO);

      --  Initialise the device.
      --  Refer to section 3.1: Device Initialization.
      --  2.1.2 specifies that:
      --   "The device MUST initialize device status to 0 upon reset."
      Device_Registers.Status := Status;

      Status.Acknowledge := True;
      Device_Registers.Status := Status;

      Status.Driver := True;
      Device_Registers.Status := Status;

      --  Negotiate Device Features.
      --  Section 4.2.2.2 Driver Requirements states that:
      --  Before reading from DeviceFeatures, the driver MUST write a
      --  value to DeviceFeaturesSel.
      Device_Registers.Device_Features_Select := 0;
      Device_Features := Device_Registers.Device_Features;

      Device_Features.VIRTIO_BLK_F_RO := False;
      Device_Features.VIRTIO_BLK_F_SCSI := False;
      Device_Features.VIRTIO_BLK_F_CONFIG_WCE := False;
      Device_Features.VIRTIO_BLK_F_MQ := False;
      Device_Features.VIRTIO_F_ANY_LAYOUT := False;
      Device_Features.VIRTIO_RING_F_INDIRECT_DESC := False;
      Device_Features.VIRTIO_RING_F_EVENT_IDX := False;

      --  Section 4.2.2.2 Driver Requirements states that:
      --  Before writing to the DriverFeatures register, the driver MUST
      --  write a value to the DriverFeaturesSel register.
      Device_Registers.Driver_Features_Select := 0;
      Device_Registers.Driver_Features := Device_Features;

      Status.Features_OK := True;
      Device_Registers.Status := Status;

      Status := Device_Registers.Status;
      if not Status.Features_OK then
         Log_Error
           ("Device feature negotiation unsuccessful.", Logging_Tags_VirtIO);
         Result := Unhandled_Exception;
      end if;

      Log_Debug
        ("Device feature negotiation successful.", Logging_Tags_VirtIO);

      Queue_Size_Maximum := Device_Registers.Queue_Size_Maximum;
      if Queue_Size_Maximum = 0 then
         Log_Error ("No queues available.", Logging_Tags_VirtIO);
         Result := Unhandled_Exception;
         return;
      elsif Queue_Size_Maximum < Maximum_VirtIO_Queue_Length then
         Log_Error ("Not enough queues available.", Logging_Tags_VirtIO);
         Result := Unhandled_Exception;
         return;
      end if;

      Device_Registers.Queue_Size := Maximum_VirtIO_Queue_Length;

      Device_Registers.Queue_Select := 0;

      Device_Registers.Queue_Descriptor_Low :=
        Get_Address_Word_Low
          (Address (Device.Bus_Info_VirtIO.Q_Descriptor.Physical_Address));
      Device_Registers.Queue_Descriptor_High :=
        Get_Address_Word_High
          (Address (Device.Bus_Info_VirtIO.Q_Descriptor.Physical_Address));

      Device_Registers.Queue_Device_Low :=
        Get_Address_Word_Low
          (Address (Device.Bus_Info_VirtIO.Q_Used.Physical_Address));
      Device_Registers.Queue_Device_High :=
        Get_Address_Word_High
          (Address (Device.Bus_Info_VirtIO.Q_Used.Physical_Address));

      Device_Registers.Queue_Driver_Low :=
        Get_Address_Word_Low
          (Address (Device.Bus_Info_VirtIO.Q_Available.Physical_Address));
      Device_Registers.Queue_Driver_High :=
        Get_Address_Word_High
          (Address (Device.Bus_Info_VirtIO.Q_Available.Physical_Address));

      Device_Registers.Queue_Ready := 1;

      Status.Driver_OK := True;
      Device_Registers.Status := Status;

      for I in VirtIO_Descriptor_Array_Index_T loop
         Device.Bus_Info_VirtIO.Descriptor_Status (I) := True;
      end loop;

      Log_Debug ("Initialised VirtIO MMIO Device.", Logging_Tags_VirtIO);

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error
           ("Constraint_Error: Initialise_MMIO_Device", Logging_Tags_VirtIO);
         Result := Constraint_Exception;
   end Initialise_MMIO_Device;

end Devices.VirtIO;
