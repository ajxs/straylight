with Memory.Allocators; use Memory.Allocators;
with Memory.Kernel;     use Memory.Kernel;
with RISCV.Atomics;     use RISCV.Atomics;
with Scheduler;         use Scheduler;

package body Devices.VirtIO.Graphics is
   procedure Attach_Framebuffer_To_Resource
     (Reading_Process              : in out Process_Control_Block_T;
      Device                       : in out Device_T;
      Resource_Id                  : Unsigned_32;
      Framebuffer_Physical_Address : Physical_Address_T;
      Result                       : out Function_Result) is
   begin
      Acquire_Spinlock (Device.Spinlock);

      Attach_Framebuffer_To_Resource_Unlocked
        (Reading_Process,
         Device,
         Resource_Id,
         Framebuffer_Physical_Address,
         Result);

      Release_Spinlock (Device.Spinlock);
   end Attach_Framebuffer_To_Resource;

   procedure Attach_Framebuffer_To_Resource_Unlocked
     (Reading_Process              : in out Process_Control_Block_T;
      Device                       : in out Device_T;
      Resource_Id                  : Unsigned_32;
      Framebuffer_Physical_Address : Physical_Address_T;
      Result                       : out Function_Result)
   is
      Device_Registers : VirtIO_MMIO_Device_Registers_T
      with Import, Alignment => 1, Address => Device.Virtual_Address;

      Descriptor_Indexes : Allocated_Descriptor_Array_T;

      Resource_Allocation : Memory_Allocation_Result;
   begin
      Request_Size_Attach_Backing : constant Natural :=
        Request_Resource_Attach_Backing'Size / 8;
      Request_Size_Mem_Entry : constant Natural :=
        Virtio_Gpu_Mem_Entry'Size / 8;
      Response_Size : constant Natural := Virtio_Gpu_Ctrl_Hdr'Size / 8;

      Log_Debug
        ("Attaching framebuffer to VirtIO Graphics Resource",
         Logging_Tags_VirtIO_Graphics);

      Allocate_Descriptors (Device, 3, Descriptor_Indexes, Result);
      if Is_Error (Result) then
         return;
      end if;

      Allocate_Kernel_Physical_Memory
        (Request_Size_Attach_Backing + Request_Size_Mem_Entry + Response_Size,
         Resource_Allocation,
         Result);
      if Is_Error (Result) then
         return;
      end if;

      Setup_Request :
      declare
         Request_Data : Request_Resource_Attach_Backing
         with
           Import,
           Address   => Resource_Allocation.Virtual_Address,
           Alignment => 1;

         Request_Mem_Entry : Virtio_Gpu_Mem_Entry
         with
           Import,
           Address   =>
             Resource_Allocation.Virtual_Address
             + Storage_Offset (Request_Size_Attach_Backing),
           Alignment => 1;
      begin
         Request_Data :=
           (Hdr         =>
              (Ctrl_Type => Cmd_Resource_Attach_Backing,
               Flags     => 0,
               Fence_Id  => 0,
               Ctx_Id    => 0,
               Padding   => 0),
            Resource_Id => Resource_Id,
            Num_Entries => 1);

         Request_Mem_Entry :=
           (Addr    =>
              Address_To_Unsigned_64 (Address (Framebuffer_Physical_Address)),
            Length  =>
              Device.Bus_Info_VirtIO.Framebuffer_Width
              * Device.Bus_Info_VirtIO.Framebuffer_Height
              * 4,
            Padding => 0);
      end Setup_Request;

      Setup_Queues :
      declare
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

      begin
         Descriptors (Descriptor_Indexes (0)) :=
           (Address => Resource_Allocation.Physical_Address,
            Length  => Unsigned_32 (Request_Size_Attach_Backing),
            Flags   => VIRTQ_DESC_F_NEXT,
            Next    => Descriptor_Indexes (1));

         Descriptors (Descriptor_Indexes (1)) :=
           (Address =>
              Resource_Allocation.Physical_Address
              + Storage_Offset (Request_Size_Attach_Backing),
            Length  => Unsigned_32 (Request_Size_Mem_Entry),
            Flags   => VIRTQ_DESC_F_NEXT,
            Next    => Descriptor_Indexes (2));

         Descriptors (Descriptor_Indexes (2)) :=
           (Address =>
              Resource_Allocation.Physical_Address
              + Storage_Offset (Request_Size_Attach_Backing)
              + Storage_Offset (Request_Size_Mem_Entry),
            Length  => Unsigned_32 (Response_Size),
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

      Device_Registers.Queue_Notify := Device_Control_Queue_Index;

      Blocking_Channel : constant Blocking_Channel_T :=
        Address_To_Unsigned_64 (Address (Resource_Allocation.Virtual_Address));

      Device.Bus_Info_VirtIO.Request_Info (Descriptor_Indexes (0)).Channel :=
        Blocking_Channel;

      Lock_Process_Waiting_For_Channel
        (Blocking_Channel, Device.Spinlock, Reading_Process);

      Test_Result :
      declare
         Response_Data : Virtio_Gpu_Ctrl_Hdr
         with
           Import,
           Address   =>
             Resource_Allocation.Virtual_Address
             + Storage_Offset (Request_Size_Attach_Backing)
             + Storage_Offset (Request_Size_Mem_Entry),
           Alignment => 1;
      begin
         if Response_Data.Ctrl_Type /= Resp_Ok_NoData then
            Log_Error
              ("Failed to attach framebuffer to resource. Response Code:"
               & Response_Data.Ctrl_Type'Image);
            Result := Unhandled_Exception;
            return;
         end if;
      end Test_Result;

      --  Reached once the request is complete and control is returned to
      --  the calling process.
      Free_Descriptor_Chain (Device, Descriptor_Indexes (0), Result);
      if Is_Error (Result) then
         return;
      end if;

      Free_Kernel_Memory (Resource_Allocation.Virtual_Address, Result);
      if Is_Error (Result) then
         return;
      end if;
   exception
      when others =>
         Log_Error
           ("Constraint_Error: Attach_Framebuffer_To_Resource_Unlocked");
         Result := Constraint_Exception;
   end Attach_Framebuffer_To_Resource_Unlocked;

   procedure Get_Display_Info
     (Reading_Process : in out Process_Control_Block_T;
      Device          : in out Device_T;
      Result          : out Function_Result) is
   begin
      Acquire_Spinlock (Device.Spinlock);

      Get_Display_Info_Unlocked (Reading_Process, Device, Result);

      Release_Spinlock (Device.Spinlock);
   end Get_Display_Info;

   procedure Get_Display_Info_Unlocked
     (Reading_Process : in out Process_Control_Block_T;
      Device          : in out Device_T;
      Result          : out Function_Result)
   is
      Device_Registers : VirtIO_MMIO_Device_Registers_T
      with Import, Alignment => 1, Address => Device.Virtual_Address;

      Descriptor_Indexes : Allocated_Descriptor_Array_T;

      Resource_Allocation : Memory_Allocation_Result;
   begin
      Request_Size : constant Natural := Virtio_Gpu_Ctrl_Hdr'Size / 8;
      Response_Size : constant Natural :=
        Virtio_Gpu_Resp_Display_Info'Size / 8;

      Log_Debug
        ("Getting display info from VirtIO Graphics Device",
         Logging_Tags_VirtIO_Graphics);

      Allocate_Descriptors (Device, 2, Descriptor_Indexes, Result);
      if Is_Error (Result) then
         return;
      end if;

      Allocate_Kernel_Physical_Memory
        (Request_Size + Response_Size, Resource_Allocation, Result);
      if Is_Error (Result) then
         return;
      end if;

      Setup_Request :
      declare
         Request_Data : Virtio_Gpu_Ctrl_Hdr
         with
           Import,
           Address   => Resource_Allocation.Virtual_Address,
           Alignment => 1;
      begin
         Request_Data :=
           (Ctrl_Type => Cmd_Get_Display_Info,
            Flags     => 0,
            Fence_Id  => 0,
            Ctx_Id    => 0,
            Padding   => 0);
      end Setup_Request;

      Setup_Queues :
      declare
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

      begin
         Descriptors (Descriptor_Indexes (0)) :=
           (Address => Resource_Allocation.Physical_Address,
            Length  => Unsigned_32 (Request_Size),
            Flags   => VIRTQ_DESC_F_NEXT,
            Next    => Descriptor_Indexes (1));

         Descriptors (Descriptor_Indexes (1)) :=
           (Address =>
              Resource_Allocation.Physical_Address
              + Storage_Offset (Request_Size),
            Length  => Unsigned_32 (Response_Size),
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

      Device_Registers.Queue_Notify := Device_Control_Queue_Index;

      Blocking_Channel : constant Blocking_Channel_T :=
        Address_To_Unsigned_64 (Address (Resource_Allocation.Virtual_Address));

      Device.Bus_Info_VirtIO.Request_Info (Descriptor_Indexes (0)).Channel :=
        Blocking_Channel;

      Lock_Process_Waiting_For_Channel
        (Blocking_Channel, Device.Spinlock, Reading_Process);

      Read_Response :
      declare
         Response_Data : Virtio_Gpu_Resp_Display_Info
         with
           Import,
           Address   =>
             Resource_Allocation.Virtual_Address
             + Storage_Offset (Request_Size),
           Alignment => 1;
      begin
         for I in Response_Data.Pmodes'Range loop
            if Response_Data.Pmodes (I).Enabled /= 0 then
               Log_Debug
                 ("Found enabled display mode: "
                  & "X="
                  & Response_Data.Pmodes (I).R.X'Image
                  & ", Y="
                  & Response_Data.Pmodes (I).R.Y'Image
                  & ", Width="
                  & Response_Data.Pmodes (I).R.Width'Image
                  & ", Height="
                  & Response_Data.Pmodes (I).R.Height'Image,
                  Logging_Tags_VirtIO_Graphics);
            end if;
         end loop;
      end Read_Response;

      --  Reached once the request is complete and control is returned to
      --  the calling process.
      Free_Descriptor_Chain (Device, Descriptor_Indexes (0), Result);
      if Is_Error (Result) then
         return;
      end if;

      Free_Kernel_Memory (Resource_Allocation.Virtual_Address, Result);
      if Is_Error (Result) then
         return;
      end if;
   exception
      when others =>
         Log_Error ("Constraint_Error: Get_Display_Info_Unlocked");
         Result := Constraint_Exception;
   end Get_Display_Info_Unlocked;

   procedure Created_2d_Resource
     (Reading_Process : in out Process_Control_Block_T;
      Device          : in out Device_T;
      Resource_Id     : Unsigned_32;
      Result          : out Function_Result) is
   begin
      Acquire_Spinlock (Device.Spinlock);

      Created_2d_Resource_Unlocked
        (Reading_Process, Device, Resource_Id, Result);

      Release_Spinlock (Device.Spinlock);
   end Created_2d_Resource;

   procedure Created_2d_Resource_Unlocked
     (Reading_Process : in out Process_Control_Block_T;
      Device          : in out Device_T;
      Resource_Id     : Unsigned_32;
      Result          : out Function_Result)
   is
      Device_Registers : VirtIO_MMIO_Device_Registers_T
      with Import, Alignment => 1, Address => Device.Virtual_Address;

      Descriptor_Indexes : Allocated_Descriptor_Array_T;

      Resource_Allocation : Memory_Allocation_Result;
   begin
      Request_Size : constant Natural := Request_Resource_Create_2D'Size / 8;
      Response_Size : constant Natural := Virtio_Gpu_Ctrl_Hdr'Size / 8;

      Allocate_Descriptors (Device, 2, Descriptor_Indexes, Result);
      if Is_Error (Result) then
         return;
      end if;

      Allocate_Kernel_Physical_Memory
        (Request_Size + Response_Size, Resource_Allocation, Result);
      if Is_Error (Result) then
         return;
      end if;

      Setup_Request :
      declare
         Request_Data : Request_Resource_Create_2D
         with
           Import,
           Address   => Resource_Allocation.Virtual_Address,
           Alignment => 1;
      begin
         Request_Data :=
           (Hdr         =>
              (Ctrl_Type => Cmd_Resource_Create_2D,
               Flags     => 0,
               Fence_Id  => 0,
               Ctx_Id    => 0,
               Padding   => 0),
            Resource_Id => Resource_Id,
            Format      => VIRTIO_GPU_FORMAT_R8G8B8A8_UNORM,
            Width       => Device.Bus_Info_VirtIO.Framebuffer_Width,
            Height      => Device.Bus_Info_VirtIO.Framebuffer_Height);
      end Setup_Request;

      Setup_Queues :
      declare
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

      begin
         Descriptors (Descriptor_Indexes (0)) :=
           (Address => Resource_Allocation.Physical_Address,
            Length  => Unsigned_32 (Request_Size),
            Flags   => VIRTQ_DESC_F_NEXT,
            Next    => Descriptor_Indexes (1));

         Descriptors (Descriptor_Indexes (1)) :=
           (Address =>
              Resource_Allocation.Physical_Address
              + Storage_Offset (Request_Size),
            Length  => Unsigned_32 (Response_Size),
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

      Device_Registers.Queue_Notify := Device_Control_Queue_Index;

      Blocking_Channel : constant Blocking_Channel_T :=
        Address_To_Unsigned_64 (Address (Resource_Allocation.Virtual_Address));

      Device.Bus_Info_VirtIO.Request_Info (Descriptor_Indexes (0)).Channel :=
        Blocking_Channel;

      Lock_Process_Waiting_For_Channel
        (Blocking_Channel, Device.Spinlock, Reading_Process);

      Test_Result :
      declare
         Response_Data : Virtio_Gpu_Ctrl_Hdr
         with
           Import,
           Address   =>
             Resource_Allocation.Virtual_Address
             + Storage_Offset (Request_Size),
           Alignment => 1;
      begin
         if Response_Data.Ctrl_Type /= Resp_Ok_NoData then
            Log_Error
              ("Failed to create 2d resource. Response Code: "
               & Response_Data.Ctrl_Type'Image);
            Result := Unhandled_Exception;
            return;
         end if;
      end Test_Result;

      --  Reached once the request is complete and control is returned to
      --  the calling process.
      Free_Descriptor_Chain (Device, Descriptor_Indexes (0), Result);
      if Is_Error (Result) then
         return;
      end if;

      Free_Kernel_Memory (Resource_Allocation.Virtual_Address, Result);
      if Is_Error (Result) then
         return;
      end if;
   exception
      when others =>
         Log_Error ("Constraint_Error: Created_2d_Resource_Unlocked");
         Result := Constraint_Exception;
   end Created_2d_Resource_Unlocked;

   procedure Set_Scanout
     (Reading_Process : in out Process_Control_Block_T;
      Device          : in out Device_T;
      Resource_Id     : Unsigned_32;
      Scanout_Id      : Unsigned_32;
      Width           : Unsigned_32;
      Height          : Unsigned_32;
      Result          : out Function_Result) is
   begin
      Acquire_Spinlock (Device.Spinlock);

      Set_Scanout_Unlocked
        (Reading_Process,
         Device,
         Resource_Id,
         Scanout_Id,
         Width,
         Height,
         Result);

      Release_Spinlock (Device.Spinlock);
   end Set_Scanout;

   procedure Set_Scanout_Unlocked
     (Reading_Process : in out Process_Control_Block_T;
      Device          : in out Device_T;
      Resource_Id     : Unsigned_32;
      Scanout_Id      : Unsigned_32;
      Width           : Unsigned_32;
      Height          : Unsigned_32;
      Result          : out Function_Result)
   is
      Device_Registers : VirtIO_MMIO_Device_Registers_T
      with Import, Alignment => 1, Address => Device.Virtual_Address;

      Descriptor_Indexes : Allocated_Descriptor_Array_T;

      Resource_Allocation : Memory_Allocation_Result;
   begin
      Request_Size : constant Natural := Request_Set_Scanout'Size / 8;
      Response_Size : constant Natural := Virtio_Gpu_Ctrl_Hdr'Size / 8;

      Allocate_Descriptors (Device, 2, Descriptor_Indexes, Result);
      if Is_Error (Result) then
         return;
      end if;

      Allocate_Kernel_Physical_Memory
        (Request_Size + Response_Size, Resource_Allocation, Result);
      if Is_Error (Result) then
         return;
      end if;

      Setup_Request :
      declare
         Request_Data : Request_Set_Scanout
         with
           Import,
           Address   => Resource_Allocation.Virtual_Address,
           Alignment => 1;
      begin
         Request_Data :=
           (Hdr         =>
              (Ctrl_Type => Cmd_Set_Scanout,
               Flags     => 0,
               Fence_Id  => 0,
               Ctx_Id    => 0,
               Padding   => 0),
            Scanout_Id  => Scanout_Id,
            Resource_Id => Resource_Id,
            R           => (X => 0, Y => 0, Width => Width, Height => Height));
      end Setup_Request;

      Setup_Queues :
      declare
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

      begin
         Descriptors (Descriptor_Indexes (0)) :=
           (Address => Resource_Allocation.Physical_Address,
            Length  => Unsigned_32 (Request_Size),
            Flags   => VIRTQ_DESC_F_NEXT,
            Next    => Descriptor_Indexes (1));

         Descriptors (Descriptor_Indexes (1)) :=
           (Address =>
              Resource_Allocation.Physical_Address
              + Storage_Offset (Request_Size),
            Length  => Unsigned_32 (Response_Size),
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

      Device_Registers.Queue_Notify := Device_Control_Queue_Index;

      Blocking_Channel : constant Blocking_Channel_T :=
        Address_To_Unsigned_64 (Address (Resource_Allocation.Virtual_Address));

      Device.Bus_Info_VirtIO.Request_Info (Descriptor_Indexes (0)).Channel :=
        Blocking_Channel;

      Lock_Process_Waiting_For_Channel
        (Blocking_Channel, Device.Spinlock, Reading_Process);

      Test_Result :
      declare
         Response_Data : Virtio_Gpu_Ctrl_Hdr
         with
           Import,
           Address   =>
             Resource_Allocation.Virtual_Address
             + Storage_Offset (Request_Size),
           Alignment => 1;
      begin
         if Response_Data.Ctrl_Type /= Resp_Ok_NoData then
            Log_Error
              ("Failed to set scanout. Response Code: "
               & Response_Data.Ctrl_Type'Image);
            Result := Unhandled_Exception;
            return;
         end if;
      end Test_Result;

      --  Reached once the request is complete and control is returned to
      --  the calling process.
      Free_Descriptor_Chain (Device, Descriptor_Indexes (0), Result);
      if Is_Error (Result) then
         return;
      end if;

      Free_Kernel_Memory (Resource_Allocation.Virtual_Address, Result);
      if Is_Error (Result) then
         return;
      end if;
   exception
      when others =>
         Log_Error ("Constraint_Error: Set_Scanout_Unlocked");
         Result := Constraint_Exception;
   end Set_Scanout_Unlocked;

   procedure Transfer_To_Host_2d
     (Reading_Process : in out Process_Control_Block_T;
      Device          : in out Device_T;
      Resource_Id     : Unsigned_32;
      X               : Unsigned_32;
      Y               : Unsigned_32;
      Width           : Unsigned_32;
      Height          : Unsigned_32;
      Result          : out Function_Result) is
   begin
      Acquire_Spinlock (Device.Spinlock);

      Transfer_To_Host_2d_Unlocked
        (Reading_Process, Device, Resource_Id, X, Y, Width, Height, Result);

      Release_Spinlock (Device.Spinlock);
   end Transfer_To_Host_2d;

   procedure Transfer_To_Host_2d_Unlocked
     (Reading_Process : in out Process_Control_Block_T;
      Device          : in out Device_T;
      Resource_Id     : Unsigned_32;
      X               : Unsigned_32;
      Y               : Unsigned_32;
      Width           : Unsigned_32;
      Height          : Unsigned_32;
      Result          : out Function_Result)
   is
      Device_Registers : VirtIO_MMIO_Device_Registers_T
      with Import, Alignment => 1, Address => Device.Virtual_Address;

      Descriptor_Indexes : Allocated_Descriptor_Array_T;

      Resource_Allocation : Memory_Allocation_Result;
   begin
      Request_Size : constant Natural := Request_Transfer_To_Host_2D'Size / 8;
      Response_Size : constant Natural := Virtio_Gpu_Ctrl_Hdr'Size / 8;

      Allocate_Descriptors (Device, 2, Descriptor_Indexes, Result);
      if Is_Error (Result) then
         return;
      end if;

      Allocate_Kernel_Physical_Memory
        (Request_Size + Response_Size, Resource_Allocation, Result);
      if Is_Error (Result) then
         return;
      end if;

      Setup_Request :
      declare
         Request_Data : Request_Transfer_To_Host_2D
         with
           Import,
           Address   => Resource_Allocation.Virtual_Address,
           Alignment => 1;
      begin
         Request_Data :=
           (Hdr         =>
              (Ctrl_Type => Cmd_Transfer_To_Host_2D,
               Flags     => 0,
               Fence_Id  => 0,
               Ctx_Id    => 0,
               Padding   => 0),
            R           => (X => X, Y => Y, Width => Width, Height => Height),
            Offset      => 0,
            Resource_Id => Resource_Id,
            Padding     => 0);
      end Setup_Request;

      Setup_Queues :
      declare
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

      begin
         Descriptors (Descriptor_Indexes (0)) :=
           (Address => Resource_Allocation.Physical_Address,
            Length  => Unsigned_32 (Request_Size),
            Flags   => VIRTQ_DESC_F_NEXT,
            Next    => Descriptor_Indexes (1));

         Descriptors (Descriptor_Indexes (1)) :=
           (Address =>
              Resource_Allocation.Physical_Address
              + Storage_Offset (Request_Size),
            Length  => Unsigned_32 (Response_Size),
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

      Device_Registers.Queue_Notify := Device_Control_Queue_Index;

      Blocking_Channel : constant Blocking_Channel_T :=
        Address_To_Unsigned_64 (Address (Resource_Allocation.Virtual_Address));

      Device.Bus_Info_VirtIO.Request_Info (Descriptor_Indexes (0)).Channel :=
        Blocking_Channel;

      Lock_Process_Waiting_For_Channel
        (Blocking_Channel, Device.Spinlock, Reading_Process);

      Test_Result :
      declare
         Response_Data : Virtio_Gpu_Ctrl_Hdr
         with
           Import,
           Address   =>
             Resource_Allocation.Virtual_Address
             + Storage_Offset (Request_Size),
           Alignment => 1;
      begin
         if Response_Data.Ctrl_Type /= Resp_Ok_NoData then
            Log_Error
              ("Failed to transfer to host. Response Code: "
               & Response_Data.Ctrl_Type'Image);
            Result := Unhandled_Exception;
            return;
         end if;
      end Test_Result;

      --  Reached once the request is complete and control is returned to
      --  the calling process.
      Free_Descriptor_Chain (Device, Descriptor_Indexes (0), Result);
      if Is_Error (Result) then
         return;
      end if;

      Free_Kernel_Memory (Resource_Allocation.Virtual_Address, Result);
      if Is_Error (Result) then
         return;
      end if;
   exception
      when others =>
         Log_Error ("Constraint_Error: Transfer_To_Host_2d_Unlocked");
         Result := Constraint_Exception;
   end Transfer_To_Host_2d_Unlocked;

   procedure Resource_Flush
     (Reading_Process : in out Process_Control_Block_T;
      Device          : in out Device_T;
      Resource_Id     : Unsigned_32;
      X               : Unsigned_32;
      Y               : Unsigned_32;
      Width           : Unsigned_32;
      Height          : Unsigned_32;
      Result          : out Function_Result) is
   begin
      Acquire_Spinlock (Device.Spinlock);

      Resource_Flush_Unlocked
        (Reading_Process, Device, Resource_Id, X, Y, Width, Height, Result);

      Release_Spinlock (Device.Spinlock);
   end Resource_Flush;

   procedure Resource_Flush_Unlocked
     (Reading_Process : in out Process_Control_Block_T;
      Device          : in out Device_T;
      Resource_Id     : Unsigned_32;
      X               : Unsigned_32;
      Y               : Unsigned_32;
      Width           : Unsigned_32;
      Height          : Unsigned_32;
      Result          : out Function_Result)
   is
      Device_Registers : VirtIO_MMIO_Device_Registers_T
      with Import, Alignment => 1, Address => Device.Virtual_Address;

      Descriptor_Indexes : Allocated_Descriptor_Array_T;

      Resource_Allocation : Memory_Allocation_Result;
   begin
      Request_Size : constant Natural := Request_Resource_Flush'Size / 8;
      Response_Size : constant Natural := Virtio_Gpu_Ctrl_Hdr'Size / 8;

      Allocate_Descriptors (Device, 2, Descriptor_Indexes, Result);
      if Is_Error (Result) then
         return;
      end if;

      Allocate_Kernel_Physical_Memory
        (Request_Size + Response_Size, Resource_Allocation, Result);
      if Is_Error (Result) then
         return;
      end if;

      Setup_Request :
      declare
         Request_Data : Request_Resource_Flush
         with
           Import,
           Address   => Resource_Allocation.Virtual_Address,
           Alignment => 1;
      begin
         Request_Data :=
           (Hdr         =>
              (Ctrl_Type => Cmd_Resource_Flush,
               Flags     => 0,
               Fence_Id  => 0,
               Ctx_Id    => 0,
               Padding   => 0),
            R           => (X => X, Y => Y, Width => Width, Height => Height),
            Resource_Id => Resource_Id,
            Padding     => 0);
      end Setup_Request;

      Setup_Queues :
      declare
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

      begin
         Descriptors (Descriptor_Indexes (0)) :=
           (Address => Resource_Allocation.Physical_Address,
            Length  => Unsigned_32 (Request_Size),
            Flags   => VIRTQ_DESC_F_NEXT,
            Next    => Descriptor_Indexes (1));

         Descriptors (Descriptor_Indexes (1)) :=
           (Address =>
              Resource_Allocation.Physical_Address
              + Storage_Offset (Request_Size),
            Length  => Unsigned_32 (Response_Size),
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

      Device_Registers.Queue_Notify := Device_Control_Queue_Index;

      Blocking_Channel : constant Blocking_Channel_T :=
        Address_To_Unsigned_64 (Address (Resource_Allocation.Virtual_Address));

      Device.Bus_Info_VirtIO.Request_Info (Descriptor_Indexes (0)).Channel :=
        Blocking_Channel;

      Lock_Process_Waiting_For_Channel
        (Blocking_Channel, Device.Spinlock, Reading_Process);

      Test_Result :
      declare
         Response_Data : Virtio_Gpu_Ctrl_Hdr
         with
           Import,
           Address   =>
             Resource_Allocation.Virtual_Address
             + Storage_Offset (Request_Size),
           Alignment => 1;
      begin
         if Response_Data.Ctrl_Type /= Resp_Ok_NoData then
            Log_Error
              ("Failed to flush resource. Response Code: "
               & Response_Data.Ctrl_Type'Image);
            Result := Unhandled_Exception;
            return;
         end if;
      end Test_Result;

      --  Reached once the request is complete and control is returned to
      --  the calling process.
      Free_Descriptor_Chain (Device, Descriptor_Indexes (0), Result);
      if Is_Error (Result) then
         return;
      end if;

      Free_Kernel_Memory (Resource_Allocation.Virtual_Address, Result);
      if Is_Error (Result) then
         return;
      end if;
   exception
      when others =>
         Log_Error ("Constraint_Error: Resource_Flush_Unlocked");
         Result := Constraint_Exception;
   end Resource_Flush_Unlocked;

end Devices.VirtIO.Graphics;
