-------------------------------------------------------------------------------
--  Copyright (c) 2026, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with GNAT.Debug_Utilities;
with System.Storage_Elements; use System.Storage_Elements;

with Devices;              use Devices;
with Devices.Console;
with Devices.Virtio;       use Devices.Virtio;
with Devices.Virtio.Block; use Devices.Virtio.Block;
with Devices.UART;
with Devices.PLIC;
with Function_Results;     use Function_Results;
with Locks;                use Locks;
with Logging;              use Logging;
with Memory.Physical;      use Memory.Physical;
with Memory.Virtual;       use Memory.Virtual;
with Utilities;            use Utilities;

package body Boot.Device_Discovery is
   --  The Devicetree phandle of Interrupt_Controller's node, so that devices
   --  can find it by their 'interrupt-parent'. 0 until the PLIC is created.
   Interrupt_Controller_Phandle : Unsigned_32 := 0;

   ----------------------------------------------------------------------------
   --  Returns the index of the next unused System_Devices entry, in the order
   --  devices are created. Panics if every entry is in use.
   --  Each device is created by assigning to System_Devices (Index) directly.
   --  Through a Device_Access, an entry is constrained to its current
   --  Driver_Type, so assigning a new device through one raises
   --  Constraint_Error. Only the Device_Access is kept, not the index.
   ----------------------------------------------------------------------------
   procedure Allocate_System_Device_Index (Index : out Positive) is
   begin
      if Attached_System_Device_Count >= System_Devices'Length then
         Panic ("No unused system device entries");
      end if;

      Index := System_Devices'First + Attached_System_Device_Count;
      Attached_System_Device_Count := Attached_System_Device_Count + 1;
   exception
      when Constraint_Error =>
         Panic_Constraint_Error;
   end Allocate_System_Device_Index;

   procedure Create_PLIC_Device
     (Device          : out Device_Access;
      Node            : Devicetree_Device_T;
      Virtual_Address : Virtual_Address_T)
   is
      Device_Index : Positive;
   begin
      Allocate_System_Device_Index (Device_Index);

      System_Devices (Device_Index) :=
        (Driver_Type      => Driver_Type_PLIC,
         Common           =>
           (Memory_Size      =>
              Align_Memory_Size_Up_To_Page_Boundary (Node.Size),
            Virtual_Address  => Virtual_Address,
            Physical_Address => Node.Base,
            others           => <>),
         Interrupt_Parent => null,
         PLIC             => <>);

      --  Record each hart's supervisor-mode PLIC context in the device's
      --  state. This allows the PLIC to route interrupts to the correct
      --  context, depending on which hart the interrupt occurred on.
      for Hart of Devicetree_Harts (1 .. Devicetree_Hart_Count) loop
         Hart_Has_Valid_Index : constant Boolean :=
           Hart.Hart_Id <= Unsigned_64 (Hart_Index_T'Last);

         if Hart.Has_Supervisor_PLIC_Context and then Hart_Has_Valid_Index then
            System_Devices (Device_Index).PLIC.Supervisor_Contexts
              (Hart_Index_T (Hart.Hart_Id)) :=
              Hart.Supervisor_PLIC_Context;
         end if;
      end loop;

      Device := System_Devices (Device_Index)'Access;
   exception
      when Constraint_Error =>
         Panic_Constraint_Error;
   end Create_PLIC_Device;

   procedure Create_UART_Device
     (Device           : out Device_Access;
      Node             : Devicetree_Device_T;
      Virtual_Address  : Virtual_Address_T;
      Interrupt_Parent : Device_Access)
   is
      Index : Positive;
   begin
      Allocate_System_Device_Index (Index);

      System_Devices (Index) :=
        (Driver_Type      => Driver_Type_UART,
         Common           =>
           (Memory_Size        =>
              Align_Memory_Size_Up_To_Page_Boundary (Node.Size),
            Virtual_Address    => Virtual_Address,
            Physical_Address   => Node.Base,
            Interrupt_Line     => Integer (Node.Interrupt_Line),
            Interrupt_Priority => 7,
            Spinlock           => Locks.Null_Spinlock),
         Interrupt_Parent => Interrupt_Parent,
         UART             => (others => <>));

      Device := System_Devices (Index)'Access;
   exception
      when Constraint_Error =>
         Panic_Constraint_Error;
   end Create_UART_Device;

   procedure Create_Console_Device
     (Device : out Device_Access; Backend : Device_Access)
   is
      Index : Positive;
   begin
      Allocate_System_Device_Index (Index);

      System_Devices (Index) :=
        (Driver_Type      => Driver_Type_Console,
         Common           =>
           (Virtual_Address  => Null_Address,
            Physical_Address => Null_Physical_Address,
            others           => <>),
         Interrupt_Parent => null,
         Console          => (Backend => Backend, others => <>));

      System_Devices (Index).Console.Line_Read_Lock.Spinlock.Lock_Id :=
        Lock_Id_Console_Line_Read;

      Device := System_Devices (Index)'Access;
   exception
      when Constraint_Error =>
         Panic_Constraint_Error;
   end Create_Console_Device;

   procedure Create_Ramdisk_Device
     (Device           : out Device_Access;
      Physical_Address : Physical_Address_T;
      Size             : Storage_Count;
      Virtual_Address  : Virtual_Address_T)
   is
      Index : Positive;
   begin
      Allocate_System_Device_Index (Index);

      System_Devices (Index) :=
        (Driver_Type      => Driver_Type_Ramdisk,
         Common           =>
           (Memory_Size        => Size,
            Virtual_Address    => Virtual_Address,
            Physical_Address   => Physical_Address,
            Interrupt_Line     => 0,
            Interrupt_Priority => 0,
            Spinlock           => Locks.Null_Spinlock),
         Interrupt_Parent => null);

      Device := System_Devices (Index)'Access;
   exception
      when Constraint_Error =>
         Panic_Constraint_Error;
   end Create_Ramdisk_Device;

   procedure Create_Virtio_Block_Device
     (Node             : Devicetree_Device_T;
      Virtual_Address  : Virtual_Address_T;
      Interrupt_Parent : Device_Access)
   is
      --  Default unallocated addresses for Virtio device resources.
      --  When using this constant value for the unallocated address pairs in
      --  the Virtio device structures, it causes the GNAT compiler to make
      --  the change directly in the system device array. Without using the
      --  constant value, GNAT will construct the structures temporarily on
      --  the stack, which will overflow the 4 KiB early kernel stack. This
      --  issue needs to be resolved in the future.
      Default_Unallocated_Addresses :
        constant Virtual_Physical_Address_Pair_T := (others => <>);

      Driver_Features : constant Virtio_Device_Features_Pages_T :=
        [not Create_U32_Bitmask_From_Flags
               ([VIRTIO_BLK_F_RO,
                 VIRTIO_BLK_F_SCSI,
                 VIRTIO_BLK_F_CONFIG_WCE,
                 VIRTIO_BLK_F_MQ,
                 VIRTIO_F_ANY_LAYOUT,
                 VIRTIO_F_INDIRECT_DESC,
                 VIRTIO_F_EVENT_IDX]),
         Create_U32_Bitmask_From_Flags ([VIRTIO_F_VERSION_1])];

      Device_Index : Positive;
   begin
      Allocate_System_Device_Index (Device_Index);

      System_Devices (Device_Index) :=
        (Driver_Type      => Driver_Type_Virtio_Block,
         Common           =>
           (Memory_Size        =>
              Align_Memory_Size_Up_To_Page_Boundary (Node.Size),
            Virtual_Address    => Virtual_Address,
            Physical_Address   => Node.Base,
            Interrupt_Line     => Integer (Node.Interrupt_Line),
            Interrupt_Priority => 7,
            Spinlock           => Locks.Null_Spinlock),
         Interrupt_Parent => Interrupt_Parent,
         Virtio_Block     =>
           (Virtio                        =>
              (Driver_Features           => Driver_Features,
               Request_Serviced_Index    => 0,
               Request_Status_Array      => Default_Unallocated_Addresses,
               Q_Used                    => Default_Unallocated_Addresses,
               Q_Available               => Default_Unallocated_Addresses,
               Q_Descriptor              => Default_Unallocated_Addresses,
               Request_Info_Address      => Null_Address,
               Descriptor_Status_Address => Null_Address),
            Block_Request_Array_Addresses => Default_Unallocated_Addresses,
            Total_Sectors                 => 0));
   exception
      when Constraint_Error =>
         Panic_Constraint_Error;
   end Create_Virtio_Block_Device;

   procedure Create_Virtio_GPU_Device
     (Device           : out Device_Access;
      Node             : Devicetree_Device_T;
      Virtual_Address  : Virtual_Address_T;
      Interrupt_Parent : Device_Access)
   is
      --  Refer to the comment on this constant in Create_Virtio_Block_Device.
      Default_Unallocated_Addresses :
        constant Virtual_Physical_Address_Pair_T := (others => <>);

      Driver_Features : constant Virtio_Device_Features_Pages_T :=
        [not Create_U32_Bitmask_From_Flags
               ([VIRTIO_F_ANY_LAYOUT,
                 VIRTIO_F_INDIRECT_DESC,
                 VIRTIO_F_EVENT_IDX]),
         Create_U32_Bitmask_From_Flags ([VIRTIO_F_VERSION_1])];

      Device_Index : Positive;
   begin
      Allocate_System_Device_Index (Device_Index);

      System_Devices (Device_Index) :=
        (Driver_Type      => Driver_Type_Virtio_GPU,
         Common           =>
           (Memory_Size        =>
              Align_Memory_Size_Up_To_Page_Boundary (Node.Size),
            Virtual_Address    => Virtual_Address,
            Physical_Address   => Node.Base,
            Interrupt_Line     => Integer (Node.Interrupt_Line),
            Interrupt_Priority => 7,
            Spinlock           => Locks.Null_Spinlock),
         Interrupt_Parent => Interrupt_Parent,
         Virtio_GPU       =>
           (Virtio                =>
              (Driver_Features           => Driver_Features,
               Request_Serviced_Index    => 0,
               Request_Status_Array      => Default_Unallocated_Addresses,
               Q_Used                    => Default_Unallocated_Addresses,
               Q_Available               => Default_Unallocated_Addresses,
               Q_Descriptor              => Default_Unallocated_Addresses,
               Request_Info_Address      => Null_Address,
               Descriptor_Status_Address => Null_Address),
            Resource_Id           => 0,
            Framebuffer_Addresses => Default_Unallocated_Addresses,
            Framebuffer_Width     => 0,
            Framebuffer_Height    => 0));

      Device := System_Devices (Device_Index)'Access;
   exception
      when Constraint_Error =>
         Panic_Constraint_Error;
   end Create_Virtio_GPU_Device;

   ----------------------------------------------------------------------------
   --  The interrupt controller that a node's device raises interrupts on,
   --  found by the node's 'interrupt-parent' phandle.
   --  A node with no 'interrupts' property gets given a default interrupt
   --  line value of 0. Since it doesn't raise interrupts, don't assign it an
   --  interrupt parent.
   --  The PLIC is the only interrupt controller the kernel supports, so a
   --  device whose interrupt parent is anything else gets none, and its
   --  interrupts are never enabled.
   ----------------------------------------------------------------------------
   function Get_Interrupt_Parent
     (Node : Devicetree_Device_T) return Device_Access is
   begin
      if Node.Interrupt_Line = 0 then
         return null;
      end if;

      if Node.Interrupt_Parent = 0
        or else Node.Interrupt_Parent /= Interrupt_Controller_Phandle
      then
         Log_Error
           ("Device's interrupt parent isn't the PLIC, so its interrupts are"
            & " disabled: addr_phys="
            & GNAT.Debug_Utilities.Image_C (Address (Node.Base)));
         return null;
      end if;

      return Interrupt_Controller;
   end Get_Interrupt_Parent;

   ----------------------------------------------------------------------------
   --  Creates a system device for a 'virtio,mmio' compatible Devicetree node.
   --  The Devicetree populates a 'virtio,mmio' node for every Virtio slot,
   --  whether or not any device is actually attached. To determine whether
   --  a device is present, each slot is probed for its device ID. This
   --  reads the device registers through the kernel's map of all physical
   --  memory, so device slots never need to be mapped into kernel memory
   --  just to validate.
   ----------------------------------------------------------------------------
   procedure Create_Attached_Virtio_Device
     (Node : Devicetree_Device_T; Virtual_Address : Virtual_Address_T)
   is
      Virtio_Device_ID : constant Unsigned_32 :=
        Get_MMIO_Device_ID (Get_Physical_Address_Virtual_Mapping (Node.Base));

      --  This is a separate procedure, so that its exception handler is
      --  kept whether or not Debug_Boot_Device_Discovery is enabled.
      procedure Log_Probed_Virtio_Slot is
      begin
         Log_Debug
           ("Probed Virtio slot: addr_phys="
            & GNAT.Debug_Utilities.Image_C (Address (Node.Base))
            & " device_id="
            & Virtio_Device_ID'Image (2 .. Virtio_Device_ID'Image'Length));
      exception
         when Constraint_Error =>
            Log_Constraint_Error;
      end Log_Probed_Virtio_Slot;
   begin
      pragma Debug (Debug_Boot_Device_Discovery, Log_Probed_Virtio_Slot);

      case Virtio_Device_ID is
         when Virtio_Device_ID_Block =>
            Create_Virtio_Block_Device
              (Node, Virtual_Address, Get_Interrupt_Parent (Node));

         when Virtio_Device_ID_GPU   =>
            if Graphics_Device = null then
               Create_Virtio_GPU_Device
                 (Graphics_Device,
                  Node,
                  Virtual_Address,
                  Get_Interrupt_Parent (Node));
            end if;

         when others                 =>
            --  An empty slot, or a device without a driver.
            null;
      end case;
   end Create_Attached_Virtio_Device;

   ----------------------------------------------------------------------------
   --  Creates a system device from a parsed Devicetree node, if applicable.
   --  This procedure assigns each created device their own virtual memory
   --  space, as required.
   --  Until a more comprehensive device discovery mechanism is implemented,
   --  this procedure only creates a single UART/PLIC device, even if multiple
   --  nodes of these types exist in the Devicetree.
   ----------------------------------------------------------------------------
   procedure Create_System_Device_From_Devicetree_Node
     (Node : Devicetree_Device_T; Virtual_Address : in out Virtual_Address_T)
   is
   begin
      case Node.Device_Type is
         when Devicetree_Device_Type_PLIC          =>
            if Interrupt_Controller = null then
               Create_PLIC_Device
                 (Interrupt_Controller, Node, Virtual_Address);

               Interrupt_Controller_Phandle := Node.Phandle;
            end if;

         when Devicetree_Device_Type_UART_NS16550A =>
            if UART_Device = null then
               Create_UART_Device
                 (UART_Device,
                  Node,
                  Virtual_Address,
                  Get_Interrupt_Parent (Node));
            end if;

         when Devicetree_Device_Type_Virtio_MMIO   =>
            Create_Attached_Virtio_Device (Node, Virtual_Address);

         when Devicetree_Device_Type_None          =>
            null;
      end case;

      --  Nodes without a device, such as empty Virtio slots, still occupy
      --  virtual address space. This isn't a significant issue due to the
      --  large amount of space reserved for devices.
      Virtual_Address :=
        Virtual_Address + Align_Memory_Size_Up_To_Page_Boundary (Node.Size);
   exception
      when Constraint_Error =>
         Panic_Constraint_Error;
   end Create_System_Device_From_Devicetree_Node;

   procedure Initialise_Devices is
      --  The start of the virtual memory in which each device is mapped.
      --  This address will be incremented as each device is mapped.
      Device_Virtual_Address : Virtual_Address_T :=
        Device_Mapping_Virtual_Address;

      Root_Filesystem_Physical_Address : Physical_Address_T :=
        Null_Physical_Address;

      Ramdisk_Size : constant := 16#1000#;

      Result : Function_Result := Unset;
   begin
      pragma
        Debug
          (Debug_Boot_Device_Discovery, Log_Debug ("Initialising devices..."));

      --  First, scan for any interrupt controller device nodes.
      --  Interrupt controllers are created first, because each interrupt
      --  raising device references its interrupt controller.
      --  In QEMU's Devicetree, the PLIC device node comes after the devices
      --  that use it, so we can safely create it first, then reference it from
      --  other devices.
      --  Creating it first also gives it the first System_Devices entry, so
      --  the init loop below maps it before any device enables an interrupt
      --  line on it.
      for Node of Devicetree_Devices (1 .. Devicetree_Device_Count) loop
         if Node.Device_Type = Devicetree_Device_Type_PLIC then
            Create_System_Device_From_Devicetree_Node
              (Node, Device_Virtual_Address);
         end if;
      end loop;

      --  If there's no interrupt controller, panic.
      --  The kernel can't currently handle this scenario.
      if Interrupt_Controller = null then
         Panic ("No PLIC found in the Devicetree");
      end if;

      --  Only the boot hart enables PLIC interrupts, so we need to ensure it
      --  has a valid supervisor interrupt context.
      if not (for some Hart of Devicetree_Harts (1 .. Devicetree_Hart_Count) =>
                Hart.Hart_Id = Unsigned_64 (Get_Current_Hart_Id)
                and then Hart.Has_Supervisor_PLIC_Context)
      then
         Panic ("No PLIC supervisor context for the boot hart");
      end if;

      Supervisor_Interrupt_Context : constant Natural :=
        Interrupt_Controller.all.PLIC.Supervisor_Contexts
          (Get_Current_Hart_Id);

      --  Next, scan the remaining nodes and create system devices for them.
      for Node of Devicetree_Devices (1 .. Devicetree_Device_Count) loop
         if Node.Device_Type /= Devicetree_Device_Type_PLIC then
            Create_System_Device_From_Devicetree_Node
              (Node, Device_Virtual_Address);
         end if;
      end loop;

      --  For temporary development puporses, panic if these devices aren't
      --  present. This will be removed when more comprehensive device
      --  discovery is implemented.
      if UART_Device = null then
         Panic ("No UART found in the Devicetree");
      end if;

      Create_Console_Device (Console_Device, UART_Device);

      Allocate_Physical_Memory
        (Ramdisk_Size, Root_Filesystem_Physical_Address, Result);
      if Is_Error (Result) then
         Panic ("Error allocating ramdisk device memory");
      end if;

      Create_Ramdisk_Device
        (Root_Filesystem_Memory_Device,
         Root_Filesystem_Physical_Address,
         Ramdisk_Size,
         Device_Virtual_Address);

      for I in System_Devices'Range loop
         --  Assign a unique Lock ID to the device spinlocks.
         System_Devices (I).Common.Spinlock.Lock_Id :=
           Lock_Id_Devices_Prefix + Lock_Id_T (I);

         if System_Devices (I).Driver_Type /= Driver_Type_None then
            Device : Device_T renames System_Devices (I);

            pragma
              Debug
                (Debug_Boot_Device_Discovery,
                 Log_Debug
                   ("Mapping Device: id="
                    & I'Image (2 .. I'Image'Length)
                    --  Because the kernel uses pragma Discard_Names,
                    --  Driver_Type is printed as an integer. For this reason
                    --  the first space can be safely trimmed.
                    & " driver_type="
                    & Device.Driver_Type'Image
                        (2 .. Device.Driver_Type'Image'Length)
                    & " addr_virt="
                    & GNAT.Debug_Utilities.Image_C
                        (Device.Common.Virtual_Address)
                    & " addr_phys="
                    & GNAT.Debug_Utilities.Image_C
                        (Address (Device.Common.Physical_Address))
                    & " irq="
                    & Device.Common.Interrupt_Line'Image
                        (2 .. Device.Common.Interrupt_Line'Image'Length)));

            if Device.Common.Memory_Size /= 0 then
               --  Map each device into the kernel's address space.
               Map_Kernel_Memory
                 (Device.Common.Virtual_Address,
                  Device.Common.Physical_Address,
                  Device.Common.Memory_Size,
                  (True, True, False, False),
                  Result);
               if Is_Error (Result) then
                  --  Error already printed.
                  Panic;
               end if;
            end if;

            --  Initialise devices.
            case Device.Driver_Type is
               when Driver_Type_Virtio_Block
               =>
                  Devices.Virtio.Initialise_MMIO_Device
                    (Device.Common, Device.Virtio_Block.Virtio, Result);
                  if Is_Error (Result) then
                     Panic;
                  end if;

                  Devices.Virtio.Block.Initialise_Block_Device
                    (Device.Common, Device.Virtio_Block, Result);
                  if Is_Error (Result) then
                     Panic;
                  end if;

               when Driver_Type_Virtio_GPU
               =>
                  Devices.Virtio.Initialise_MMIO_Device
                    (Device.Common, Device.Virtio_GPU.Virtio, Result);
                  if Is_Error (Result) then
                     Panic;
                  end if;

               when Driver_Type_UART
               =>
                  Devices.UART.Initialise (Device.Common, Device.UART, Result);
                  if Is_Error (Result) then
                     Panic;
                  end if;

                  Devices.UART.Set_Interrupt_Generation
                    (Device.Common, Devices.UART.Rx_Data_Available, True);

               when Driver_Type_Console
               =>
                  Devices.Console.Initialise (Device.Console, Result);
                  if Is_Error (Result) then
                     Panic;
                  end if;

               when Driver_Type_None | Driver_Type_PLIC | Driver_Type_Ramdisk
               =>
                  null;
            end case;

            --  If the device raises interrupts, record it as the owner of its
            --  interrupt line, then enable the line on its controller.
            if Device.Interrupt_Parent /= null then
               Parent : Device_T renames Device.Interrupt_Parent.all;

               Parent.PLIC.Interrupt_Devices
                 (Unsigned_32 (Device.Common.Interrupt_Line)) :=
                 Device'Access;

               Devices.PLIC.Set_IRQ_Enable_State
                 (Parent.Common,
                  Supervisor_Interrupt_Context,
                  Device.Common.Interrupt_Line,
                  True);

               Devices.PLIC.Set_Interrupt_Priority
                 (Parent.Common,
                  Device.Common.Interrupt_Line,
                  Device.Common.Interrupt_Priority,
                  Result);
               if Is_Error (Result) then
                  Panic;
               end if;
            end if;
         end if;
      end loop;

      Devices.PLIC.Set_IRQ_Priority_Threshold
        (Interrupt_Controller.all.Common, Supervisor_Interrupt_Context, 1);

      pragma
        Debug
          (Debug_Boot_Device_Discovery, Log_Debug ("Initialised devices."));
   exception
      when Constraint_Error =>
         Panic_Constraint_Error;
   end Initialise_Devices;

end Boot.Device_Discovery;
