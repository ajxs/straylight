-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Interfaces;              use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;

with Devices;                use Devices;
with Devices.VirtIO;
with Devices.VirtIO.Graphics;
with Devices.UART;
with Devices.PLIC;
with Filesystems;            use Filesystems;
with Filesystems.Root;       use Filesystems.Root;
with Function_Results;       use Function_Results;
with Graphics;               use Graphics;
with Loader;
with Locks;
with Memory.Allocators;      use Memory.Allocators;
with Memory.Allocators.Heap;
with Memory.Allocators.Page; use Memory.Allocators.Page;
with Memory.Physical;        use Memory.Physical;
with Memory.Virtual;         use Memory.Virtual;
with RISCV;                  use RISCV;
with Scheduler;
with System_State;           use System_State;
with Traps;

package body Boot is
   procedure Allocate_Kernel_Heap is
      Allocated_Physical_Address : Physical_Address_T := Null_Physical_Address;

      Result : Function_Result := Unset;
   begin
      Allocate_Physical_Memory
        (Kernel_Heap_Initial_Size, Allocated_Physical_Address, Result);
      if Is_Error (Result) then
         --  Error already printed.
         Panic;
      end if;

      Current_System_State.Kernel_Heap.Add_Memory_Region_To_Heap
        (Kernel_Heap_Virtual_Address,
         Allocated_Physical_Address,
         Storage_Offset (Kernel_Heap_Initial_Size),
         Result);
      if Is_Error (Result) then
         --  Error already printed.
         Panic;
      end if;
   end Allocate_Kernel_Heap;

   procedure Allocate_Kernel_Page_Pool is
      --  16MiB kernel page pool initial size.
      Kernel_Page_Pool_Initial_Size : constant Positive := 16#100_0000#;

      Kernel_Page_Pool_Region_Count : Natural := 0;
      Page_Pool_Mapping_Offset      : Storage_Offset := 0;

      Allocated_Physical_Address : Physical_Address_T := Null_Physical_Address;

      Result : Function_Result := Unset;
   begin
      Log_Debug ("Allocating kernel page pool...", Logging_Tags);

      Allocate_Physical_Memory
        (Kernel_Page_Pool_Initial_Size, Allocated_Physical_Address, Result);
      if Is_Error (Result) then
         --  Error already printed.
         Panic;
      end if;

      Kernel_Page_Pool_Region_Count :=
        Natural
          (Storage_Offset (Kernel_Page_Pool_Initial_Size)
           / Page_Pool_Region_Size_In_Bytes);

      for I in 1 .. Kernel_Page_Pool_Region_Count loop
         Log_Debug ("Adding region to kernel page pool...", Logging_Tags);
         Page_Pool_Mapping_Offset :=
           Storage_Offset (I - 1) * Page_Pool_Region_Size_In_Bytes;

         Current_System_State.Kernel_Page_Pool.Add_Region_To_Page_Pool
           (Kernel_Page_Pool_Virtual_Address + Page_Pool_Mapping_Offset,
            Allocated_Physical_Address + Page_Pool_Mapping_Offset,
            Result);
         if Is_Error (Result) then
            --  Error already printed.
            Panic;
         end if;
      end loop;

      Log_Debug ("Allocated kernel page pool...", Logging_Tags);
   end Allocate_Kernel_Page_Pool;

   procedure Free_Boot_Memory is
      --  These are the physical memory addresses of the boot section, with the
      --  higher-half offset added. This is so that they are loadable from the
      --  higher-half kernel code to 'free' the physical boot memory.
      Boot_Start_Offset : constant Natural
      with Import, External_Name => "__boot_start_offset";

      Boot_End_Offset : constant Natural
      with Import, External_Name => "__boot_end_offset";

      Result : Function_Result := Unset;
   begin
      Log_Debug ("Freeing boot memory...", Logging_Tags);

      Free_Physical_Memory_Length : constant Natural :=
        Natural (Boot_End_Offset'Address - Boot_Start_Offset'Address);

      Create_Free_Physical_Memory_Region
        (Get_Lower_Physical_Address (Boot_Start_Offset'Address),
         Free_Physical_Memory_Length,
         Result);
      if Is_Error (Result) then
         --  Error already printed.
         Panic;
      end if;

      Log_Debug ("Freed boot memory.", Logging_Tags);
   exception
      when Constraint_Error =>
         Panic ("Constraint_Error: Free_Boot_Memory");
   end Free_Boot_Memory;

   procedure Initialise_Devices is
      --  The start of the virtual memory in which each device is mapped.
      --  This address will be incremented as each device is mapped.
      Device_Virtual_Address : Virtual_Address_T :=
        Device_Mapping_Virtual_Address;

      Supervisor_Interrupt_Context : constant Integer :=
        Get_Current_Hart_Supervisor_Interrupt_Context;

      System_Devices renames Current_System_State.Devices;

      APIC_Device renames System_Devices (1);
      UART_Device renames System_Devices (2);
      Disk_Device renames System_Devices (3);
      Root_Filesystem_Memory_Device renames System_Devices (4);
      Disk_B_Device renames System_Devices (5);
      Graphics_Device renames System_Devices (6);

      Default_Unallocated_Addresses :
        constant VirtIO_Resource_Allocated_Addresses_T :=
          (Virtual_Address  => Null_Address,
           Physical_Address => Null_Physical_Address);

      Root_Filesystem_Physical_Address : Physical_Address_T :=
        Null_Physical_Address;

      Result : Function_Result := Unset;
   begin
      Log_Debug ("Initialising devices...", Logging_Tags);

      APIC_Device :=
        (Device_Class       => Device_Class_Interrupt_Controller,
         Device_Bus         => Device_Bus_Memory_Mapped,
         Memory_Size        => 16#4000000#,
         Virtual_Address    => Device_Virtual_Address,
         Physical_Address   => Physical_Address_T (To_Address (16#C00_0000#)),
         Interrupt_Line     => 0,
         Interrupt_Priority => 0,
         Spinlock           => Locks.Null_Spinlock,
         Record_Used        => True);

      Device_Virtual_Address :=
        Device_Virtual_Address + APIC_Device.Memory_Size;

      UART_Device :=
        (Device_Class       => Device_Class_Serial,
         Device_Bus         => Device_Bus_Memory_Mapped,
         Memory_Size        => 16#1000#,
         Virtual_Address    => Device_Virtual_Address,
         Physical_Address   => Physical_Address_T (To_Address (16#1000_0000#)),
         Interrupt_Line     => 10,
         Interrupt_Priority => 7,
         Spinlock           => Locks.Null_Spinlock,
         Record_Used        => True);

      Device_Virtual_Address :=
        Device_Virtual_Address + UART_Device.Memory_Size;

      Disk_Device :=
        (Device_Class       => Device_Class_Storage,
         Device_Bus         => Device_Bus_VirtIO_MMIO,
         Memory_Size        => 16#1000#,
         Virtual_Address    => Device_Virtual_Address,
         Physical_Address   => Physical_Address_T (To_Address (16#1000_1000#)),
         Interrupt_Line     => 1,
         Interrupt_Priority => 7,
         Spinlock           => Locks.Null_Spinlock,
         Record_Used        => True,
         Bus_Info_VirtIO    =>
           (Device_Type                   => VirtIO_Device_Type_Block,
            Request_Info                  => [others => (Channel => 0)],
            Request_Serviced_Index        => 0,
            Request_Status_Array          => Default_Unallocated_Addresses,
            Q_Used                        => Default_Unallocated_Addresses,
            Q_Available                   => Default_Unallocated_Addresses,
            Q_Descriptor                  => Default_Unallocated_Addresses,
            Descriptor_Status             => [others => True],
            Block_Request_Array_Addresses => Default_Unallocated_Addresses));

      Device_Virtual_Address :=
        Device_Virtual_Address + Disk_Device.Memory_Size;

      Allocate_Physical_Memory
        (16#1000#, Root_Filesystem_Physical_Address, Result);
      if Is_Error (Result) then
         Panic ("Error allocating ramdisk device memory");
      end if;

      Root_Filesystem_Memory_Device :=
        (Device_Class       => Device_Class_Storage,
         Device_Bus         => Device_Bus_Memory_Mapped,
         Memory_Size        => 16#1000#,
         Virtual_Address    => Device_Virtual_Address,
         Physical_Address   => Root_Filesystem_Physical_Address,
         Interrupt_Line     => 0,
         Interrupt_Priority => 0,
         Spinlock           => Locks.Null_Spinlock,
         Record_Used        => True);

      Device_Virtual_Address :=
        Device_Virtual_Address + Root_Filesystem_Memory_Device.Memory_Size;

      Disk_B_Device :=
        (Device_Class       => Device_Class_Storage,
         Device_Bus         => Device_Bus_VirtIO_MMIO,
         Memory_Size        => 16#1000#,
         Virtual_Address    => Device_Virtual_Address,
         Physical_Address   => Physical_Address_T (To_Address (16#1000_2000#)),
         Interrupt_Line     => 2,
         Interrupt_Priority => 7,
         Spinlock           => Locks.Null_Spinlock,
         Record_Used        => True,
         Bus_Info_VirtIO    =>
           (Device_Type                   => VirtIO_Device_Type_Block,
            Request_Info                  => [others => (Channel => 0)],
            Request_Serviced_Index        => 0,
            Request_Status_Array          => Default_Unallocated_Addresses,
            Q_Used                        => Default_Unallocated_Addresses,
            Q_Available                   => Default_Unallocated_Addresses,
            Q_Descriptor                  => Default_Unallocated_Addresses,
            Descriptor_Status             => [others => True],
            Block_Request_Array_Addresses => Default_Unallocated_Addresses));

      Device_Virtual_Address :=
        Device_Virtual_Address + Disk_B_Device.Memory_Size;

      Graphics_Device :=
        (Device_Class       => Device_Class_Graphics,
         Device_Bus         => Device_Bus_VirtIO_MMIO,
         Memory_Size        => 16#1000#,
         Virtual_Address    => Device_Virtual_Address,
         Physical_Address   => Physical_Address_T (To_Address (16#1000_3000#)),
         Interrupt_Line     => 3,
         Interrupt_Priority => 7,
         Spinlock           => Locks.Null_Spinlock,
         Record_Used        => True,
         Bus_Info_VirtIO    =>
           (Device_Type            => VirtIO_Device_Type_Graphics,
            Request_Info           => [others => (Channel => 0)],
            Request_Serviced_Index => 0,
            Request_Status_Array   => Default_Unallocated_Addresses,
            Q_Used                 => Default_Unallocated_Addresses,
            Q_Available            => Default_Unallocated_Addresses,
            Q_Descriptor           => Default_Unallocated_Addresses,
            Descriptor_Status      => [others => True],
            Resource_Id            => 0,
            Framebuffer_Addresses  => Default_Unallocated_Addresses,
            Framebuffer_Width      => 0,
            Framebuffer_Height     => 0));

      Device_Virtual_Address :=
        Device_Virtual_Address + Graphics_Device.Memory_Size;

      for I in System_Devices'Range loop
         if System_Devices (I).Record_Used then
            Log_Debug
              ("Mapping Device "
               & I'Image
               & ": "
               & ASCII.LF
               & "  Class:     "
               & System_Devices (I).Device_Class'Image
               & ASCII.LF
               & "  Virt Addr: "
               & System_Devices (I).Virtual_Address'Image
               & ASCII.LF
               & "  Phys Addr: "
               & System_Devices (I).Physical_Address'Image,
               Logging_Tags);

            --  Map each device into the kernel's address space.
            Map_Kernel_Memory
              (System_Devices (I).Virtual_Address,
               System_Devices (I).Physical_Address,
               System_Devices (I).Memory_Size,
               (True, True, False, False),
               Result);
            if Is_Error (Result) then
               --  Error already printed.
               Panic;
            end if;

            --  Initialise devices.
            if System_Devices (I).Device_Bus = Device_Bus_VirtIO_MMIO then
               Devices.VirtIO.Allocate_VirtIO_Device_Resources
                 (System_Devices (I), Result);
               if Is_Error (Result) then
                  Panic ("Error allocating VirtIO device resources");
               end if;

               Devices.VirtIO.Initialise_MMIO_Device
                 (System_Devices (I), Result);
               if Is_Error (Result) then
                  Panic;
               end if;
            elsif System_Devices (I).Device_Class = Device_Class_Serial then
               Devices.UART.Initialise (UART_Device.Virtual_Address);
               Devices.UART.Set_Interrupt_Generation
                 (UART_Device.Virtual_Address,
                  Devices.UART.Rx_Data_Available,
                  True);
            end if;

            --  If the device has an interrupt line, then enable it.
            if System_Devices (I).Interrupt_Line /= 0 then
               Log_Debug
                 ("Setting interrupt line: "
                  & System_Devices (I).Interrupt_Line'Image,
                  Logging_Tags);

               Devices.PLIC.Set_IRQ_Enable_State
                 (APIC_Device.Virtual_Address,
                  Supervisor_Interrupt_Context,
                  System_Devices (I).Interrupt_Line,
                  True);

               Devices.PLIC.Set_Interrupt_Priority
                 (APIC_Device.Virtual_Address,
                  System_Devices (I).Interrupt_Line,
                  System_Devices (I).Interrupt_Priority);
            end if;
         end if;
      end loop;

      Devices.PLIC.Set_IRQ_Priority_Threshold
        (APIC_Device.Virtual_Address, Supervisor_Interrupt_Context, 1);

      Log_Debug ("Initialised devices.", Logging_Tags);
   exception
      when Constraint_Error =>
         Panic ("Constraint_Error: Initialise_Devices");
   end Initialise_Devices;

   procedure Initialise_Filesystem is
      System_Devices renames Current_System_State.Devices;

      Disk_Device renames System_Devices (3);
      Disk_B_Device renames System_Devices (5);
      Root_Filesystem_Memory_Device renames System_Devices (4);

      Result : Function_Result := Unset;

      Root_Filesystem_Node_Index    : Filesystem_Node_Index_T := 0;
      Devices_Filesystem_Node_Index : Filesystem_Node_Index_T := 0;
      Disk_Filesystem_Node_Index    : Filesystem_Node_Index_T := 0;
   begin
      Log_Debug ("Initialising filesystem...", Logging_Tags);

      Current_System_State.Mounted_Filesystems :=
        [others =>
           (Filesystem_Type              => Filesystem_Type_None,
            Filesystem_Meta_Info_Address => Null_Address,
            Filesystem_Meta_Info_Size    => 0,
            Device                       => null)];

      Current_System_State.Mounted_Filesystems (1) :=
        (Filesystem_Type              => Filesystem_Type_Root,
         Filesystem_Meta_Info_Address => Null_Address,
         Filesystem_Meta_Info_Size    => 0,
         Device                       => Root_Filesystem_Memory_Device'Access);

      Current_System_State.Root_Filesystem :=
        Current_System_State.Mounted_Filesystems (1)'Access;

      Initialise_Root_Filesystem
        (Current_System_State.Root_Filesystem, Result);

      Current_System_State.Mounted_Filesystems (2) :=
        (Filesystem_Type              => Filesystem_Type_FAT,
         Filesystem_Meta_Info_Address => Null_Address,
         Filesystem_Meta_Info_Size    => 0,
         Device                       => Disk_Device'Access);

      Current_System_State.Mounted_Filesystems (3) :=
        (Filesystem_Type              => Filesystem_Type_UStar,
         Filesystem_Meta_Info_Address => Null_Address,
         Filesystem_Meta_Info_Size    => 0,
         Device                       => Disk_B_Device'Access);

      Add_Filesystem_Node_To_Root_Filesystem
        (Current_System_State.Root_Filesystem,
         "/",
         0,
         Root_Filesystem_Node_Index,
         Result,
         Filesystem_Node_Type_Directory);
      if Is_Error (Result) then
         Panic;
      end if;

      Add_Filesystem_Node_To_Root_Filesystem
        (Current_System_State.Root_Filesystem,
         "Devices",
         Root_Filesystem_Node_Index,
         Devices_Filesystem_Node_Index,
         Result,
         Filesystem_Node_Type_Directory);
      if Is_Error (Result) then
         Panic;
      end if;

      Add_Filesystem_Node_To_Root_Filesystem
        (Current_System_State.Root_Filesystem,
         "Disk",
         Devices_Filesystem_Node_Index,
         Disk_Filesystem_Node_Index,
         Result,
         Filesystem_Node_Type_Mounted_Filesystem,
         Current_System_State.Mounted_Filesystems (2)'Access);
      if Is_Error (Result) then
         Panic;
      end if;

      Add_Filesystem_Node_To_Root_Filesystem
        (Current_System_State.Root_Filesystem,
         "Disk_B",
         Devices_Filesystem_Node_Index,
         Disk_Filesystem_Node_Index,
         Result,
         Filesystem_Node_Type_Mounted_Filesystem,
         Current_System_State.Mounted_Filesystems (3)'Access);
      if Is_Error (Result) then
         Panic;
      end if;

      Log_Debug ("Initialised filesystem.", Logging_Tags);
   end Initialise_Filesystem;

   procedure Initialise_Hart (Hart_Id : Integer) is
   begin
      System_State.Hart_States (Hart_Id).Current_Process := null;
      System_State.Hart_States (Hart_Id).Interrupts_Off_Counter := 0;
      System_State.Hart_States (Hart_Id)
        .Were_Interrupts_Enabled_Before_Push_Off :=
        True;
   exception
      when Constraint_Error =>
         Panic ("Constraint_Error: Initialise_Hart");
   end Initialise_Hart;

   procedure Initialise_Init_Process is
      Result : Function_Result := Unset;
   begin
      Create_New_Process (Init_Process, Result);
      if Is_Error (Result) then
         --  Error already printed.
         Panic;
      end if;

      --  Set the Init process' entry point.
      --  Instead of 'returning' to the normal userland process initialisation
      --  procedure, we jump to the kernel-level 'Start_Init_Process'.
      Init_Process.all.Kernel_Context (ra) :=
        Address_To_Unsigned_64 (Start_Init_Process'Address);

      System_State.Add_Process (Init_Process);
   exception
      when Constraint_Error =>
         Panic ("Constraint_Error: Initialise_Init_Process");
   end Initialise_Init_Process;

   procedure Initialise_Kernel_Memory
     (Hart_Id : Integer; DTB_Address : Address)
   is
      procedure Switch_To_Kernel_Address_Space
        (SATP : Unsigned_64; Stack_Pointer : Address)
      with
        No_Return,
        Import,
        Convention    => Assembler,
        External_Name => "switch_to_kernel_address_space";

      Kernel_Address_Space renames Current_System_State.Kernel_Address_Space;

      Boot_Secondary_Stack_Top : Virtual_Address_T := Null_Address;

      Result : Function_Result := Unset;
   begin
      pragma Unreferenced (DTB_Address);

      Log_Debug ("Booting Hart" & Hart_Id'Image & "...", Logging_Tags);

      Initialise_Physical_Memory_Manager;

      Allocate_Kernel_Heap;

      Allocate_Kernel_Page_Pool;

      Initialise_Kernel_Address_Space;

      Initialise_Hart (Hart_Id);

      Log_Debug ("Initialising boot secondary stack...", Logging_Tags);

      Allocate_Physical_Memory
        (Boot_Secondary_Stack_Size, Boot_Secondary_Stack_Phys_Address, Result);
      if Is_Error (Result) then
         --  Error already printed.
         Panic;
      end if;

      Map_Kernel_Memory
        (Boot_Secondary_Stack_Virtual_Address,
         Boot_Secondary_Stack_Phys_Address,
         Boot_Secondary_Stack_Size,
         (True, True, False, False),
         Result);
      if Is_Error (Result) then
         --  Error already printed.
         Panic;
      end if;

      Boot_Secondary_Stack_Top :=
        Boot_Secondary_Stack_Virtual_Address + Boot_Secondary_Stack_Size;

      Log_Debug ("Initialised boot secondary stack.", Logging_Tags);

      Log_Debug ("Jumping to kernel address space...", Logging_Tags);

      Switch_To_Kernel_Address_Space
        (Create_SATP
           (Address (Kernel_Address_Space.Base_Page_Table_Addr),
            Kernel_Address_Space.Address_Space_ID),
         Boot_Secondary_Stack_Top);

   exception
      when Constraint_Error =>
         Panic ("Constraint_Error: Initialise_Kernel_Memory");
   end Initialise_Kernel_Memory;

   procedure Initialise_Kernel_Address_Space is
      use Memory.Allocators.Heap;

      Text_Section_Start_Marker : constant Integer
      with Import, External_Name => "__text_start";

      Text_Section_End_Marker : constant Integer
      with Import, External_Name => "__text_end";

      Rodata_Section_Start_Marker : constant Integer
      with Import, External_Name => "__rodata_start";

      Rodata_Section_End_Marker : constant Integer
      with Import, External_Name => "__rodata_end";

      Data_Section_Start_Marker : constant Integer
      with Import, External_Name => "__data_start";

      Data_Section_End_Marker : constant Integer
      with Import, External_Name => "__data_end";

      Bss_Section_Start_Marker : constant Integer
      with Import, External_Name => "__bss_start";

      Bss_Section_End_Marker : constant Integer
      with Import, External_Name => "__bss_end";

      Region_Size : Memory_Region_Size := 0;

      Heap_Region_Index : Heap_Memory_Region_Index_T :=
        Null_Memory_Region_Index;

      Pool_Region_Index : Positive := 1;
      Kernel_Page_Pool_Regions renames
        Current_System_State.Kernel_Page_Pool.Page_Pool_Regions;

      Kernel_Heap renames Current_System_State.Kernel_Heap;

      Kernel_Address_Space renames Current_System_State.Kernel_Address_Space;

      Result : Function_Result := Unset;
   begin
      Log_Debug ("Initialising kernel address space...", Logging_Tags);

      Memory.Virtual.Create_New_Process_Memory_Space
        (Kernel_Address_Space, Result);
      if Is_Error (Result) then
         --  Error already printed.
         Panic;
      end if;

      Kernel_Address_Space.User_Address_Space := False;

      Region_Size :=
        Text_Section_End_Marker'Address - Text_Section_Start_Marker'Address;

      Map_Kernel_Memory
        (Text_Section_Start_Marker'Address,
         Get_Lower_Physical_Address (Text_Section_Start_Marker'Address),
         Region_Size,
         (True, False, True, False),
         Result);
      if Is_Error (Result) then
         --  Error already printed.
         Panic;
      end if;

      Region_Size :=
        Rodata_Section_End_Marker'Address
        - Rodata_Section_Start_Marker'Address;

      if Region_Size > 0 then
         Map_Kernel_Memory
           (Rodata_Section_Start_Marker'Address,
            Get_Lower_Physical_Address (Rodata_Section_Start_Marker'Address),
            Region_Size,
            (True, False, False, False),
            Result);
         if Is_Error (Result) then
            --  Error already printed.
            Panic;
         end if;
      end if;

      Region_Size :=
        Data_Section_End_Marker'Address - Data_Section_Start_Marker'Address;

      if Region_Size > 0 then
         Map_Kernel_Memory
           (Data_Section_Start_Marker'Address,
            Get_Lower_Physical_Address (Data_Section_Start_Marker'Address),
            Region_Size,
            (True, True, False, False),
            Result);
         if Is_Error (Result) then
            --  Error already printed.
            Panic;
         end if;
      end if;

      Region_Size :=
        Bss_Section_End_Marker'Address - Bss_Section_Start_Marker'Address;

      if Region_Size > 0 then
         Map_Kernel_Memory
           (Bss_Section_Start_Marker'Address,
            Get_Lower_Physical_Address (Bss_Section_Start_Marker'Address),
            Region_Size,
            (True, True, False, False),
            Result);
         if Is_Error (Result) then
            --  Error already printed.
            Panic;
         end if;
      end if;

      --  Map all physical memory into the kernel's address space.
      Map_Kernel_Memory
        (To_Address (Physical_Memory_Map_Address),
         Physical_Address_T (To_Address (0)),
         Physical_Memory_Map_Limit,
         (True, True, False, False),
         Result);
      if Is_Error (Result) then
         --  Error already printed.
         Panic;
      end if;

      Log_Debug ("Mapping kernel heap regions...", Logging_Tags);

      --  Map all kernel heap regions.
      Heap_Region_Index := Kernel_Heap.Memory_Regions_Head;
      while Heap_Region_Index /= Null_Memory_Region_Index loop
         Map_Kernel_Memory
           (Kernel_Heap.Memory_Regions (Heap_Region_Index).Virtual_Address,
            Kernel_Heap.Memory_Regions (Heap_Region_Index).Physical_Address,
            Kernel_Heap.Memory_Regions (Heap_Region_Index).Size,
            (True, True, False, False),
            Result);
         if Is_Error (Result) then
            --  Error already printed.
            Panic;
         end if;

         Heap_Region_Index :=
           Kernel_Heap.Memory_Regions (Heap_Region_Index).Next_Region;
      end loop;

      Log_Debug ("Mapping kernel page pool regions...", Logging_Tags);

      --  Map kernel page pool.
      while Pool_Region_Index <= Max_Page_Pool_Regions loop
         if Kernel_Page_Pool_Regions (Pool_Region_Index).Allocated then
            Log_Debug
              ("Mapping kernel page pool region..."
               & Kernel_Page_Pool_Regions (Pool_Region_Index)
                   .Virtual_Address'Image,
               Logging_Tags);

            Map_Kernel_Memory
              (Kernel_Page_Pool_Regions (Pool_Region_Index).Virtual_Address,
               Kernel_Page_Pool_Regions (Pool_Region_Index).Physical_Address,
               Page_Pool_Region_Size_In_Bytes,
               (True, True, False, False),
               Result);
            if Is_Error (Result) then
               --  Error already printed.
               Panic;
            end if;
         end if;

         Pool_Region_Index := Pool_Region_Index + 1;
      end loop;

      Log_Debug ("Initialised kernel address space.", Logging_Tags);
   exception
      when Constraint_Error =>
         Panic ("Constraint_Error: Initialise_Kernel_Address_Space");
   end Initialise_Kernel_Address_Space;

   procedure Initialise_Physical_Memory_Manager is
      Kernel_End : constant Natural
      with Import, External_Name => "__kernel_end";

      Result : Function_Result := Unset;
   begin
      Log_Debug ("Initialising PMM...", Logging_Tags);

      Free_Physical_Memory_Length : constant Natural :=
        Natural
          (To_Address (16#8800_0000#)
           - (Kernel_End'Address - Higher_Half_Offset));

      Create_Free_Physical_Memory_Region
        (Get_Lower_Physical_Address (Kernel_End'Address),
         Free_Physical_Memory_Length,
         Result);
      if Is_Error (Result) then
         --  Error already printed.
         Panic;
      end if;

      Log_Debug ("Initialised PMM.", Logging_Tags);
   exception
      when Constraint_Error =>
         Panic ("Constraint_Error: Initialise_Physical_Memory_Manager");
   end Initialise_Physical_Memory_Manager;

   procedure Initialise_Kernel_Services is
      Result : Function_Result := Unset;
   begin
      Log_Debug ("Initialising kernel services...", Logging_Tags);

      Traps.Setup_Next_Timer_Interrupt;
      Log_Debug ("Set initial system tick.", Logging_Tags);

      --  Initialise the first process ID to be used.
      Current_System_State.Next_Process_Id := 1;

      Initialise_Devices;

      --  Create the kernel idle process.
      Create_New_Process
        (System_State.Current_System_State.Idle_Process, Result);
      if Is_Error (Result) then
         --  Error already printed.
         Panic;
      end if;

      --  Set the idle process's return address to the idle function.
      --  When the scheduler switches to the idle process, it will 'return' to
      --  the idle function address.
      System_State.Current_System_State.Idle_Process.all.Kernel_Context (ra) :=
        Address_To_Unsigned_64 (Idle'Address);

      Initialise_Block_Cache;

      Initialise_Filesystem;

      Initialise_Init_Process;

      Free_Boot_Memory;

      Log_Debug ("Starting scheduler...", Logging_Tags);

      Scheduler.Run;
   exception
      when Constraint_Error =>
         Panic ("Constraint_Error: Initialise_Kernel_Services");
   end Initialise_Kernel_Services;

   procedure Initialise_Graphics is
      Result : Function_Result := Unset;

      Resource_Id        : constant := 1;
      Framebuffer_Width  : constant := 640;
      Framebuffer_Height : constant := 480;
      Framebuffer_Size   : constant :=
        Framebuffer_Width * Framebuffer_Height * 4;

      Framebuffer_Allocation : Memory_Allocation_Result;

      Graphics_Device renames Current_System_State.Devices (6);
   begin
      Devices.VirtIO.Graphics.Get_Display_Info
        (Init_Process.all, Graphics_Device, Result);
      if Is_Error (Result) then
         Panic;
      end if;

      Log_Debug ("Allocating framebuffer memory...", Logging_Tags);
      Allocate_Kernel_Physical_Memory
        (Framebuffer_Size, Framebuffer_Allocation, Result);
      Log_Debug
        ("Allocated framebuffer: "
         & ASCII.LF
         & "  Virt Addr: "
         & Framebuffer_Allocation.Virtual_Address'Image
         & ASCII.LF
         & "  Phys Addr: "
         & Framebuffer_Allocation.Physical_Address'Image,
         Logging_Tags);

      Graphics_Device.Bus_Info_VirtIO.Resource_Id := Resource_Id;
      Graphics_Device.Bus_Info_VirtIO.Framebuffer_Addresses :=
        Framebuffer_Allocation;
      Graphics_Device.Bus_Info_VirtIO.Framebuffer_Width := Framebuffer_Width;
      Graphics_Device.Bus_Info_VirtIO.Framebuffer_Height := Framebuffer_Height;

      Devices.VirtIO.Graphics.Created_2d_Resource
        (Init_Process.all, Graphics_Device, Resource_Id, Result);
      if Is_Error (Result) then
         Panic;
      end if;

      Devices.VirtIO.Graphics.Attach_Framebuffer_To_Resource
        (Init_Process.all,
         Graphics_Device,
         Resource_Id,
         Framebuffer_Allocation.Physical_Address,
         Result);
      if Is_Error (Result) then
         Panic;
      end if;

      Devices.VirtIO.Graphics.Set_Scanout
        (Init_Process.all,
         Graphics_Device,
         Resource_Id,
         0,
         Framebuffer_Width,
         Framebuffer_Height,
         Result);
      if Is_Error (Result) then
         Panic;
      end if;

      Fill_Framebuffer
        (Framebuffer_Allocation.Virtual_Address,
         Framebuffer_Width,
         Framebuffer_Height,
         Make_Colour (Red => 0, Green => 0, Blue => 0, Alpha => 255));

      Devices.VirtIO.Graphics.Transfer_To_Host_2d
        (Init_Process.all,
         Graphics_Device,
         Resource_Id,
         0,
         0,
         Framebuffer_Width,
         Framebuffer_Height,
         Result);
      if Is_Error (Result) then
         Panic;
      end if;

      Devices.VirtIO.Graphics.Resource_Flush
        (Init_Process.all,
         Graphics_Device,
         Resource_Id,
         0,
         0,
         Framebuffer_Width,
         Framebuffer_Height,
         Result);
      if Is_Error (Result) then
         Panic;
      end if;
   exception
      when Constraint_Error =>
         Panic ("Constraint_Error: Initialise_Graphics");
   end Initialise_Graphics;

   procedure Start_Init_Process is
      Result : Function_Result := Unset;
   begin
      Log_Debug ("Starting init process...", Logging_Tags);

      --  The graphics system is initialised here because the VirtIO driver
      --  requires a process context to operate in.
      Initialise_Graphics;

      Loader.Load_New_Process_From_Filesystem
        (Init_Process.all,
         "/Devices/Disk/Programs/print_random_words.elf",
         Result);

      Loader.Load_New_Process_From_Filesystem
        (Init_Process.all,
         "/Devices/Disk/Programs/print_more_words.elf",
         Result);

      Loader.Load_New_Process_From_Filesystem
        (Init_Process.all,
         "/Devices/Disk/Programs/print_fractal_pattern.elf",
         Result);

      Exit_Process (Init_Process.all, Result);
      if Is_Error (Result) then
         Panic;
      end if;

      --  Because we exit the process *in* kernel-space, we need to explicitly
      --  run the scheduler here to switch to another process.
      --  We don't call scheduler from the Exit_process procedure because we
      --  want to keep the ability to exit userland processes without
      --  interrupting the normal scheduling.
      Scheduler.Run;
      Panic ("Init_Process still running after exit.");
   exception
      when Constraint_Error =>
         Panic ("Constraint_Error: Start_Init_Process");
   end Start_Init_Process;

end Boot;
