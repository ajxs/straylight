-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with System.Storage_Elements; use System.Storage_Elements;

with Devices;           use Devices;
with Devices.VirtIO;
with Devices.VirtIO.Graphics;
with Devices.UART;
with Devices.PLIC;
with Filesystems;       use Filesystems;
with Filesystems.Root;  use Filesystems.Root;
with Function_Results;  use Function_Results;
with Graphics;          use Graphics;
with Loader;
with Locks;
with Memory.Allocators; use Memory.Allocators;
with Memory.Kernel;     use Memory.Kernel;
with Memory.Physical;   use Memory.Physical;
with Memory.Virtual;    use Memory.Virtual;
with RISCV.SBI;
with Processes.Scheduler;
with Traps;

package body Boot is
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

      Supervisor_Interrupt_Context : constant Natural :=
        Get_Current_Hart_Supervisor_Interrupt_Context;

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
               Devices.UART.Initialise (UART_Device);
               Devices.UART.Set_Interrupt_Generation
                 (UART_Device, Devices.UART.Rx_Data_Available, True);
            end if;

            --  If the device has an interrupt line, then enable it.
            if System_Devices (I).Interrupt_Line /= 0 then
               Log_Debug
                 ("Setting interrupt line: "
                  & System_Devices (I).Interrupt_Line'Image,
                  Logging_Tags);

               Devices.PLIC.Set_IRQ_Enable_State
                 (APIC_Device,
                  Supervisor_Interrupt_Context,
                  System_Devices (I).Interrupt_Line,
                  True,
                  Result);
               if Is_Error (Result) then
                  Panic;
               end if;

               Devices.PLIC.Set_Interrupt_Priority
                 (APIC_Device,
                  System_Devices (I).Interrupt_Line,
                  System_Devices (I).Interrupt_Priority,
                  Result);
               if Is_Error (Result) then
                  Panic;
               end if;
            end if;
         end if;
      end loop;

      Devices.PLIC.Set_IRQ_Priority_Threshold
        (APIC_Device, Supervisor_Interrupt_Context, 1);

      Log_Debug ("Initialised devices.", Logging_Tags);
   exception
      when Constraint_Error =>
         Panic ("Constraint_Error: Initialise_Devices");
   end Initialise_Devices;

   procedure Initialise_Filesystem is
      Disk_Device renames System_Devices (3);
      Disk_B_Device renames System_Devices (5);
      Root_Filesystem_Memory_Device renames System_Devices (4);

      Result : Function_Result := Unset;

      Root_Filesystem_Node_Index    : Filesystem_Node_Index_T := 0;
      Devices_Filesystem_Node_Index : Filesystem_Node_Index_T := 0;
      Disk_Filesystem_Node_Index    : Filesystem_Node_Index_T := 0;
   begin
      Log_Debug ("Initialising filesystem...", Logging_Tags);

      Filesystems.Mounted_Filesystems :=
        [others =>
           (Filesystem_Type              => Filesystem_Type_None,
            Filesystem_Meta_Info_Address => Null_Address,
            Filesystem_Meta_Info_Size    => 0,
            Device                       => null)];

      Filesystems.Mounted_Filesystems (1) :=
        (Filesystem_Type              => Filesystem_Type_Root,
         Filesystem_Meta_Info_Address => Null_Address,
         Filesystem_Meta_Info_Size    => 0,
         Device                       => Root_Filesystem_Memory_Device'Access);

      Filesystems.System_Root_Filesystem :=
        Filesystems.Mounted_Filesystems (1)'Access;

      Initialise_Root_Filesystem (Filesystems.System_Root_Filesystem, Result);

      Filesystems.Mounted_Filesystems (2) :=
        (Filesystem_Type              => Filesystem_Type_FAT,
         Filesystem_Meta_Info_Address => Null_Address,
         Filesystem_Meta_Info_Size    => 0,
         Device                       => Disk_Device'Access);

      Filesystems.Mounted_Filesystems (3) :=
        (Filesystem_Type              => Filesystem_Type_UStar,
         Filesystem_Meta_Info_Address => Null_Address,
         Filesystem_Meta_Info_Size    => 0,
         Device                       => Disk_B_Device'Access);

      Add_Filesystem_Node_To_Root_Filesystem
        (Filesystems.System_Root_Filesystem,
         "/",
         0,
         Root_Filesystem_Node_Index,
         Result,
         Filesystem_Node_Type_Directory);
      if Is_Error (Result) then
         Panic;
      end if;

      Add_Filesystem_Node_To_Root_Filesystem
        (Filesystems.System_Root_Filesystem,
         "Devices",
         Root_Filesystem_Node_Index,
         Devices_Filesystem_Node_Index,
         Result,
         Filesystem_Node_Type_Directory);
      if Is_Error (Result) then
         Panic;
      end if;

      Add_Filesystem_Node_To_Root_Filesystem
        (Filesystems.System_Root_Filesystem,
         "Disk",
         Devices_Filesystem_Node_Index,
         Disk_Filesystem_Node_Index,
         Result,
         Filesystem_Node_Type_Mounted_Filesystem,
         Filesystems.Mounted_Filesystems (2)'Access);
      if Is_Error (Result) then
         Panic;
      end if;

      Add_Filesystem_Node_To_Root_Filesystem
        (Filesystems.System_Root_Filesystem,
         "Disk_B",
         Devices_Filesystem_Node_Index,
         Disk_Filesystem_Node_Index,
         Result,
         Filesystem_Node_Type_Mounted_Filesystem,
         Filesystems.Mounted_Filesystems (3)'Access);
      if Is_Error (Result) then
         Panic;
      end if;

      Log_Debug ("Initialised filesystem.", Logging_Tags);
   end Initialise_Filesystem;

   procedure Initialise_Hart (Hart_Id : Integer) is
      procedure Save_Hart_State_Pointer (Hart_State_Address : Address)
      with
        Import,
        Convention    => Assembler,
        External_Name => "store_hart_state_pointer";
   begin
      Hart_States (Hart_Id) :=
        (Current_Process                            => null,
         Hart_Id                                    => Hart_Index_T (Hart_Id),
         Interrupts_Off_Counter                     => 0,
         Interrupts_Enabled_Before_Initial_Push_Off => False,
         Hart_Status                                => Hart_State_Initialised);

      Save_Hart_State_Pointer (Hart_States (Hart_Id)'Address);
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

      Add_Process (Init_Process, Result);
      if Is_Error (Result) then
         Panic ("Error adding init process");
      end if;
   exception
      when Constraint_Error =>
         Panic ("Constraint_Error: Initialise_Init_Process");
   end Initialise_Init_Process;

   procedure Kernel_Main (Hart_Id : Integer; DTB_Address : Address) is
      Result : Function_Result := Unset;
   begin
      --  This needs to be the first function called on each hart to set up
      --  the hart's state structure. This needs be called before any logging
      --  can take place, since the spinlock mechanism requires being able to
      --  determine the current hart id.
      Initialise_Hart (Hart_Id);

      Log_Debug ("Booting Hart" & Hart_Id'Image & "...", Logging_Tags);

      --  This logging line is to avoid an obscure compile-time error if the
      --  DTB_Address argument is set as 'unreferenced'.
      --  Error: pragma "Unreferenced" argument must be in same declarative
      --  part
      Log_Debug ("DTB_Address: " & DTB_Address'Image, Logging_Tags);

      Log_Debug ("Initialising core kernel subsystems...", Logging_Tags);

      Initialise_Physical_Memory_Manager;

      Initialise_Kernel_Address_Space;

      Initialise_Kernel_Heap;

      Initialise_Kernel_Page_Pool;

      Log_Debug
        ("Initialising boot secondary stack for Hart " & Hart_Id'Image & "...",
         Logging_Tags);

      --  Allocate the boot secondary stack for all harts.
      --  However only the current hart will be mapped here.
      Allocate_Physical_Memory
        (Boot_Secondary_Stack_Size * Maximum_Harts,
         Boot_Secondary_Stack_Phys_Address_Base,
         Result);
      if Is_Error (Result) then
         --  Error already printed.
         Panic;
      end if;

      Boot_Secondary_Stack_Phys_Address : constant Physical_Address_T :=
        Get_Boot_Secondary_Stack_Physical_Address (Hart_Id);

      Boot_Secondary_Stack_Virtual_Address : constant Virtual_Address_T :=
        Get_Boot_Secondary_Stack_Virtual_Address (Hart_Id);

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

      Boot_Secondary_Stack_Top : constant Virtual_Address_T :=
        Boot_Secondary_Stack_Virtual_Address + Boot_Secondary_Stack_Size;

      Log_Debug ("Initialised boot secondary stack.", Logging_Tags);

      --  Once the kernel address space is set up, we can mark this hart as
      --  running. Once all harts are running, the boot memory is no longer
      --  needed, and can be safely freed.
      Hart_States (Hart_Id).Hart_Status := Hart_Status_Running;

      Log_Debug ("Jumping to kernel address space...", Logging_Tags);

      Switch_To_Kernel_Address_Space
        (Get_Kernel_Address_Space_SATP,
         Boot_Secondary_Stack_Top,
         Initialise_Kernel_Services'Address);
   exception
      when Constraint_Error =>
         Panic ("Constraint_Error: Kernel_Main");
   end Kernel_Main;

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
   begin
      Log_Debug ("Initialising kernel services...", Logging_Tags);

      Initialise_Devices;

      Initialise_Hart_Idle_Process (Get_Current_Hart_Id);

      Initialise_Block_Cache;

      Initialise_Filesystem;

      Initialise_Init_Process;

      Start_Non_Boot_Harts;

      Traps.Setup_Next_Timer_Interrupt;
      Log_Debug ("Set initial system tick.", Logging_Tags);

      Log_Debug ("Starting scheduler...", Logging_Tags);

      Processes.Scheduler.Run;
   end Initialise_Kernel_Services;

   procedure Initialise_Graphics is
      Result : Function_Result := Unset;

      Resource_Id        : constant := 1;
      Framebuffer_Width  : constant := 640;
      Framebuffer_Height : constant := 480;
      Framebuffer_Size   : constant :=
        Framebuffer_Width * Framebuffer_Height * 4;

      Framebuffer_Allocation : Memory_Allocation_Result;

      Graphics_Device renames Devices.System_Devices (6);
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
         "/Devices/Disk/Programs/print_fractal_pattern.elf",
         Result);

      Loader.Load_New_Process_From_Filesystem
        (Init_Process.all,
         "/Devices/Disk/Programs/print_random_words.elf",
         Result);

      --  Wait for all the harts to start before freeing the boot memory.
      --  Since their boot stacks are in the boot memory region that would be
      --  freed here.
      Wait_For_All_Harts_To_Start;
      Free_Boot_Memory;

      Exit_Process (Init_Process.all, Result);
      if Is_Error (Result) then
         Panic;
      end if;

      --  Because we exit the process *in* kernel-space, we need to explicitly
      --  run the scheduler here to switch to another process.
      --  We don't call scheduler from the Exit_process procedure because we
      --  want to keep the ability to exit userland processes without
      --  interrupting the normal scheduling.
      Processes.Scheduler.Run;
      Panic ("Init_Process still running after exit.");
   exception
      when Constraint_Error =>
         Panic ("Constraint_Error: Start_Init_Process");
   end Start_Init_Process;

   procedure Wait_For_All_Harts_To_Start is
      --  A timeout is declared here to avoid an infinite loop in case
      --  something goes wrong with hart startup.
      Hart_Spinup_Wait_Timeout : constant Integer := 1000000;
      All_Harts_Started        : Boolean := False;
   begin
      --  Loop continuously, polling all the harts until we can see that
      --  they've all been initialised.
      for I in 1 .. Hart_Spinup_Wait_Timeout loop
         for Hart_Id in 0 .. Maximum_Harts - 1 loop
            All_Harts_Started := True;

            --  All harts should be in either the Running or Invalid state.
            --  Invalid indicates that the hart is not present.
            if not (Hart_States (Hart_Id).Hart_Status = Hart_Status_Running
                    or else
                      Hart_States (Hart_Id).Hart_Status = Hart_Status_Invalid)
            then
               All_Harts_Started := False;
            end if;
         end loop;

         if All_Harts_Started then
            Log_Debug ("All harts have started.", Logging_Tags);
            return;
         end if;
      end loop;

      Log_Debug ("Timeout waiting for all harts to start.", Logging_Tags);
   end Wait_For_All_Harts_To_Start;

   procedure Start_Non_Boot_Harts is
      Non_Boot_Hart_Entry_Marker : constant Integer
      with Import, External_Name => "non_boot_hart_entry";

      SBI_Result : RISCV.SBI.SBI_Result_T;
   begin
      Log_Debug ("Starting non-boot harts...", Logging_Tags);

      for Hart_Id in 0 .. Hart_State.Maximum_Harts - 1 loop
         if Hart_Id /= Get_Current_Hart_Id then
            Hart_States (Hart_Id).Hart_Status := Hart_Status_Unknown;

            SBI_Result := RISCV.SBI.Get_Hart_Status (Unsigned_32 (Hart_Id));

            --  SBI_ERR_INVALID_PARAM indicates that the hart is not present.
            --  For more documentation, refer to:
            --  https://github.com/
            --    riscv-non-isa/riscv-sbi-doc/blob/master/src/ext-hsm.adoc
            if SBI_Result.Error /= 0 then
               if SBI_Result.Error = RISCV.SBI.SBI_ERR_INVALID_PARAM then
                  Hart_States (Hart_Id).Hart_Status := Hart_Status_Invalid;

                  Log_Debug
                    ("Hart" & Hart_Id'Image & " is not present.",
                     Logging_Tags);
               else
                  Log_Error
                    ("Error getting hart"
                     & Hart_Id'Image
                     & " status: "
                     & SBI_Result.Error'Image);
               end if;
            else
               Log_Debug
                 ("Hart"
                  & Hart_Id'Image
                  & " status: "
                  & SBI_Result.Value'Image,
                  Logging_Tags);

               SBI_Result :=
                 RISCV.SBI.Hart_Start
                   (Unsigned_32 (Hart_Id),
                    Address_To_Unsigned_64
                      (Non_Boot_Hart_Entry_Marker'Address),
                    Get_Kernel_Address_Space_SATP);
               if SBI_Result.Error /= RISCV.SBI.SBI_SUCCESS then
                  Panic
                    ("Error starting non-boot hart"
                     & Hart_Id'Image
                     & ": "
                     & SBI_Result.Error'Image);
               end if;
            end if;
         end if;
      end loop;
   exception
      when Constraint_Error =>
         Panic ("Constraint_Error: Start_Non_Boot_Harts");
   end Start_Non_Boot_Harts;

   procedure Non_Boot_Hart_Entry (Hart_Id : Hart_Index_T) is
      Result : Function_Result := Unset;
   begin
      --  This needs to be the first function called on each hart to set up
      --  the hart's state structure. This needs be called before any logging
      --  can take place, since the spinlock mechanism requires being able to
      --  determine the current hart id.
      Initialise_Hart (Hart_Id);

      Log_Debug ("Starting non-boot hart: " & Hart_Id'Image, Logging_Tags);

      --  Map the boot secondary stack for this hart.
      Boot_Secondary_Stack_Phys_Address : constant Physical_Address_T :=
        Get_Boot_Secondary_Stack_Physical_Address (Hart_Id);

      Boot_Secondary_Stack_Virtual_Address : constant Virtual_Address_T :=
        Get_Boot_Secondary_Stack_Virtual_Address (Hart_Id);

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

      Boot_Secondary_Stack_Top : constant Virtual_Address_T :=
        Boot_Secondary_Stack_Virtual_Address + Boot_Secondary_Stack_Size;

      Log_Debug ("Parking non-boot hart: " & Hart_Id'Image, Logging_Tags);

      Hart_States (Hart_Id).Hart_Status := Hart_Status_Running;

      --  Enter the kernel address space properly, and park this hart.
      Switch_To_Kernel_Address_Space
        (Get_Kernel_Address_Space_SATP,
         Boot_Secondary_Stack_Top,
         Park_Non_Boot_Hart'Address);
   exception
      when Constraint_Error =>
         Panic ("Constraint_Error: Non_Boot_Hart_Entry");
   end Non_Boot_Hart_Entry;

   --  An issue currently exists here with phantom Constraint_Error warnings.
   --  To work around this, we disable warnings for these functions.
   pragma Warnings (Off, "Constraint_Error");
   function Get_Boot_Secondary_Stack_Virtual_Address
     (Hart_Id : Integer) return Virtual_Address_T is
   begin
      return
        Boot_Secondary_Stack_Virtual_Address_Base
        + Storage_Offset (Hart_Id * Boot_Secondary_Stack_Size);
   end Get_Boot_Secondary_Stack_Virtual_Address;
   pragma Warnings (On);

   pragma Warnings (Off, "Constraint_Error");
   function Get_Boot_Secondary_Stack_Physical_Address
     (Hart_Id : Integer) return Physical_Address_T is
   begin
      return
        Boot_Secondary_Stack_Phys_Address_Base
        + Storage_Offset (Hart_Id * Boot_Secondary_Stack_Size);
   end Get_Boot_Secondary_Stack_Physical_Address;
   pragma Warnings (On);
end Boot;
