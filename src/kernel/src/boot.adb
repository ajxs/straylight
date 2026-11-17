-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with GNAT.Debug_Utilities;
with System.Storage_Elements; use System.Storage_Elements;

with Boot.Device_Discovery;           use Boot.Device_Discovery;
with Devices;                         use Devices;
with Devices.Attached_System_Devices; use Devices.Attached_System_Devices;
with Devices.Virtio;
with Devices.Virtio.Block;
with Devices.Virtio.Graphics;
with Devicetree;
with Filesystems;                     use Filesystems;
with Filesystems.FAT;
with Filesystems.Root;                use Filesystems.Root;
with Filesystems.UStar;
with Function_Results;                use Function_Results;
with Graphics;                        use Graphics;
with Loader;
with Logging;                         use Logging;
with Memory.Kernel;                   use Memory.Kernel;
with Memory.Physical;                 use Memory.Physical;
with Memory.Virtual;                  use Memory.Virtual;
with RISCV.SBI;
with Processes.Scheduler;
with Traps;

package body Boot is
   function Get_Early_Kernel_Stack_Virtual_Address
     (Hart_Id : Hart_Index_T) return Virtual_Address_T
   is (Early_Kernel_Stack_Virtual_Address_Base
       + Storage_Offset (Hart_Id) * Early_Kernel_Stack_Size)
   with Inline, Pure_Function;

   function Get_Early_Kernel_Stack_Physical_Address
     (Hart_Id : Hart_Index_T) return Physical_Address_T
   is (Early_Kernel_Stack_Phys_Address_Base
       + Storage_Offset (Hart_Id) * Early_Kernel_Stack_Size)
   with Inline, Pure_Function;

   function Get_Hart_Emergency_Stack_Top_Address
     (Hart_Id : Hart_Index_T) return Virtual_Address_T
   is
      Emergency_Stacks_Base : constant Integer
      with
        Import,
        Alignment     => 4096,
        External_Name => "__hart_emergency_stacks_base";

      Emergency_Stacks_Top : constant Integer
      with
        Import,
        Alignment     => 4096,
        External_Name => "__hart_emergency_stacks_top";

   begin
      Stack_Size : constant Storage_Count :=
        (Emergency_Stacks_Top'Address - Emergency_Stacks_Base'Address)
        / Maximum_Harts;

      return
        (Emergency_Stacks_Base'Address + Storage_Offset (Hart_Id) * Stack_Size)
        + Stack_Size;
   exception
      when Constraint_Error =>
         Panic_Constraint_Error;
   end Get_Hart_Emergency_Stack_Top_Address;

   procedure Initialise_Hart_Early_Kernel_Stack
     (Hart_Id : Hart_Index_T; Early_Kernel_Stack_Top : out Virtual_Address_T)
   is
      Result : Function_Result := Unset;
   begin
      Early_Kernel_Stack_Phys_Address : constant Physical_Address_T :=
        Get_Early_Kernel_Stack_Physical_Address (Hart_Id);

      Early_Kernel_Stack_Virtual_Address : constant Virtual_Address_T :=
        Get_Early_Kernel_Stack_Virtual_Address (Hart_Id);

      Map_Kernel_Memory
        (Early_Kernel_Stack_Virtual_Address,
         Early_Kernel_Stack_Phys_Address,
         Early_Kernel_Stack_Size,
         (True, True, False, False),
         Result);
      if Is_Error (Result) then
         --  Error already printed.
         Panic;
      end if;

      Early_Kernel_Stack_Top :=
        Early_Kernel_Stack_Virtual_Address + Early_Kernel_Stack_Size;
   end Initialise_Hart_Early_Kernel_Stack;

   procedure Free_Boot_Memory is
      --  The physical address range of the boot section.
      Boot_Start : constant Natural
      with Import, External_Name => "__boot_start_physical";

      Boot_End : constant Natural
      with Import, External_Name => "__boot_end_physical";

      Result : Function_Result := Unset;
   begin
      pragma Debug (Debug_Boot, Log_Debug ("Freeing boot memory..."));

      Free_Physical_Memory_Length : constant Storage_Offset :=
        Boot_End'Address - Boot_Start'Address;

      if Free_Physical_Memory_Length not in Storage_Count'Range then
         Panic ("Invalid free physical memory length");
      end if;

      Create_Free_Physical_Memory_Region
        (Physical_Address_T (Boot_Start'Address),
         Free_Physical_Memory_Length,
         Result);
      if Is_Error (Result) then
         --  Error already printed.
         Panic;
      end if;

      pragma Debug (Debug_Boot, Log_Debug ("Freed boot memory."));
   exception
      when Constraint_Error =>
         Panic_Constraint_Error;
   end Free_Boot_Memory;

   procedure Initialise_Filesystem is
      Result : Function_Result := Unset;

      Root_Filesystem_Node_Index    : Filesystem_Node_Index_T := 0;
      Devices_Filesystem_Node_Index : Filesystem_Node_Index_T := 0;
      Disk_Filesystem_Node_Index    : Filesystem_Node_Index_T := 0;
      Disk_B_Filesystem_Node_Index  : Filesystem_Node_Index_T := 0;
      UART_Filesystem_Node_Index    : Filesystem_Node_Index_T := 0;
      Console_Filesystem_Node_Index : Filesystem_Node_Index_T := 0;
   begin
      pragma Debug (Debug_Boot, Log_Debug ("Initialising filesystem..."));

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
         Device                       => Root_Filesystem_Memory_Device);

      Filesystems.System_Root_Filesystem :=
        Filesystems.Mounted_Filesystems (1)'Access;

      Initialise_Root_Filesystem (Filesystems.System_Root_Filesystem, Result);

      --  The devices of the two disk filesystems are set by Mount_Disks.
      Filesystems.Mounted_Filesystems (2) :=
        (Filesystem_Type => Filesystem_Type_FAT, others => <>);

      Filesystems.Mounted_Filesystems (3) :=
        (Filesystem_Type => Filesystem_Type_UStar, others => <>);

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
         Mounted_Filesystem => Filesystems.Mounted_Filesystems (2)'Access);
      if Is_Error (Result) then
         Panic;
      end if;

      Add_Filesystem_Node_To_Root_Filesystem
        (Filesystems.System_Root_Filesystem,
         "Disk_B",
         Devices_Filesystem_Node_Index,
         Disk_B_Filesystem_Node_Index,
         Result,
         Filesystem_Node_Type_Mounted_Filesystem,
         Mounted_Filesystem => Filesystems.Mounted_Filesystems (3)'Access);
      if Is_Error (Result) then
         Panic;
      end if;

      Add_Filesystem_Node_To_Root_Filesystem
        (Filesystems.System_Root_Filesystem,
         "Serial",
         Devices_Filesystem_Node_Index,
         UART_Filesystem_Node_Index,
         Result,
         Filesystem_Node_Type_Device,
         Mounted_Device => Console_Device.all.Console.Backend);
      if Is_Error (Result) then
         Panic;
      end if;

      Add_Filesystem_Node_To_Root_Filesystem
        (Filesystems.System_Root_Filesystem,
         "Console",
         Devices_Filesystem_Node_Index,
         Console_Filesystem_Node_Index,
         Result,
         Filesystem_Node_Type_Device,
         Mounted_Device => Console_Device);
      if Is_Error (Result) then
         Panic;
      end if;

      pragma Debug (Debug_Boot, Log_Debug ("Initialised filesystem."));
   exception
      when Constraint_Error =>
         Panic_Constraint_Error;
   end Initialise_Filesystem;

   procedure Mount_Disks is
      --  The device writes the sector to a buffer by its physical address.
      --  The smallest physical allocation is a page.
      Sector_Buffer_Size : constant := 16#1000#;

      Sector_Buffer_Physical_Address : Physical_Address_T :=
        Null_Physical_Address;

      FAT_Filesystem   : Filesystem_T renames
        Filesystems.Mounted_Filesystems (2);
      UStar_Filesystem : Filesystem_T renames
        Filesystems.Mounted_Filesystems (3);

      Result : Function_Result := Unset;
   begin
      Allocate_Physical_Memory
        (Sector_Buffer_Size, Sector_Buffer_Physical_Address, Result);
      if Is_Error (Result) then
         Panic ("Error allocating the disk sector buffer");
      end if;

      Sector_Buffer_Address : constant Virtual_Address_T :=
        Get_Physical_Address_Virtual_Mapping (Sector_Buffer_Physical_Address);

      for Device of
        System_Devices (System_Devices'First .. Attached_System_Device_Count)
        when Device.Driver_Type = Driver_Type_Virtio_Block
      loop
         Devices.Virtio.Block.Read_Sector
           (Init_Process.all,
            Device.Common,
            Device.Virtio_Block,
            Sector_Buffer_Physical_Address,
            0,
            Result);
         if Is_Error (Result) then
            Panic ("Error reading the first sector of a disk");
         end if;

         if Filesystems.FAT.Is_FAT_Boot_Sector (Sector_Buffer_Address) then
            FAT_Filesystem.Device := Device'Access;
         elsif Filesystems.UStar.Is_UStar_Header (Sector_Buffer_Address) then
            UStar_Filesystem.Device := Device'Access;
         end if;
      end loop;

      Free_Physical_Memory (Sector_Buffer_Physical_Address, Result);
      if Is_Error (Result) then
         Panic ("Error freeing the disk sector buffer");
      end if;

      if FAT_Filesystem.Device = null then
         Panic ("No FAT disk found");
      end if;

      --  Only test programs use the UStar disk, so the kernel can run
      --  without it. In the case that it's not present, a read operation from
      --  its mount point will return an error.
      if UStar_Filesystem.Device = null then
         Log_Error ("No UStar disk found");
      end if;
   exception
      when Constraint_Error =>
         Panic_Constraint_Error;
   end Mount_Disks;

   procedure Initialise_Hart (Hart_Id : Hart_Index_T) is
      procedure Save_Hart_State_Pointer (Hart_State_Address : Address)
      with
        Import,
        Convention    => Assembler,
        External_Name => "store_hart_state_pointer";
   begin
      Hart_States (Hart_Id) :=
        (Hart_Id                                    => Hart_Id,
         Interrupts_Enabled_Before_Initial_Push_Off => False,
         Hart_Status                                => Hart_Status_Initialised,
         Emergency_Stack_Top                        =>
           Get_Hart_Emergency_Stack_Top_Address (Hart_Id),
         others                                     => <>);

      Save_Hart_State_Pointer (Hart_States (Hart_Id)'Address);
   exception
      when Constraint_Error =>
         Panic_Constraint_Error;
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
      --  procedure, we jump to the kernel-level init process.
      Init_Process.all.Kernel_Context (ra) :=
        Address_To_Unsigned_64 (Start_Init_Process'Address);

      Add_Process_To_Process_Queue (Init_Process, Result);
      if Is_Error (Result) then
         Panic ("Error adding init process");
      end if;
   exception
      when Constraint_Error =>
         Panic_Constraint_Error;
   end Initialise_Init_Process;

   procedure Kernel_Main
     (Hart_Id : Hart_Index_T; DTB_Address : Physical_Address_T)
   is
      Result : Function_Result := Unset;

      Early_Kernel_Stack_Top : Virtual_Address_T := Null_Address;

      --  Parse_Devicetree fails with Region_Array_Exhausted if the
      --  Devicetree describes more usable memory regions than fit here.
      Maximum_Memory_Regions : constant := 16;

      Memory_Regions      :
        Physical_Memory_Region_Array_T (1 .. Maximum_Memory_Regions);
      Memory_Region_Count : Natural := 0;

      Timebase_Frequency : Unsigned_64 := 0;

      --  The kernel image's physical address range. It starts with the boot
      --  section, which Free_Boot_Memory frees once it's no longer needed.
      Kernel_Start : constant Natural
      with Import, External_Name => "__boot_start_physical";

      Kernel_End : constant Natural
      with Import, External_Name => "__kernel_end_physical";
   begin
      --  This needs to be the first function called on each hart to set up
      --  the hart's state structure. This needs be called before any logging
      --  can take place, since the spinlock mechanism requires being able to
      --  determine the current hart id.
      Initialise_Hart (Hart_Id);

      pragma
        Debug (Debug_Boot, Log_Debug ("Booting Hart" & Hart_Id'Image & "..."));

      --  The Devicetree blob was mapped into the higher-half address space
      --  at this offset in Boot.Early.Map_Devicetree.
      Devicetree_Higher_Half_Address : constant Address :=
        Address (DTB_Address) + Higher_Half_Offset;

      Devicetree.Parse_Devicetree
        (Devicetree_Higher_Half_Address,
         Memory_Regions,
         Memory_Region_Count,
         Devicetree_Devices,
         Devicetree_Device_Count,
         Devicetree_Harts,
         Devicetree_Hart_Count,
         Timebase_Frequency,
         Result);
      if Is_Error (Result) then
         --  Error already printed.
         Panic;
      end if;

      --  A frequency below the tick rate would give a tick interval of 0.
      if Timebase_Frequency < Traps.System_Tick_Rate then
         Panic ("No valid timebase-frequency in the Devicetree");
      end if;

      Traps.System_Tick_Interval :=
        Timebase_Frequency / Traps.System_Tick_Rate;

      --  The kernel image isn't free memory, so it's removed from the regions
      --  that Parse_Devicetree returns.
      Kernel_Image_Region : constant Physical_Memory_Region_T :=
        (Base => Physical_Address_T (Kernel_Start'Address),
         Size => Kernel_End'Address - Kernel_Start'Address);

      Remove_Physical_Memory_Region_From_Array
        (Memory_Regions, Memory_Region_Count, Kernel_Image_Region, Result);
      if Is_Error (Result) then
         Panic ("Error removing the kernel image from the memory regions");
      end if;

      for Device of Devicetree_Devices (1 .. Devicetree_Device_Count) loop
         pragma
           Debug
             (Debug_Boot,
              Log_Debug
                ("Devicetree device: device_type="
                 & Device.Device_Type'Image
                     (2 .. Device.Device_Type'Image'Length)
                 & " base="
                 & GNAT.Debug_Utilities.Image_C (Address (Device.Base))
                 & " size="
                 & Device.Size'Image (2 .. Device.Size'Image'Length)
                 & " interrupt_line="
                 & Device.Interrupt_Line'Image
                     (2 .. Device.Interrupt_Line'Image'Length)));
      end loop;

      pragma
        Debug
          (Debug_Boot, Log_Debug ("Initialising core kernel subsystems..."));

      Initialise_Physical_Memory_Manager
        (Memory_Regions (1 .. Memory_Region_Count));

      Initialise_Kernel_Address_Space;

      --  Allocate the early kernel stack for all harts.
      --  However only the current hart will be mapped here.
      Allocate_Physical_Memory
        (Early_Kernel_Stack_Size * Maximum_Harts,
         Early_Kernel_Stack_Phys_Address_Base,
         Result);
      if Is_Error (Result) then
         --  Error already printed.
         Panic;
      end if;

      Initialise_Hart_Early_Kernel_Stack (Hart_Id, Early_Kernel_Stack_Top);

      pragma Debug (Debug_Boot, Log_Debug ("Initialised early kernel stack."));

      --  Once the kernel address space is set up, we can mark this hart as
      --  running. Once all harts are running, the boot memory is no longer
      --  needed, and can be safely freed.
      Hart_States (Hart_Id).Hart_Status := Hart_Status_Running;

      pragma
        Debug (Debug_Boot, Log_Debug ("Jumping to kernel address space..."));

      Switch_To_Kernel_Address_Space
        (Hart_Id,
         Get_Kernel_Address_Space_SATP,
         Early_Kernel_Stack_Top,
         Early_Kernel_Stack_Size,
         Initialise_Kernel_Services'Address);
   exception
      when Constraint_Error =>
         Panic_Constraint_Error;
   end Kernel_Main;

   procedure Initialise_Physical_Memory_Manager
     (Memory_Regions : Physical_Memory_Region_Array_T)
   is
      --  Memory above the physical memory map's limit can't be reached
      --  through the map, so it isn't used.
      Map_Limit : constant Physical_Address_T :=
        Physical_Address_T (To_Address (Physical_Memory_Map_Limit));

      Result : Function_Result := Unset;
   begin
      pragma Debug (Debug_Boot, Log_Debug ("Initialising PMM..."));

      --  Parse_Devicetree has already removed the reserved regions and the
      --  kernel image from these regions.
      if Memory_Regions'Length = 0 then
         Panic ("No memory regions available");
      end if;

      --  The Devicetree blob isn't used after this point, so it's alright for
      --  the memory it sits in to be allocated as free memory.
      for Region of Memory_Regions loop
         Map_Physical_Memory_Region : declare
            Region_End : constant Physical_Address_T :=
              Physical_Address_T (Address (Region.Base) + Region.Size);

            --  Align the region to page boundaries, and clamp it to the
            --  physical memory map's limit. Clamping before rounding means
            --  rounding can't wrap around.
            Aligned_Start : constant Physical_Address_T :=
              Align_Up_To_Page_Boundary
                ((if Region.Base < Map_Limit then Region.Base else Map_Limit));

            Aligned_End : constant Physical_Address_T :=
              Align_Down_To_Page_Boundary
                ((if Region_End < Map_Limit then Region_End else Map_Limit));
         begin
            if Aligned_End > Aligned_Start then
               pragma
                 Debug
                   (Debug_Boot,
                    Log_Debug
                      ("Free physical memory: start="
                       & GNAT.Debug_Utilities.Image_C (Address (Aligned_Start))
                       & " end="
                       & GNAT.Debug_Utilities.Image_C
                           (Address (Aligned_End))));

               Create_Free_Physical_Memory_Region
                 (Aligned_Start,
                  Address (Aligned_End) - Address (Aligned_Start),
                  Result);
               if Is_Error (Result) then
                  Panic ("Error creating free physical memory region");
               end if;
            end if;
         end Map_Physical_Memory_Region;
      end loop;

      pragma Debug (Debug_Boot, Log_Debug ("Initialised PMM."));
   exception
      when Constraint_Error =>
         Panic_Constraint_Error;
   end Initialise_Physical_Memory_Manager;

   procedure Initialise_Kernel_Services (Hart_Id : Hart_Index_T) is
   begin
      pragma Debug (Debug_Boot, Log_Debug ("Initialising kernel services..."));

      Initialise_Devices;

      Initialise_Hart_Idle_Process (Hart_Id);

      Initialise_Block_Cache;

      Initialise_Filesystem;

      --  Certain kernel services, such as device IO, require a process context
      --  to operate in, so we create the init process here.
      Initialise_Init_Process;

      Start_Non_Boot_Harts;

      Traps.Setup_Next_Timer_Interrupt;

      pragma Debug (Debug_Boot, Log_Debug ("Set initial system tick."));

      --  The scheduler will jump to the init process.
      Processes.Scheduler.Run;
   end Initialise_Kernel_Services;

   procedure Initialise_Graphics is
      Result : Function_Result := Unset;

      Resource_Id        : constant := 1;
      Framebuffer_Width  : constant := 640;
      Framebuffer_Height : constant := 480;
      Framebuffer_Size   : constant :=
        Framebuffer_Width * Framebuffer_Height * 4;

      Framebuffer_Allocation : Virtual_Physical_Address_Pair_T;
   begin
      --  In the case that the system has no graphics device attached, any
      --  attempt by a userspace program to send a framebuffer to the active
      --  graphics device will return an error.
      if Graphics_Device = null then
         pragma Debug (Debug_Boot, Log_Debug ("No graphics device found."));
         return;
      end if;

      Devices.Virtio.Graphics.Get_Display_Info
        (Init_Process.all,
         Graphics_Device.all.Common,
         Graphics_Device.all.Virtio_GPU,
         Result);
      if Is_Error (Result) then
         Panic;
      end if;

      pragma
        Debug (Debug_Boot, Log_Debug ("Allocating framebuffer memory..."));

      Allocate_Kernel_Physical_Memory
        (Framebuffer_Size, Framebuffer_Allocation, Result);
      if Is_Error (Result) then
         Panic;
      end if;

      pragma
        Debug
          (Debug_Boot,
           Log_Debug
             ("Allocated framebuffer: "
              & "addr_virt="
              & GNAT.Debug_Utilities.Image_C
                  (Framebuffer_Allocation.Virtual_Address)
              & " addr_phys="
              & GNAT.Debug_Utilities.Image_C
                  (Address (Framebuffer_Allocation.Physical_Address))));

      Graphics_Device.all.Virtio_GPU.Resource_Id := Resource_Id;
      Graphics_Device.all.Virtio_GPU.Framebuffer_Addresses :=
        Framebuffer_Allocation;
      Graphics_Device.all.Virtio_GPU.Framebuffer_Width := Framebuffer_Width;
      Graphics_Device.all.Virtio_GPU.Framebuffer_Height := Framebuffer_Height;

      Devices.Virtio.Graphics.Created_2d_Resource
        (Init_Process.all,
         Graphics_Device.all.Common,
         Graphics_Device.all.Virtio_GPU,
         Resource_Id,
         Result);
      if Is_Error (Result) then
         Panic;
      end if;

      Devices.Virtio.Graphics.Attach_Framebuffer_To_Resource
        (Init_Process.all,
         Graphics_Device.all.Common,
         Graphics_Device.all.Virtio_GPU,
         Resource_Id,
         Framebuffer_Allocation.Physical_Address,
         Result);
      if Is_Error (Result) then
         Panic;
      end if;

      Devices.Virtio.Graphics.Set_Scanout
        (Init_Process.all,
         Graphics_Device.all.Common,
         Graphics_Device.all.Virtio_GPU,
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

      Devices.Virtio.Graphics.Transfer_To_Host_2d
        (Init_Process.all,
         Graphics_Device.all.Common,
         Graphics_Device.all.Virtio_GPU,
         Resource_Id,
         0,
         0,
         Framebuffer_Width,
         Framebuffer_Height,
         Result);
      if Is_Error (Result) then
         Panic;
      end if;

      Devices.Virtio.Graphics.Resource_Flush
        (Init_Process.all,
         Graphics_Device.all.Common,
         Graphics_Device.all.Virtio_GPU,
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
         Panic_Constraint_Error;
   end Initialise_Graphics;

   procedure Start_Init_Process is
      Result : Function_Result := Unset;
   begin
      --  Note: There's no need to release the lock of any previous process
      --  when starting the init process, as it's impossible by design for any
      --  other process to be running at this point in time.
      pragma Debug (Debug_Boot, Log_Debug ("Starting init process..."));

      --  The graphics system is initialised here because the Virtio driver
      --  requires a process context to operate in.
      Initialise_Graphics;

      --  This must run before any program is loaded from either disk.
      Mount_Disks;

      Loader.Load_New_Process_From_Filesystem
        (Init_Process.all,
         "/Devices/Disk/Programs/print_fractal_pattern.elf",
         Result);
      if Is_Error (Result) then
         Panic;
      end if;

      Loader.Load_New_Process_From_Filesystem
        (Init_Process.all,
         "/Devices/Disk/Programs/print_fractal_pattern_2.elf",
         Result);
      if Is_Error (Result) then
         Panic;
      end if;

      --  Loader.Load_New_Process_From_Filesystem
      --    (Init_Process.all, "/Devices/Disk/Programs/test_file_io.elf",
      --  Result);

      --  Loader.Load_New_Process_From_Filesystem
      --    (Init_Process.all,
      --     "/Devices/Disk/Programs/print_random_words.elf",
      --     Result);

      Loader.Load_New_Process_From_Filesystem
        (Init_Process.all, "/Devices/Disk/Programs/simple_shell.elf", Result);
      if Is_Error (Result) then
         Panic;
      end if;

      --  Wait for all the harts to start before freeing the boot memory.
      --  Since their boot stacks are in the boot memory region that would be
      --  freed here.
      Wait_For_All_Harts_To_Start;
      Free_Boot_Memory;

      --  Exit the init process via the scheduler.
      Processes.Scheduler.Run (Process_Stopped);
      Panic ("Init_Process still running after exit.");
   exception
      when Constraint_Error =>
         Panic_Constraint_Error;
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
         All_Harts_Started := True;

         for Hart_Id in 0 .. Maximum_Harts - 1 loop

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
            pragma
              Debug
                (Debug_Boot, Log_Debug ("All harts started successfully."));
            return;
         end if;
      end loop;

      Panic ("Timeout waiting for all harts to start.");
   end Wait_For_All_Harts_To_Start;

   procedure Start_Non_Boot_Harts is
      Non_Boot_Hart_Entry_Marker : constant Integer
      with Import, External_Name => "non_boot_hart_entry";

      SBI_Result : RISCV.SBI.SBI_Result_T;
   begin
      pragma Debug (Debug_Boot, Log_Debug ("Starting non-boot harts..."));

      for Hart of Devicetree_Harts (1 .. Devicetree_Hart_Count) loop
         if Hart.Hart_Id > Unsigned_64 (Hart_Index_T'Last) then
            Log_Error
              ("Not starting a hart above the kernel's maximum: hart_id="
               & Hart.Hart_Id'Image (2 .. Hart.Hart_Id'Image'Length));
         end if;
      end loop;

      for Hart_Id in 0 .. Hart_State.Maximum_Harts - 1 loop
         Was_Hart_Described_As_Available_in_Devicetree : constant Boolean :=
           (for some Hart of Devicetree_Harts (1 .. Devicetree_Hart_Count) =>
              Hart.Hart_Id = Unsigned_64 (Hart_Id));

         if Hart_Id = Get_Current_Hart_Id then
            --  This hart is already running.
            null;
         elsif not Was_Hart_Described_As_Available_in_Devicetree then
            --  A hart the Devicetree doesn't list as available is never
            --  started, such as a disabled machine-mode-only monitor core.
            Hart_States (Hart_Id).Hart_Status := Hart_Status_Invalid;
         else
            Hart_States (Hart_Id).Hart_Status := Hart_Status_Unknown;

            SBI_Result := RISCV.SBI.Get_Hart_Status (Unsigned_32 (Hart_Id));

            --  SBI_ERR_INVALID_PARAM indicates that the hart is not present.
            --  For more documentation, refer to:
            --  https://github.com/
            --    riscv-non-isa/riscv-sbi-doc/blob/master/src/ext-hsm.adoc
            if SBI_Result.Error /= 0 then
               if SBI_Result.Error = RISCV.SBI.SBI_ERR_INVALID_PARAM then
                  Hart_States (Hart_Id).Hart_Status := Hart_Status_Invalid;

                  pragma
                    Debug
                      (Debug_Boot,
                       Log_Debug
                         ("Hart" & Hart_Id'Image & " is not present."));
               else
                  Log_Error
                    ("Error getting hart"
                     & Hart_Id'Image
                     & " status: "
                     & SBI_Result.Error'Image);
               end if;
            else
               pragma
                 Debug
                   (Debug_Boot,
                    Log_Debug
                      ("Hart"
                       & Hart_Id'Image
                       & " status: "
                       & SBI_Result.Value'Image));

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
         Panic_Constraint_Error;
   end Start_Non_Boot_Harts;

   procedure Non_Boot_Hart_Entry (Hart_Id : Hart_Index_T) is
      Early_Kernel_Stack_Top : Virtual_Address_T := Null_Address;

      procedure Log_Hart is
      begin
         Log_Debug ("Starting non-boot Hart#" & Hart_Id'Image);
      exception
         when Constraint_Error =>
            null;
      end Log_Hart;
   begin
      --  This needs to be the first function called on each hart to set up
      --  the hart's state structure. This needs be called before any logging
      --  can take place, since the spinlock mechanism requires being able to
      --  determine the current hart id.
      Initialise_Hart (Hart_Id);

      pragma Debug (Debug_Boot, Log_Hart);

      Initialise_Hart_Early_Kernel_Stack (Hart_Id, Early_Kernel_Stack_Top);

      --  Enter the kernel address space.
      Switch_To_Kernel_Address_Space
        (Hart_Id,
         Get_Kernel_Address_Space_SATP,
         Early_Kernel_Stack_Top,
         Early_Kernel_Stack_Size,
         Non_Boot_Hart_Start'Address);
   end Non_Boot_Hart_Entry;

   procedure Non_Boot_Hart_Start (Hart_Id : Hart_Index_T) is
   begin
      pragma
        Debug
          (Debug_Boot, Log_Debug ("Started non-boot hart#" & Hart_Id'Image));

      Initialise_Hart_Idle_Process (Hart_Id);

      Hart_States (Hart_Id).Hart_Status := Hart_Status_Running;

      Processes.Scheduler.Run;
   exception
      when Constraint_Error =>
         Panic_Constraint_Error;
   end Non_Boot_Hart_Start;
end Boot;
