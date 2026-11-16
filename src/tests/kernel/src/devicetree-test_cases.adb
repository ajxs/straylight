-------------------------------------------------------------------------------
--  Copyright (c) 2026, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Ada.Command_Line;
with Ada.Directories;
with Ada.Streams;      use Ada.Streams;
with Ada.Streams.Stream_IO;
with AUnit.Assertions; use AUnit.Assertions;

package body Devicetree.Test_Cases is
   --  Large enough for the captured DTB, which is about 11 KiB.
   Maximum_DTB_Size : constant := 16#1_0000#;

   subtype DTB_Buffer_T is Stream_Element_Array (1 .. Maximum_DTB_Size);

   ----------------------------------------------------------------------------
   --  Reads a DTB from the tests' data directory. It's found relative to the
   --  test executable, at bin/../data/, so the tests can be run from any
   --  directory.
   --  virt.dtb is the DTB that the kernel receives on QEMU's 'virt' machine,
   --  which scripts/capture_virt_dtb.sh captures. The others are built from
   --  the .dts file with the same name.
   ----------------------------------------------------------------------------
   procedure Read_DTB (File_Name : String; DTB : out DTB_Buffer_T) is
      package Directories renames Ada.Directories;

      Tests_Directory : constant String :=
        Directories.Containing_Directory
          (Directories.Containing_Directory
             (Directories.Full_Name (Ada.Command_Line.Command_Name)));

      DTB_Path : constant String :=
        Directories.Compose
          (Directories.Compose (Tests_Directory, "data"), File_Name);

      File : Stream_IO.File_Type;
      Last : Stream_Element_Offset := 0;
   begin
      Stream_IO.Open (File, Stream_IO.In_File, DTB_Path);
      Stream_IO.Read (File, DTB, Last);
      Stream_IO.Close (File);

      Assert
        (Last > 0 and then Last < DTB'Last,
         File_Name & " was read, and fits in the buffer");
   end Read_DTB;

   function Create_Phys_Addr
     (Value : Integer_Address) return Physical_Address_T
   is (Physical_Address_T (To_Address (Value)));

   overriding
   procedure Run_Test (T : in out Test_Parse_Virt_Devicetree) is
      pragma Unreferenced (T);

      Memory_Regions      : Physical_Memory_Region_Array_T (1 .. 16);
      Memory_Region_Count : Natural := 0;
      Devices             : Devicetree_Device_Array_T (1 .. 16);
      Device_Count        : Natural := 0;
      Harts               : Devicetree_Hart_Array_T (1 .. 16);
      Hart_Count          : Natural := 0;
      Timebase_Frequency  : Unsigned_64 := 0;
      Result              : Function_Result := Unset;

      DTB : DTB_Buffer_T
      with Alignment => 8;

      --  The PLIC's phandle, which every other device's 'interrupt-parent'
      --  refers to.
      PLIC_Phandle : constant := 7;

      procedure Assert_Device
        (Index            : Positive;
         Device_Type      : Devicetree_Device_Type_T;
         Base             : Integer_Address;
         Size             : Storage_Count;
         Interrupt_Line   : Unsigned_32;
         Phandle          : Unsigned_32;
         Interrupt_Parent : Unsigned_32) is
      begin
         Assert
           (Devices (Index)
            = (Device_Type      => Device_Type,
               Base             => Create_Phys_Addr (Base),
               Size             => Size,
               Interrupt_Line   => Interrupt_Line,
               Phandle          => Phandle,
               Interrupt_Parent => Interrupt_Parent),
            "Device" & Index'Image & " is wrong");
      end Assert_Device;
   begin
      Read_DTB ("virt.dtb", DTB);

      Parse_Devicetree
        (DTB'Address,
         Memory_Regions,
         Memory_Region_Count,
         Devices,
         Device_Count,
         Harts,
         Hart_Count,
         Timebase_Frequency,
         Result);
      Assert (Result = Success, "Parse_Devicetree succeeds");

      Assert
        (Timebase_Frequency = 10_000_000,
         "The timebase is QEMU's 10 MHz, from /cpus");

      --  Each hart has a machine-mode PLIC context, then a supervisor-mode
      --  one, so hart N's supervisor-mode context is N * 2 + 1.
      Assert (Hart_Count = 3, "There are three harts");
      for Hart in 0 .. 2 loop
         Assert
           (Harts (Hart + 1)
            = (Hart_Id                     => Unsigned_64 (Hart),
               Has_Supervisor_PLIC_Context => True,
               Supervisor_PLIC_Context     => Hart * 2 + 1),
            "Hart" & Hart'Image & " is wrong");
      end loop;

      --  The memory node is 0x80000000 to 0x88000000, and OpenSBI's two
      --  'reserved-memory' children cover 0x80000000 to 0x80060000. The
      --  parser doesn't know about the kernel image, so the rest is one
      --  region.
      Assert (Memory_Region_Count = 1, "There is one memory region");
      Assert
        (Memory_Regions (1)
         = (Base => Create_Phys_Addr (16#8006_0000#), Size => 16#7FA_0000#),
         "The region runs from OpenSBI's memory to the end of memory");

      --  Devices are in Devicetree order: the UART, then the eight Virtio
      --  slots from the highest address down, then the PLIC.
      Assert (Device_Count = 10, "There are ten recognised devices");
      Assert_Device
        (1,
         Devicetree_Device_Type_UART_NS16550A,
         16#1000_0000#,
         16#100#,
         10,
         0,
         PLIC_Phandle);

      for Slot in 1 .. 8 loop
         Assert_Device
           (1 + Slot,
            Devicetree_Device_Type_Virtio_MMIO,
            16#1000_0000# + Integer_Address (9 - Slot) * 16#1000#,
            16#1000#,
            Unsigned_32 (9 - Slot),
            0,
            PLIC_Phandle);
      end loop;

      Assert_Device
        (10,
         Devicetree_Device_Type_PLIC,
         16#0C00_0000#,
         16#60_0000#,
         0,
         PLIC_Phandle,
         0);
   end Run_Test;

   overriding
   procedure Run_Test (T : in out Test_Parse_Devicetree_Errors) is
      pragma Unreferenced (T);

      Memory_Regions      : Physical_Memory_Region_Array_T (1 .. 16);
      Memory_Region_Count : Natural := 0;
      Harts               : Devicetree_Hart_Array_T (1 .. 16);
      Hart_Count          : Natural := 0;
      Timebase_Frequency  : Unsigned_64 := 0;
      Result              : Function_Result := Unset;

      --  Room for only 5 of the 10 devices.
      Too_Few_Devices : Devicetree_Device_Array_T (1 .. 5);
      Devices         : Devicetree_Device_Array_T (1 .. 16);
      Device_Count    : Natural := 0;

      DTB : DTB_Buffer_T
      with Alignment => 8;
   begin
      Read_DTB ("virt.dtb", DTB);

      Parse_Devicetree
        (DTB'Address,
         Memory_Regions,
         Memory_Region_Count,
         Too_Few_Devices,
         Device_Count,
         Harts,
         Hart_Count,
         Timebase_Frequency,
         Result);
      Assert
        (Result = Region_Array_Exhausted,
         "Too small a device array fails with Region_Array_Exhausted");
      Assert (Device_Count = 0, "No devices are returned on failure");

      --  Break the magic number at the start of the header.
      DTB (DTB'First) := 0;
      Parse_Devicetree
        (DTB'Address,
         Memory_Regions,
         Memory_Region_Count,
         Devices,
         Device_Count,
         Harts,
         Hart_Count,
         Timebase_Frequency,
         Result);
      Assert (Is_Error (Result), "A DTB with a bad magic number fails");
   end Run_Test;

   overriding
   procedure Run_Test (T : in out Test_Parse_Node_Status) is
      pragma Unreferenced (T);

      Memory_Regions      : Physical_Memory_Region_Array_T (1 .. 16);
      Memory_Region_Count : Natural := 0;
      Devices             : Devicetree_Device_Array_T (1 .. 16);
      Device_Count        : Natural := 0;
      Harts               : Devicetree_Hart_Array_T (1 .. 16);
      Hart_Count          : Natural := 0;
      Timebase_Frequency  : Unsigned_64 := 0;
      Result              : Function_Result := Unset;

      DTB : DTB_Buffer_T
      with Alignment => 8;
   begin
      Read_DTB ("status.dtb", DTB);

      Parse_Devicetree
        (DTB'Address,
         Memory_Regions,
         Memory_Region_Count,
         Devices,
         Device_Count,
         Harts,
         Hart_Count,
         Timebase_Frequency,
         Result);
      Assert (Result = Success, "Parse_Devicetree succeeds");

      --  The disabled memory node isn't added, and the disabled reservation
      --  doesn't split the enabled memory node.
      Assert (Memory_Region_Count = 1, "There is one memory region");
      Assert
        (Memory_Regions (1)
         = (Base => Create_Phys_Addr (16#8000_0000#), Size => 16#100_0000#),
         "The region is the whole of the enabled memory node");

      --  The UARTs with "okay", "ok" and no 'status' are kept, and the ones
      --  with "disabled" and "fail" are skipped. The PLIC comes after them.
      Assert (Device_Count = 4, "There are four available devices");

      Assert (Hart_Count = 1, "Only the available hart is returned");
      Assert
        (Harts (1)
         = (Hart_Id                     => 1,
            Has_Supervisor_PLIC_Context => True,
            Supervisor_PLIC_Context     => 2),
         "Hart 1's supervisor context comes from interrupts-extended");

      for Index in 1 .. 3 loop
         Assert
           (Devices (Index).Base
            = Create_Phys_Addr
                (16#1000_0000# + Integer_Address (Index - 1) * 16#1000#),
            "Device" & Index'Image & " is wrong");
      end loop;

      --  The root's 'interrupt-parent' is the PLIC, and the second UART has
      --  its own. The third UART comes after it, so it must get the root's
      --  again.
      Assert (Devices (4).Phandle = 16#20#, "The PLIC's phandle is read");
      Assert
        (Devices (1).Interrupt_Parent = 16#20#,
         "The first UART inherits the root's interrupt parent");
      Assert
        (Devices (2).Interrupt_Parent = 16#21#,
         "The second UART's own interrupt parent overrides the root's");
      Assert
        (Devices (3).Interrupt_Parent = 16#20#,
         "The third UART inherits the root's interrupt parent");
   end Run_Test;

end Devicetree.Test_Cases;
