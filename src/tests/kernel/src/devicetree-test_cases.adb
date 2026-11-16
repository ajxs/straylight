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
   --  Reads the DTB that the kernel receives on QEMU's 'virt' machine, which
   --  scripts/capture_virt_dtb.sh captures. It's found relative to the test
   --  executable, at bin/../data/virt.dtb, so the tests can be run from any
   --  directory.
   ----------------------------------------------------------------------------
   procedure Read_Virt_DTB (DTB : out DTB_Buffer_T) is
      package Directories renames Ada.Directories;

      Tests_Directory : constant String :=
        Directories.Containing_Directory
          (Directories.Containing_Directory
             (Directories.Full_Name (Ada.Command_Line.Command_Name)));

      DTB_Path : constant String :=
        Directories.Compose
          (Directories.Compose (Tests_Directory, "data"), "virt.dtb");

      File : Stream_IO.File_Type;
      Last : Stream_Element_Offset := 0;
   begin
      Stream_IO.Open (File, Stream_IO.In_File, DTB_Path);
      Stream_IO.Read (File, DTB, Last);
      Stream_IO.Close (File);

      Assert
        (Last > 0 and then Last < DTB'Last,
         "virt.dtb was read, and fits in the buffer");
   end Read_Virt_DTB;

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
      Timebase_Frequency  : Unsigned_64 := 0;
      Result              : Function_Result := Unset;

      DTB : DTB_Buffer_T
      with Alignment => 8;

      procedure Assert_Device
        (Index          : Positive;
         Device_Type    : Devicetree_Device_Type_T;
         Base           : Integer_Address;
         Size           : Storage_Count;
         Interrupt_Line : Unsigned_32) is
      begin
         Assert
           (Devices (Index)
            = (Device_Type    => Device_Type,
               Base           => Create_Phys_Addr (Base),
               Size           => Size,
               Interrupt_Line => Interrupt_Line),
            "Device" & Index'Image & " is wrong");
      end Assert_Device;
   begin
      Read_Virt_DTB (DTB);

      Parse_Devicetree
        (DTB'Address,
         Memory_Regions,
         Memory_Region_Count,
         Devices,
         Device_Count,
         Timebase_Frequency,
         Result);
      Assert (Result = Success, "Parse_Devicetree succeeds");

      Assert
        (Timebase_Frequency = 10_000_000,
         "The timebase is QEMU's 10 MHz, from /cpus");

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
        (1, Devicetree_Device_Type_UART_NS16550A, 16#1000_0000#, 16#100#, 10);

      for Slot in 1 .. 8 loop
         Assert_Device
           (1 + Slot,
            Devicetree_Device_Type_Virtio_MMIO,
            16#1000_0000# + Integer_Address (9 - Slot) * 16#1000#,
            16#1000#,
            Unsigned_32 (9 - Slot));
      end loop;

      Assert_Device
        (10, Devicetree_Device_Type_PLIC, 16#0C00_0000#, 16#60_0000#, 0);
   end Run_Test;

   overriding
   procedure Run_Test (T : in out Test_Parse_Devicetree_Errors) is
      pragma Unreferenced (T);

      Memory_Regions      : Physical_Memory_Region_Array_T (1 .. 16);
      Memory_Region_Count : Natural := 0;
      Timebase_Frequency  : Unsigned_64 := 0;
      Result              : Function_Result := Unset;

      --  Room for only 5 of the 10 devices.
      Too_Few_Devices : Devicetree_Device_Array_T (1 .. 5);
      Devices         : Devicetree_Device_Array_T (1 .. 16);
      Device_Count    : Natural := 0;

      DTB : DTB_Buffer_T
      with Alignment => 8;
   begin
      Read_Virt_DTB (DTB);

      Parse_Devicetree
        (DTB'Address,
         Memory_Regions,
         Memory_Region_Count,
         Too_Few_Devices,
         Device_Count,
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
         Timebase_Frequency,
         Result);
      Assert (Is_Error (Result), "A DTB with a bad magic number fails");
   end Run_Test;

end Devicetree.Test_Cases;
