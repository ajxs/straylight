-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with GNAT.Source_Info;

package Logging
  with Preelaborate
is
   ----------------------------------------------------------------------------
   --  Per-subsystem debug logging switches.
   --  Each debugging logging call is gated behind these switches using a
   --  pragma Debug directive. The advantage of this systems is that if the
   --  switch is disabled, the compiler will optimise out the logging call.
   ----------------------------------------------------------------------------
   Debug_Boot                    : constant Boolean := False;
   Debug_Devices                 : constant Boolean := False;
   Debug_Devices_Ramdisk         : constant Boolean := False;
   Debug_Devices_Virtio          : constant Boolean := False;
   Debug_Devices_Virtio_Graphics : constant Boolean := False;
   Debug_Devicetree              : constant Boolean := False;
   Debug_Filesystems             : constant Boolean := False;
   Debug_Filesystems_Block_Cache : constant Boolean := False;
   Debug_Filesystems_Node_Cache  : constant Boolean := False;
   Debug_Filesystems_Root        : constant Boolean := False;
   Debug_Filesystems_UStar       : constant Boolean := False;
   Debug_Filesystems_FAT         : constant Boolean := False;
   Debug_Graphics                : constant Boolean := False;
   Debug_Heap                    : constant Boolean := False;
   Debug_Idle                    : constant Boolean := False;
   Debug_Loader                  : constant Boolean := False;
   Debug_Memory                  : constant Boolean := False;
   Debug_Memory_Allocators       : constant Boolean := False;
   Debug_Memory_Page_Walking     : constant Boolean := False;
   Debug_Memory_Physical         : constant Boolean := False;
   Debug_Memory_Virtual          : constant Boolean := False;
   Debug_Page_Pool               : constant Boolean := False;
   Debug_Processes               : constant Boolean := False;
   Debug_Scheduler               : constant Boolean := False;
   Debug_System_Calls            : constant Boolean := False;
   Debug_Traps                   : constant Boolean := False;

   type Log_Transport_T is (Log_Transport_Debug_Console);

   type Log_Level_T is (Log_Level_Error, Log_Level_Info, Log_Level_Debug);
   for Log_Level_T use
     (Log_Level_Error => 0, Log_Level_Info => 1, Log_Level_Debug => 2);

   procedure Log_Debug (Message : String);

   procedure Log_Debug_Wide (Message : Wide_String);

   procedure Log_Error (Message : String);

   ----------------------------------------------------------------------------
   --  Logs the source location at which a Constraint_Error was handled.
   --  The File and Line parameter defaults are GNAT intrinsics evaluated at
   --  the *call site*, so callers shouldn't pass either. Line is a static
   --  expression, compiling to a bare immediate.
   --  File is a static string literal, which GNAT emits once per compilation
   --  unit into a mergeable .rodata.str section.
   ----------------------------------------------------------------------------
   procedure Log_Constraint_Error
     (File : String := GNAT.Source_Info.File;
      Line : Positive := GNAT.Source_Info.Line);

private
   Active_Logging_Transports : constant array (Log_Transport_T) of Boolean :=
     [Log_Transport_Debug_Console => True];

end Logging;
