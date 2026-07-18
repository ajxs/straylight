-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

package Logging
  with Preelaborate
is
   type Log_Tag_T is
     (Log_Tag_Boot,
      Log_Tag_Devices,
      Log_Tag_Devicetree,
      Log_Tag_Devices_Ramdisk,
      Log_Tag_Devices_Virtio,
      Log_Tag_Devices_Virtio_Graphics,
      Log_Tag_Filesystems,
      Log_Tag_Filesystems_Block_Cache,
      Log_Tag_Filesystems_FAT,
      Log_Tag_Filesystems_Node_Cache,
      Log_Tag_Filesystems_Root,
      Log_Tag_Filesystems_UStar,
      Log_Tag_Graphics,
      Log_Tag_Heap,
      Log_Tag_Heap_Test_Region,
      Log_Tag_Page_Pool,
      Log_Tag_Idle,
      Log_Tag_Memory,
      Log_Tag_Memory_Allocators,
      Log_Tag_Memory_Page_Walking,
      Log_Tag_Memory_Physical,
      Log_Tag_Memory_Virtual,
      Log_Tag_Loader,
      Log_Tag_Locks,
      Log_Tag_Processes,
      Log_Tag_Scheduler,
      Log_Tag_System_Calls,
      Log_Tag_Traps);

   type Log_Tags is array (Natural range <>) of Log_Tag_T;

   type Log_Transport_T is (Log_Transport_Debug_Console);

   Empty_Tag_List : constant Log_Tags := [1 .. 0 => <>];

   type Log_Level_T is (Log_Level_Error, Log_Level_Info, Log_Level_Debug);
   for Log_Level_T use
     (Log_Level_Error => 0, Log_Level_Info => 1, Log_Level_Debug => 2);

   procedure Log_Debug (Message : String; Tags : Log_Tags := Empty_Tag_List);

   procedure Log_Debug_Wide
     (Message : Wide_String; Tags : Log_Tags := Empty_Tag_List);

   procedure Log_Error (Message : String; Tags : Log_Tags := Empty_Tag_List);

private
   Active_Logging_Transports : constant array (Log_Transport_T) of Boolean :=
     [Log_Transport_Debug_Console => True];

   Active_Logging_Tags : constant array (Log_Tag_T) of Boolean :=
     [Log_Tag_Boot                    => False,
      Log_Tag_Devices                 => False,
      Log_Tag_Devicetree              => False,
      Log_Tag_Devices_Virtio          => False,
      Log_Tag_Devices_Virtio_Graphics => False,
      Log_Tag_Filesystems             => False,
      Log_Tag_Filesystems_Block_Cache => False,
      Log_Tag_Filesystems_FAT         => False,
      Log_Tag_Filesystems_Root        => False,
      Log_Tag_Filesystems_UStar       => False,
      Log_Tag_Graphics                => False,
      Log_Tag_Heap                    => False,
      Log_Tag_Idle                    => False,
      Log_Tag_Processes               => False,
      Log_Tag_System_Calls            => False,
      Log_Tag_Scheduler               => False,
      Log_Tag_Memory_Allocators       => False,
      Log_Tag_Traps                   => False,
      Log_Tag_Locks                   => False,
      Log_Tag_Loader                  => False,
      others                          => False];

   System_Logging_Level : Log_Level_T := Log_Level_Debug;

end Logging;
