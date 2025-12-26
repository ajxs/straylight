-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

package Logging is
   pragma Preelaborate;

   type Log_Tag_T is
     (Log_Tag_Allocator,
      Log_Tag_Boot,
      Log_Tag_Devices,
      Log_Tag_Devices_Ramdisk,
      Log_Tag_Devices_VirtIO,
      Log_Tag_Devices_VirtIO_Graphics,
      Log_Tag_Filesystems,
      Log_Tag_Filesystems_Block_Cache,
      Log_Tag_Filesystems_FAT,
      Log_Tag_Filesystems_Root,
      Log_Tag_Filesystems_UStar,
      Log_Tag_Graphics,
      Log_Tag_Heap,
      Log_Tag_Idle,
      Log_Tag_Memory,
      Log_Tag_Memory_Page_Walking,
      Log_Tag_Loader,
      Log_Tag_Locks,
      Log_Tag_Physical_Memory_Allocation,
      Log_Tag_Physical_Memory_Manager,
      Log_Tag_Processes,
      Log_Tag_Scheduler,
      Log_Tag_System_Calls,
      Log_Tag_Traps,
      Log_Tag_Virtual_Memory_Manager);

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
      Log_Tag_Devices_VirtIO          => False,
      Log_Tag_Devices_VirtIO_Graphics => False,
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
      Log_Tag_Allocator               => False,
      Log_Tag_Traps                   => False,
      Log_Tag_Locks                   => False,
      Log_Tag_Loader                  => False,
      others                          => False];

   System_Logging_Level : Log_Level_T := Log_Level_Debug;

   procedure Log_Message
     (Message : String; Tags : Log_Tags; Level : Log_Level_T);

   procedure Log_Message_Wide
     (Wide_Message : Wide_String; Tags : Log_Tags; Level : Log_Level_T);

   procedure Log_To_Debug_Console (Message : String; Level : Log_Level_T);

   ----------------------------------------------------------------------------
   --  Printing a string to the SBI Debug Console requires knowing the
   --  physical address of the string to be printed. Most strings will be
   --  printed from the stack, which technically doesn't have a fixed physical
   --  address. To facilitate this, a static buffer is used to store the string
   --  before it's sent to the SBI Debug Console.
   --  The buffer is a fixed size, and the string is copied into the buffer
   --  in blocks of this size.
   ----------------------------------------------------------------------------
   SBI_Log_Buffer_Length : constant := 128;

   ----------------------------------------------------------------------------
   --  Statically-allocated buffer used to transfer strings to the SBI Debug
   --  Console.
   --  This needs to have a static address, so that we can easily derive the
   --  physical address to pass to the SBI call.
   ----------------------------------------------------------------------------
   SBI_Logging_Buffer : String (1 .. SBI_Log_Buffer_Length);

   function Should_Log_To_Debug_Console
     (Tags : Log_Tags; Level : Log_Level_T) return Boolean;

   procedure Print_String_To_SBI_Console (Message : String);

end Logging;
