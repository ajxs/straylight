-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Interfaces; use Interfaces;

package Locks is
   pragma Preelaborate;

   No_Hart_Id : constant Natural := Natural'Last;

   subtype Lock_Id_T is Unsigned_16;

   Lock_Id_Process_Queue            : constant Lock_Id_T := 1;
   Lock_Id_Process_Id               : constant Lock_Id_T := 2;
   Lock_Id_SBI_Log_Buffer           : constant Lock_Id_T := 3;
   Lock_Id_Open_Files               : constant Lock_Id_T := 4;
   Lock_Id_FS_Node_Cache            : constant Lock_Id_T := 5;
   Lock_Id_Block_Cache              : constant Lock_Id_T := 6;
   Lock_Id_Kernel_Page_Pool         : constant Lock_Id_T := 7;
   Lock_Id_Kernel_Heap              : constant Lock_Id_T := 8;
   Lock_Id_Physical_Memory          : constant Lock_Id_T := 9;
   Lock_Id_Devices_Prefix           : constant Lock_Id_T := 1_000;
   Lock_Id_Block_Cache_Entry_Prefix : constant Lock_Id_T := 2_000;
   Lock_Id_Process_Prefix           : constant Lock_Id_T := 3_000;

   type Spinlock_T is record
      --  An unsigned 32-bit integer used to indicate whether the lock is held
      --  because the interface with the GCC __sync_lock_test_and_set builtin
      --  function is best implemented with a 32-bit integer.
      --  @TODO: This may change in future.
      Locked        : aliased Unsigned_32 := 0;
      Time_Acquired : Unsigned_64 := 0;
      Hart_Id       : Natural := No_Hart_Id;
      Lock_Id       : Lock_Id_T := 0;
   end record;

   Null_Spinlock : constant Spinlock_T :=
     (Locked => 0, Time_Acquired => 0, Hart_Id => No_Hart_Id, Lock_Id => 0);

   procedure Acquire_Spinlock (Lock : in out Spinlock_T);

   procedure Release_Spinlock (Lock : in out Spinlock_T);

private
   Lock_Magic_Number : constant Unsigned_32 := 7654_3210;

   function Is_Current_Hart_Holding_Spinlock
     (Lock : Spinlock_T; Hart_Id : Natural) return Boolean
   is (Lock.Locked = Lock_Magic_Number and then Lock.Hart_Id = Hart_Id)
   with Inline;

end Locks;
