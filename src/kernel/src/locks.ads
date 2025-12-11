-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Interfaces; use Interfaces;

package Locks is
   pragma Preelaborate;

   type Spinlock_T is record
      --  An unsigned 32-bit integer used to indicate whether the lock is held
      --  because the interface with the GCC __sync_lock_test_and_set builtin
      --  function is best implemented with a 32-bit integer.
      --  @TODO: This may change in future.
      Locked        : aliased Unsigned_32 := 0;
      Time_Acquired : Unsigned_64 := 0;
      Hart_Id       : Natural := 0;
   end record;

   Null_Spinlock : constant Spinlock_T :=
     (Locked => 0, Time_Acquired => 0, Hart_Id => 0);

   procedure Acquire_Spinlock (Lock : in out Spinlock_T);

   procedure Release_Spinlock (Lock : in out Spinlock_T);

private
   Lock_Magic_Number : constant Unsigned_32 := 7654_3210;

   function Is_Current_Hart_Holding_Spinlock
     (Lock : Spinlock_T; Hart_Id : Natural) return Boolean
   is (Lock.Locked = Lock_Magic_Number and then Lock.Hart_Id = Hart_Id);

end Locks;
