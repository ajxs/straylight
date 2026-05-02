-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Processes; use Processes;

package Locks.Sleeplocks is
   pragma Preelaborate;

   type Sleeplock_T is record
      Locked     : aliased Unsigned_8 := 0;
      Spinlock   : Spinlock_T;
      Process_Id : Process_Id_T;
   end record;

   procedure Acquire_Sleeplock
     (Lock           : in out Sleeplock_T;
      Condition_Lock : in out Spinlock_T;
      Process_Id     : Process_Id_T);

   procedure Release_Sleeplock (Lock : in out Sleeplock_T);

   Sleeplock_Magic_Number : constant Unsigned_8;

   function Is_Sleeplock_Locked (Lock : Sleeplock_T) return Boolean
   is (Lock.Locked = Sleeplock_Magic_Number)
   with Inline;

private
   No_Process_Id : constant Process_Id_T := Process_Id_T'Last;

   Sleeplock_Magic_Number : constant Unsigned_8 := 123;

   function Is_Current_Process_Holding_Sleeplock
     (Lock : in out Sleeplock_T; Process_Id : Process_Id_T) return Boolean;

end Locks.Sleeplocks;
