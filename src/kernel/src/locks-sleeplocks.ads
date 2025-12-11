-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Function_Results; use Function_Results;
with Processes;        use Processes;

package Locks.Sleeplocks is
   pragma Preelaborate;

   type Sleeplock_T is record
      Locked     : aliased Unsigned_8 := 0;
      Spinlock   : Spinlock_T;
      Process_Id : Process_Id_T;
   end record;

   Null_Sleeplock : constant Sleeplock_T :=
     (Locked     => 0,
      Spinlock   => (Locked => 0, Time_Acquired => 0, Hart_Id => 0),
      Process_Id => 0);

   procedure Acquire_Sleeplock
     (Lock       : in out Sleeplock_T;
      Process_Id : Process_Id_T;
      Result     : out Function_Result);

   procedure Release_Sleeplock
     (Lock : in out Sleeplock_T; Result : out Function_Result);

private
   function Is_Current_Process_Holding_Sleeplock
     (Lock : in out Sleeplock_T; Process_Id : Process_Id_T) return Boolean;

end Locks.Sleeplocks;
