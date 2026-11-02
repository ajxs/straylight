-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Memory;              use Memory;
with Processes.Scheduler; use Processes.Scheduler;

package body Locks.Sleeplocks is
   procedure Acquire_Sleeplock
     (Lock           : in out Sleeplock_T;
      Condition_Lock : in out Spinlock_T;
      Process        : in out Process_Control_Block_T) is
   begin
      Acquire_Spinlock (Lock.Spinlock);
      Release_Spinlock (Condition_Lock);

      while Lock.Locked /= 0 loop
         Lock_Process_Waiting_For_Channel
           (Channel        => Address_To_Unsigned_64 (Lock'Address),
            Condition_Lock => Lock.Spinlock,
            Process        => Process);
      end loop;

      Lock.Locked := Sleeplock_Magic_Number;
      Lock.Process_Id := Process.Process_Id;

      Release_Spinlock (Lock.Spinlock);
      Acquire_Spinlock (Condition_Lock);
   end Acquire_Sleeplock;

   procedure Release_Sleeplock (Lock : in out Sleeplock_T) is
   begin
      Acquire_Spinlock (Lock.Spinlock);

      Lock.Locked := 0;
      Lock.Process_Id := No_Process_Id;
      Wake_Processes_Waiting_For_Channel
        (Address_To_Unsigned_64 (Lock'Address));

      Release_Spinlock (Lock.Spinlock);
   end Release_Sleeplock;

end Locks.Sleeplocks;
