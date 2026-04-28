-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Hart_State;          use Hart_State;
with Memory;              use Memory;
with Processes.Scheduler; use Processes.Scheduler;

package body Locks.Sleeplocks is
   procedure Acquire_Sleeplock
     (Lock           : in out Sleeplock_T;
      Condition_Lock : in out Spinlock_T;
      Process_Id     : Process_Id_T) is
   begin
      Acquiring_Process : constant Process_Control_Block_Access :=
        Find_Running_Process_With_Id (Process_Id);
      --  The case the process isn't found is an error condition.
      if Acquiring_Process = null then
         Panic
           ("Process with ID#"
            & Process_Id'Image
            & " not found when trying to acquire sleeplock.");
      end if;

      Acquire_Spinlock (Lock.Spinlock);
      Release_Spinlock (Condition_Lock);

      while Lock.Locked /= 0 loop
         Lock_Process_Waiting_For_Channel
           (Channel        => Address_To_Unsigned_64 (Lock'Address),
            Condition_Lock => Lock.Spinlock,
            Process        => Acquiring_Process.all);
      end loop;

      Lock.Locked := Sleeplock_Magic_Number;
      Lock.Process_Id := Process_Id;

      Acquire_Spinlock (Condition_Lock);
      Release_Spinlock (Lock.Spinlock);
   exception
      when Constraint_Error =>
         Panic ("Constraint_Error: Acquire_Sleeplock");
   end Acquire_Sleeplock;

   function Is_Current_Process_Holding_Sleeplock
     (Lock : in out Sleeplock_T; Process_Id : Process_Id_T) return Boolean
   is
      Hold_Status : Boolean := False;
   begin
      Acquire_Spinlock (Lock.Spinlock);

      Hold_Status :=
        Lock.Locked = Sleeplock_Magic_Number
        and then Lock.Process_Id = Process_Id;

      Release_Spinlock (Lock.Spinlock);

      return Hold_Status;
   end Is_Current_Process_Holding_Sleeplock;

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
