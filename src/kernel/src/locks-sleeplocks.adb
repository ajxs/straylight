-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Addresses; use Addresses;
with Scheduler; use Scheduler;

package body Locks.Sleeplocks is
   procedure Acquire_Sleeplock
     (Lock       : in out Sleeplock_T;
      Process_Id : Process_Id_T;
      Result     : out Function_Result) is
   begin
      Acquiring_Process : constant Process_Control_Block_Access :=
        Find_Running_Process_With_Id (Process_Id);
      --  The case the process isn't found is an error condition.
      if Acquiring_Process = null then
         Result := Process_Not_Found;
         return;
      end if;

      Acquire_Spinlock (Lock.Spinlock);

      while Lock.Locked /= 0 loop
         Lock_Process_Waiting_For_Channel
           (Channel        => Address_To_Unsigned_64 (Lock'Address),
            Condition_Lock => Lock.Spinlock,
            Process        => Acquiring_Process.all);
      end loop;

      Lock.Locked := 1;
      Lock.Process_Id := Process_Id;

      Result := Success;

      Release_Spinlock (Lock.Spinlock);
   exception
      when Constraint_Error =>
         Result := Constraint_Exception;
   end Acquire_Sleeplock;

   function Is_Current_Process_Holding_Sleeplock
     (Lock : in out Sleeplock_T; Process_Id : Process_Id_T) return Boolean
   is
      Hold_Status : Boolean := False;
   begin
      Acquire_Spinlock (Lock.Spinlock);

      Hold_Status := Lock.Locked = 1 and then Lock.Process_Id = Process_Id;

      Release_Spinlock (Lock.Spinlock);

      return Hold_Status;
   end Is_Current_Process_Holding_Sleeplock;

   procedure Release_Sleeplock
     (Lock : in out Sleeplock_T; Result : out Function_Result) is
   begin
      Acquire_Spinlock (Lock.Spinlock);

      Lock.Locked := 0;
      Lock.Process_Id := 0;
      Wake_Processes_Waiting_For_Channel
        (Address_To_Unsigned_64 (Lock'Address), Result);

      Release_Spinlock (Lock.Spinlock);
   end Release_Sleeplock;

end Locks.Sleeplocks;
