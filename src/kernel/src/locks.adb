-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Logging;       use Logging;
with RISCV;         use RISCV;
with RISCV.Atomics; use RISCV.Atomics;
with Hart_State;    use Hart_State;

package body Locks is
   procedure Acquire_Spinlock (Lock : in out Spinlock_T) is
      Hart_Id : constant Hart_Index_T := Get_Current_Hart_Id;
   begin
      Push_Interrupts_Off;

      --  Test whether this hart already holds the lock.
      if Is_Current_Hart_Holding_Spinlock (Lock, Hart_Id) then
         --  We panic here because there's no way of resolving this situation
         --  safely without halting the system.
         Panic
           ("Lock already held by this hart at: " & Lock.Time_Acquired'Image);
      end if;

      --  Loop until we successfully acquire the lock.
      --  This atomic swap implementation works because it continuously swaps
      --  the current value of the lock with the new value, until the previous
      --  value returned was '0', indicating the lock was released.
      while Atomic_Swap_And_Return_Unsigned_32
              (Lock.Locked'Access, Lock_Magic_Number)
        /= 0
      loop
         null;
      end loop;

      --  Tell the processor to not move loads or stores
      --  past this point, to ensure that the critical section's memory
      --  references happen strictly after the lock is acquired.
      Fence;

      Lock.Hart_Id := Hart_Id;
      Lock.Time_Acquired := Get_System_Time;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Acquire_Spinlock");
   end Acquire_Spinlock;

   procedure Release_Spinlock (Lock : in out Spinlock_T) is
      Hart_Id : constant Hart_Index_T := Get_Current_Hart_Id;
   begin
      if not Is_Current_Hart_Holding_Spinlock (Lock, Hart_Id) then
         Panic ("Lock not held");
      end if;

      --  There's no need to clear the Hart Id, as it will be overwritten
      --  by the next hart that acquires the lock, and it has no meaning in the
      --  context of the lock not being held.

      --  Tell the processor to not move loads or stores
      --  past this point, to ensure that the critical section's memory
      --  references happen strictly after the lock is acquired.
      Fence;

      --  Because the Ada compiler could perform the assignment to the lock
      --  variable with multiple instructions, we use an atomic swap operation
      --  to clear the lock.
      Atomic_Clear_Unsigned_32 (Lock.Locked'Access);

      Pop_Interrupts_Off;
   end Release_Spinlock;
end Locks;
