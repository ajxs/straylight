-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

package Processes.Scheduler
  with Preelaborate
is

   procedure Run (New_Prev_Process_State : Process_Status_T := Process_Ready);

   procedure Lock_Process_Waiting_For_Channel
     (Channel        : Blocking_Channel_T;
      Condition_Lock : in out Spinlock_T;
      Process        : in out Process_Control_Block_T);

   procedure Wake_Processes_Waiting_For_Channel (Channel : Blocking_Channel_T);

   --  Releases the per-process spinlock of the process this hart switched away
   --  from (recorded in Hart_States (..).Previous_Process), once the context
   --  switch process has completed. This must be called by every process as
   --  the first thing it does when it begins running after a context switch:
   --    - Implicitly, from Switch_Process_Context, for a resumed process;
   --    - Explicitly, at the top of each first-run entry routine
   --      (Process_Start, Processes.Idle), since a freshly-created process
   --      does not resume inside Switch_Process_Context.
   procedure Finish_Context_Switch;

private
   Logging_Tags_Scheduler : constant Log_Tags := [Log_Tag_Scheduler];

   procedure Schedule_Next_Process
     (Current_Process        : Process_Control_Block_Access;
      Next_Process           : out Process_Control_Block_Access;
      New_Prev_Process_State : Process_Status_T);

   procedure Print_Process_Switch_Info
     (Prev_Process, Next_Process : Process_Control_Block_Access);

   procedure Switch_Process_Context
     (Prev_Process, Next_Process : Process_Control_Block_Access);

   --  This procedure is used to run the scheduler while holding a lock.
   --  This is to avoid a 'lost wakeup' issue where a process is inadvertently
   --  woken up before it is put to sleep, which can lead to a deadlock.
   --  One scenario where this guard is required is putting a process to sleep
   --  pending a response from a channel.
   procedure Run_Guarded
     (New_Prev_Process_State : Process_Status_T := Process_Ready;
      Condition_Lock         : in out Spinlock_T);

end Processes.Scheduler;
