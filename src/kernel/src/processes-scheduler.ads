-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

package Processes.Scheduler is
   pragma Preelaborate;

   procedure Run (New_Prev_Process_State : Process_Status_T := Process_Ready);

   procedure Lock_Process_Waiting_For_Channel
     (Channel        : Blocking_Channel_T;
      Condition_Lock : in out Spinlock_T;
      Process        : in out Process_Control_Block_T);

   procedure Wake_Processes_Waiting_For_Channel
     (Channel : Blocking_Channel_T; Result : out Function_Result);

private
   Logging_Tags_Scheduler : constant Log_Tags := [Log_Tag_Scheduler];

   procedure Schedule_Next_Process
     (Current_Process        : Process_Control_Block_Access;
      Next_Process           : out Process_Control_Block_Access;
      New_Prev_Process_State : Process_Status_T);

   procedure Schedule_Next_Process_Unlocked
     (Current_Process        : Process_Control_Block_Access;
      Next_Process           : out Process_Control_Block_Access;
      New_Prev_Process_State : Process_Status_T);

   procedure Print_Process_Switch_Info
     (Prev_Process, Next_Process : Process_Control_Block_Access);

   procedure Wake_Processes_Waiting_For_Channel_Unlocked
     (Channel : Blocking_Channel_T; Result : out Function_Result);

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

   procedure Ensure_No_Locks_Held_Before_Context_Switch;

end Processes.Scheduler;
