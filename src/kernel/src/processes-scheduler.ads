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

   procedure Get_Next_Scheduled_Process
     (Current_Process : Process_Control_Block_Access;
      Next_Process    : out Process_Control_Block_Access;
      Result          : out Function_Result);

end Processes.Scheduler;
