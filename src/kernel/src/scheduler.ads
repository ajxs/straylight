-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Function_Results; use Function_Results;
with Locks;            use Locks;
with Logging;          use Logging;
with Processes;        use Processes;

package Scheduler is
   pragma Preelaborate;

   procedure Run;

   procedure Lock_Process_Waiting_For_Channel
     (Channel        : Blocking_Channel_T;
      Condition_Lock : in out Spinlock_T;
      Process        : in out Process_Control_Block_T);

   procedure Wake_Processes_Waiting_For_Channel
     (Channel : Blocking_Channel_T; Result : out Function_Result);

   --  This is the entry point for a newly-created process.
   --  Newly-created processes have their initial 'return address' set to this
   --  procedure, so when they are first scheduled, and the process 'returns'
   --  from the context switch, they will start executing here.
   procedure Process_Start
   with No_Return;

private
   Logging_Tags : constant Log_Tags := [Log_Tag_Scheduler];

   procedure Get_Next_Scheduled_Process
     (Current_Process : Process_Control_Block_Access;
      Next_Process    : out Process_Control_Block_Access;
      Result          : out Function_Result);

end Scheduler;
