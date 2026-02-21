-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Hart_State; use Hart_State;

package body Processes.Scheduler is
   procedure Get_Next_Scheduled_Process
     (Current_Process : Process_Control_Block_Access;
      Next_Process    : out Process_Control_Block_Access;
      Result          : out Function_Result)
   is
      First_Process_To_Check : Process_Control_Block_Access := null;
   begin
      --  If there are no processes in the queue, exit.
      if Process_Queue = null then
         goto No_Ready_Processes;
      end if;

      if Current_Process = null then
         --  If there is no currently running process, start with the head
         --  of the process queue.
         First_Process_To_Check := Process_Queue;
      else
         if Current_Process.all.Next_Process = null then
            --  If the current process is the last in the queue, start from
            --  the beginning.
            First_Process_To_Check := Process_Queue;
         else
            --  If there is a currently running process, start by checking its
            --  next process.
            First_Process_To_Check := Current_Process.all.Next_Process;
         end if;
      end if;

      Next_Process := First_Process_To_Check;

      --  Iterate through the process queue, looking for a ready process.
      loop
         if Next_Process.all.Status = Process_Ready then
            Result := Success;
            return;
         end if;

         Next_Process := Next_Process.all.Next_Process;
         if Next_Process = null then
            --  If we have reached the end of the process queue, loop back to
            --  the start of the queue.
            Next_Process := Process_Queue;
         end if;

         if Next_Process = First_Process_To_Check then
            --  If we have looped through the entire  process queue and not
            --  found a ready process, exit.
            goto No_Ready_Processes;
         end if;
      end loop;

      --  At this point 'Process' will be null, as no processes are ready.
      <<No_Ready_Processes>>
      Next_Process := null;
      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Get_Next_Scheduled_Process");
         Result := Constraint_Exception;
         Next_Process := null;
   end Get_Next_Scheduled_Process;

   procedure Lock_Process_Waiting_For_Channel
     (Channel        : Blocking_Channel_T;
      Condition_Lock : in out Spinlock_T;
      Process        : in out Process_Control_Block_T) is
   begin
      Acquire_Spinlock (Process.Spinlock);

      Release_Spinlock (Condition_Lock);

      Process.Blocked_By_Channel := Channel;

      Log_Debug
        ("Process "
         & Process.Process_Id'Image
         & " now blocked on channel: "
         & Channel'Image,
         Logging_Tags_Scheduler);

      Run (Process_Blocked_Waiting_For_Response);

      --  Control will return to this point once the process is awakened.
      Release_Spinlock (Process.Spinlock);

      Acquire_Spinlock (Condition_Lock);
   exception
      when Constraint_Error =>
         Panic ("Constraint_Error: Lock_Process_Waiting_For_Channel");
   end Lock_Process_Waiting_For_Channel;

   procedure Run (New_Prev_Process_State : Process_Status_T := Process_Ready)
   is
      Prev_Process, Next_Process : Process_Control_Block_Access := null;

      Hart_Id : constant Hart_Index_T := Get_Current_Hart_Id;

      --  Save the current kernel context, and load a new one.
      --  Interrupts are re-enabled in this procedure.
      procedure Switch_Kernel_Context
        (SATP                : Unsigned_64;
         ASID                : Unsigned_16;
         New_Process         : Process_Control_Block_T;
         Old_Process_Context : in out Kernel_Context_T)
      with
        Import,
        Convention    => Assembler,
        External_Name => "scheduler_switch_kernel_context";

      --  Load a new kernel context without saving the current one.
      --  Interrupts are re-enabled in this procedure.
      procedure Load_Kernel_Context
        (SATP        : Unsigned_64;
         ASID        : Unsigned_16;
         New_Process : Process_Control_Block_T)
      with
        Import,
        Convention    => Assembler,
        External_Name => "scheduler_load_kernel_context";

      Result : Function_Result := Unset;

   begin
      Process_Queue_Spinlock.Acquire_Spinlock;

      Prev_Process := Hart_States (Hart_Id).Current_Process;
      if Prev_Process /= null then
         Prev_Process.all.Status := New_Prev_Process_State;
      end if;

      Get_Next_Scheduled_Process (Prev_Process, Next_Process, Result);
      if Is_Error (Result) then
         Panic ("Get_Next_Scheduled_Process failed: " & Result'Image);
      end if;

      --  If there are no processes ready to run, switch to the idle process.
      if Next_Process = null then
         Next_Process := Hart_Idle_Processes (Hart_Id);
      end if;

      --  If there is no ready processes found, the current process will be
      --  set as null, to trigger the scheduler to start searching for the
      --  next process from the beginning of the process queue next time.
      Hart_States (Hart_Id).Current_Process := Next_Process;

      Next_Process.all.Status := Process_Running;

      Process_Queue_Spinlock.Release_Spinlock;

      --  Handle the possibility that there is no current process running on
      --  the current HART. This could be because it's the first time the
      --  scheduler is running.
      if Prev_Process = null then
         Log_Debug
           ("Scheduler.Run: Hart "
            & Hart_Id'Image
            & " switching from none to next process with PID#"
            & Next_Process.all.Process_Id'Image,
            Logging_Tags_Scheduler);

         Load_Kernel_Context
           (Get_Process_SATP (Next_Process.all),
            Next_Process.all.Memory_Space.Address_Space_ID,
            Next_Process.all);
      elsif Prev_Process = Next_Process then
         --  If the next process to run is the same as the previous
         --  process, simply return.
         Log_Debug
           ("Scheduler.Run: Hart "
            & Hart_Id'Image
            & " continuing to run current process with PID#"
            & Next_Process.all.Process_Id'Image,
            Logging_Tags_Scheduler);
      else
         --  Otherwise, switch to the next process' kernel context.
         --  The next process will resume execution from here, if it
         --  previously yielded control via the scheduler,
         --  or 'return' to the start process routine, if never run.

         Log_Debug
           ("Scheduler.Run: Hart "
            & Hart_Id'Image
            & " switching from PID#: "
            & Prev_Process.all.Process_Id'Image
            & " to PID#: "
            & Next_Process.all.Process_Id'Image,
            Logging_Tags_Scheduler);

         Switch_Kernel_Context
           (Get_Process_SATP (Next_Process.all),
            Next_Process.all.Memory_Space.Address_Space_ID,
            Next_Process.all,
            Prev_Process.all.Kernel_Context);
      end if;

      --  A previously pre-empted process will resume execution here when
      --  control returns to it, after being scheduled again.
      Log_Debug ("Scheduler.Run: Returning to caller", Logging_Tags_Scheduler);
   exception
      when Constraint_Error =>
         Panic ("Constraint_Error: Scheduler.Run");
   end Run;

   procedure Wake_Processes_Waiting_For_Channel
     (Channel : Blocking_Channel_T; Result : out Function_Result)
   is
      Curr_Process : Process_Control_Block_Access := null;
   begin
      Log_Debug
        ("Waking processes waiting for channel: " & Channel'Image,
         Logging_Tags_Scheduler);

      Curr_Process := Process_Queue;
      while Curr_Process /= null loop
         if Curr_Process.all.Status = Process_Blocked_Waiting_For_Response
           and then Curr_Process.all.Blocked_By_Channel = Channel
         then
            Log_Debug
              ("Waking process with PID# " & Curr_Process.all.Process_Id'Image,
               Logging_Tags_Scheduler);
            Curr_Process.all.Status := Process_Ready;
            Curr_Process.all.Blocked_By_Channel := 0;
         end if;

         Curr_Process := Curr_Process.all.Next_Process;
      end loop;

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Wake_Processes_Waiting_For_Channel");
         Result := Constraint_Exception;
   end Wake_Processes_Waiting_For_Channel;

end Processes.Scheduler;
