-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Hart_State; use Hart_State;

package body Processes.Scheduler is
   procedure Verify_Context_Switch_Lock_State
     (Prev_Process : Process_Control_Block_Access)
   is
      Hart_Id : constant Hart_Index_T := Get_Current_Hart_Id;
   begin
      Counter : constant Natural :=
        Hart_States (Hart_Id).Interrupts_Off_Counter;

      if Prev_Process = null then
         --  If this is the first process being scheduled on this hart, there
         --  should be no locks held.
         if Counter /= 0 then
            Panic
              ("Hart#"
               & Hart_Id'Image
               & " has locks held at first schedule: "
               & Counter'Image);
         end if;
      else
         --  If there is a previous process, there should be exactly one lock
         --  held: the outgoing process's spinlock, which is held for the
         --  context-save hand-off.
         if Counter /= 1 then
            Panic
              ("Hart#"
               & Hart_Id'Image
               & " wrong lock count at context switch: "
               & Counter'Image);
         end if;

         --  Verify that the lock being held is held by the hart performing the
         --  context switch, and that it is the outgoing process's spinlock.
         if not Is_Current_Hart_Holding_Spinlock
                  (Prev_Process.all.Spinlock, Hart_Id)
         then
            Panic
              ("Hart#"
               & Hart_Id'Image
               & " lock held at context switch is not the outgoing"
               & " process's spinlock");
         end if;
      end if;
   exception
      when Constraint_Error =>
         Panic ("Constraint_Error: Verify_Context_Switch_Lock_State");
   end Verify_Context_Switch_Lock_State;

   procedure Schedule_Next_Process_Unlocked
     (Current_Process        : Process_Control_Block_Access;
      Next_Process           : out Process_Control_Block_Access;
      New_Prev_Process_State : Process_Status_T)
   is
      First_Process_To_Check : Process_Control_Block_Access := null;
   begin
      if Current_Process /= null then
         Acquire_Spinlock (Current_Process.all.Spinlock);
         Current_Process.all.Status := New_Prev_Process_State;
      --  The lock is intentionally NOT released here. It's held across the
      --  context switch (Switch_Process_Context) and released by the next
      --  process to run (Finish_Context_Switch). This means that the waker
      --  (Wake_Processes_Waiting_For_Channel) and the ready-scan below both
      --  block on this process until its context is fully saved, preventing
      --  another hart from resuming it on a stale/half-saved context.

      end if;

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
         if Next_Process = Current_Process then
            --  We already hold this process's spinlock (acquired above and
            --  held for the context-save hand-off) and have already set its
            --  status, so evaluate it without re-acquiring. If it is still
            --  runnable (e.g. it was pre-empted rather than blocked) it may be
            --  selected again; the lock stays held for Finish_Context_Switch.
            if Next_Process.all.Status = Process_Ready then
               Next_Process.all.Status := Process_Running;
               return;
            end if;
         else
            Acquire_Spinlock (Next_Process.all.Spinlock);

            if Next_Process.all.Status = Process_Ready then
               Next_Process.all.Status := Process_Running;
               Release_Spinlock (Next_Process.all.Spinlock);
               return;
            end if;

            Release_Spinlock (Next_Process.all.Spinlock);
         end if;

         Next_Process := Next_Process.all.Next_Process;
         if Next_Process = null then
            --  If we have reached the end of the process queue, loop back to
            --  the start of the queue.
            Next_Process := Process_Queue;
         end if;

         if Next_Process = First_Process_To_Check then
            --  If we have looped through the entire process queue and not
            --  found a ready process, exit.
            goto No_Ready_Processes;
         end if;
      end loop;

      --  At this point 'Next_Process' will be null, as no processes are ready.
      <<No_Ready_Processes>>
      --  If there are no processes ready to run, switch to the idle process.
      Next_Process := Hart_Idle_Processes (Get_Current_Hart_Id);
   exception
      when Constraint_Error =>
         Panic ("Constraint_Error: Schedule_Next_Process_Unlocked");
   end Schedule_Next_Process_Unlocked;

   procedure Schedule_Next_Process
     (Current_Process        : Process_Control_Block_Access;
      Next_Process           : out Process_Control_Block_Access;
      New_Prev_Process_State : Process_Status_T) is
   begin
      Acquire_Spinlock (Process_Queue_Spinlock);
      Schedule_Next_Process_Unlocked
        (Current_Process, Next_Process, New_Prev_Process_State);
      Release_Spinlock (Process_Queue_Spinlock);
   end Schedule_Next_Process;

   procedure Lock_Process_Waiting_For_Channel
     (Channel        : Blocking_Channel_T;
      Condition_Lock : in out Spinlock_T;
      Process        : in out Process_Control_Block_T) is
   begin
      Acquire_Spinlock (Process.Spinlock);
      Process.Blocked_By_Channel := Channel;
      Release_Spinlock (Process.Spinlock);

      Log_Debug
        ("Process "
         & Process.Process_Id'Image
         & " now blocked on channel: "
         & Channel'Image,
         Logging_Tags_Scheduler);

      Run_Guarded (Process_Blocked_Waiting_For_Response, Condition_Lock);

      --  Control will return to this point once the process is awakened.
      Acquire_Spinlock (Condition_Lock);
   exception
      when Constraint_Error =>
         Panic ("Constraint_Error: Lock_Process_Waiting_For_Channel");
   end Lock_Process_Waiting_For_Channel;

   procedure Print_Process_Switch_Info
     (Prev_Process, Next_Process : Process_Control_Block_Access)
   is
      Hart_Id : constant Hart_Index_T := Get_Current_Hart_Id;
   begin
      Log_Debug
        ("Scheduler.Run: "
         & (if Prev_Process /= null
            then
              "Old PID#"
              & Prev_Process.all.Process_Id'Image
              & (if Prev_Process = Hart_Idle_Processes (Hart_Id)
                 then " (Idle)"
                 else "")
            else "No previous process")
         & ", New PID#"
         & Next_Process.all.Process_Id'Image
         & (if Next_Process = Hart_Idle_Processes (Hart_Id)
            then " (Idle)"
            else ""),
         Logging_Tags_Scheduler);
   exception
      when Constraint_Error =>
         Panic ("Constraint_Error: Print_Process_Switch_Info");
   end Print_Process_Switch_Info;

   procedure Finish_Context_Switch is
      Hart_Id      : constant Hart_Index_T := Get_Current_Hart_Id;
      Prev_Process : Process_Control_Block_Access;
   begin
      --  Called by the newly-running process immediately after a context
      --  switch (implicitly from Switch_Process_Context, or explicitly as the
      --  first action of a first-run entry routine). Releases the per-process
      --  spinlock of the process we switched away from, now that its context
      --  has been fully saved. Releasing this lock also pops the interrupt-off
      --  nesting it established.
      Prev_Process := Hart_States (Hart_Id).Previous_Process;
      if Prev_Process /= null then
         Hart_States (Hart_Id).Previous_Process := null;
         Release_Spinlock (Prev_Process.all.Spinlock);
      end if;
   exception
      when Constraint_Error =>
         Panic ("Constraint_Error: Finish_Context_Switch");
   end Finish_Context_Switch;

   procedure Switch_Process_Context
     (Prev_Process, Next_Process : Process_Control_Block_Access)
   is
      --  Save the current kernel context, and load a new one.
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
        No_Return,
        Import,
        Convention    => Assembler,
        External_Name => "scheduler_load_kernel_context";
   begin
      Verify_Context_Switch_Lock_State (Prev_Process);

      Print_Process_Switch_Info (Prev_Process, Next_Process);

      --  Record the process we are switching away from. Its spinlock was
      --  acquired in Schedule_Next_Process, and is held across the save below.
      --  The next process to run releases it via Finish_Context_Switch.
      Hart_States (Get_Current_Hart_Id).Previous_Process := Prev_Process;

      --  Handle the possibility that there is no current process running on
      --  the current hart. This could be because it's the first time the
      --  scheduler is running.
      if Prev_Process = null then
         --  No outgoing context to save and no lock held. Previous_Process is
         --  null, so the incoming process's Finish_Context_Switch is a no-op.
         --  Load_Kernel_Context does not return here. Control resumes in the
         --  loaded process.
         Load_Kernel_Context
           (Get_Process_SATP (Next_Process.all),
            Next_Process.all.Memory_Space.Address_Space_ID,
            Next_Process.all);
      elsif Prev_Process /= Next_Process then
         --  Switch to the next process' kernel context. The next process
         --  releases our (Prev_Process') spinlock via Finish_Context_Switch.
         --  When *this* process is resumed, execution continues after
         --  Switch_Kernel_Context returns, and the Finish_Context_Switch below
         --  releases the spinlock of whichever process yielded the hart.
         Switch_Kernel_Context
           (Get_Process_SATP (Next_Process.all),
            Next_Process.all.Memory_Space.Address_Space_ID,
            Next_Process.all,
            Prev_Process.all.Kernel_Context);
      end if;

      --  Reached two ways:
      --  1. Directly, when no switch occurred (Prev = Next,
      --  e.g. idle re-selected) = releases the spinlock we hold on ourselves.
      --  2. On resume after Switch_Kernel_Context returns = releases the
      --  spinlock of whichever process yielded the hart to us.
      Finish_Context_Switch;
   exception
      when Constraint_Error =>
         Panic ("Constraint_Error: Switch_Process_Context");
   end Switch_Process_Context;

   procedure Run (New_Prev_Process_State : Process_Status_T := Process_Ready)
   is
      Prev_Process, Next_Process : Process_Control_Block_Access := null;

      Hart_Id : constant Hart_Index_T := Get_Current_Hart_Id;
   begin
      Prev_Process := Hart_States (Hart_Id).Current_Process;

      Schedule_Next_Process
        (Prev_Process, Next_Process, New_Prev_Process_State);

      Hart_States (Hart_Id).Current_Process := Next_Process;

      Switch_Process_Context (Prev_Process, Next_Process);

      --  A previously pre-empted process will resume execution here when
      --  control returns to it, after being scheduled again.
      Log_Debug ("Scheduler.Run: Exiting scheduler", Logging_Tags_Scheduler);
   exception
      when Constraint_Error =>
         Panic ("Constraint_Error: Scheduler.Run");
   end Run;

   procedure Run_Guarded
     (New_Prev_Process_State : Process_Status_T := Process_Ready;
      Condition_Lock         : in out Spinlock_T)
   is
      Prev_Process, Next_Process : Process_Control_Block_Access := null;

      Hart_Id : constant Hart_Index_T := Get_Current_Hart_Id;
   begin
      Prev_Process := Hart_States (Hart_Id).Current_Process;

      Schedule_Next_Process
        (Prev_Process, Next_Process, New_Prev_Process_State);

      Hart_States (Hart_Id).Current_Process := Next_Process;

      Release_Spinlock (Condition_Lock);

      Switch_Process_Context (Prev_Process, Next_Process);

      --  A previously pre-empted process will resume execution here when
      --  control returns to it, after being scheduled again.
      Log_Debug
        ("Scheduler.Run_Guarded: Exiting scheduler", Logging_Tags_Scheduler);
   exception
      when Constraint_Error =>
         Panic ("Constraint_Error: Scheduler.Run_Guarded");
   end Run_Guarded;

   procedure Wake_Processes_Waiting_For_Channel_Unlocked
     (Channel : Blocking_Channel_T)
   is
      Curr_Process : Process_Control_Block_Access := null;
   begin
      Log_Debug
        ("Waking processes waiting for channel: " & Channel'Image,
         Logging_Tags_Scheduler);

      Curr_Process := Process_Queue;
      while Curr_Process /= null loop
         Acquire_Spinlock (Curr_Process.all.Spinlock);

         if Curr_Process.all.Status = Process_Blocked_Waiting_For_Response
           and then Curr_Process.all.Blocked_By_Channel = Channel
         then
            Log_Debug
              ("Waking process with PID#" & Curr_Process.all.Process_Id'Image,
               Logging_Tags_Scheduler);

            Curr_Process.all.Status := Process_Ready;
            Curr_Process.all.Blocked_By_Channel := 0;
         end if;

         Release_Spinlock (Curr_Process.all.Spinlock);

         Curr_Process := Curr_Process.all.Next_Process;
      end loop;
   exception
      when Constraint_Error =>
         --  If a constraint error occurs while waking processes, it's likely
         --  that the system is in an invalid state. In this case it's better
         --  to panic and halt the system rather than continue.
         Panic
           ("Constraint_Error: Wake_Processes_Waiting_For_Channel_Unlocked");
   end Wake_Processes_Waiting_For_Channel_Unlocked;

   procedure Wake_Processes_Waiting_For_Channel (Channel : Blocking_Channel_T)
   is
   begin
      Acquire_Spinlock (Process_Queue_Spinlock);
      Wake_Processes_Waiting_For_Channel_Unlocked (Channel);
      Release_Spinlock (Process_Queue_Spinlock);
   end Wake_Processes_Waiting_For_Channel;

end Processes.Scheduler;
