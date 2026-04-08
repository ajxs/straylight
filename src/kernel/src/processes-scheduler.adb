-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Hart_State; use Hart_State;
with Utilities;  use Utilities;

package body Processes.Scheduler is
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
         Release_Spinlock (Current_Process.all.Spinlock);
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
         Acquire_Spinlock (Next_Process.all.Spinlock);

         if Next_Process.all.Status = Process_Ready then
            Next_Process.all.Status := Process_Running;
            Release_Spinlock (Next_Process.all.Spinlock);
            return;
         end if;

         Release_Spinlock (Next_Process.all.Spinlock);

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

      Old_Process_Name, New_Process_Name : String (1 .. 24) :=
        "None                    ";
   begin
      if Next_Process = Hart_Idle_Processes (Hart_Id) then
         Set_Fixed_Length_String
           ("Idle (PID#" & Next_Process.all.Process_Id'Image & ")",
            New_Process_Name);
      else
         Set_Fixed_Length_String
           ("PID#" & Next_Process.all.Process_Id'Image, New_Process_Name);
      end if;

      if Prev_Process = Hart_Idle_Processes (Hart_Id) then
         Set_Fixed_Length_String
           ("Idle (PID#" & Prev_Process.all.Process_Id'Image & ")",
            Old_Process_Name);
      elsif Prev_Process /= null then
         Set_Fixed_Length_String
           ("PID#" & Prev_Process.all.Process_Id'Image, Old_Process_Name);
      end if;

      Log_Debug
        ("Scheduler.Run:"
         & ASCII.LF
         & "  Hart#"
         & Hart_Id'Image
         & ASCII.LF
         & "  Old Process: "
         & Old_Process_Name
         & ASCII.LF
         & "  New Process: "
         & New_Process_Name,
         Logging_Tags_Scheduler);
   exception
      when Constraint_Error =>
         Panic ("Constraint_Error: Print_Process_Switch_Info");
   end Print_Process_Switch_Info;

   procedure Switch_Process_Context
     (Prev_Process, Next_Process : Process_Control_Block_Access)
   is
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
   begin
      Print_Process_Switch_Info (Prev_Process, Next_Process);

      --  Handle the possibility that there is no current process running on
      --  the current HART. This could be because it's the first time the
      --  scheduler is running.
      if Prev_Process = null then
         Load_Kernel_Context
           (Get_Process_SATP (Next_Process.all),
            Next_Process.all.Memory_Space.Address_Space_ID,
            Next_Process.all);
      elsif Prev_Process /= Next_Process then
         --  Otherwise, switch to the next process' kernel context.
         --  The next process will resume execution from here, if it
         --  previously yielded control via the scheduler,
         --  or 'return' to the start process routine, if never run.
         Switch_Kernel_Context
           (Get_Process_SATP (Next_Process.all),
            Next_Process.all.Memory_Space.Address_Space_ID,
            Next_Process.all,
            Prev_Process.all.Kernel_Context);
      end if;
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
      Log_Debug
        ("Scheduler.Run: Hart#" & Hart_Id'Image & " exiting scheduler",
         Logging_Tags_Scheduler);
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
        ("Scheduler.Run_Guarded: Hart#" & Hart_Id'Image & " exiting scheduler",
         Logging_Tags_Scheduler);
   exception
      when Constraint_Error =>
         Panic ("Constraint_Error: Scheduler.Run_Guarded");
   end Run_Guarded;

   procedure Wake_Processes_Waiting_For_Channel_Unlocked
     (Channel : Blocking_Channel_T; Result : out Function_Result)
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
              ("Waking process with PID# " & Curr_Process.all.Process_Id'Image,
               Logging_Tags_Scheduler);

            Curr_Process.all.Status := Process_Ready;
            Curr_Process.all.Blocked_By_Channel := 0;
         end if;

         Release_Spinlock (Curr_Process.all.Spinlock);

         Curr_Process := Curr_Process.all.Next_Process;
      end loop;

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error
           ("Constraint_Error: Wake_Processes_Waiting_For_Channel_Unlocked");
         Result := Constraint_Exception;
   end Wake_Processes_Waiting_For_Channel_Unlocked;

   procedure Wake_Processes_Waiting_For_Channel
     (Channel : Blocking_Channel_T; Result : out Function_Result) is
   begin
      Acquire_Spinlock (Process_Queue_Spinlock);
      Wake_Processes_Waiting_For_Channel_Unlocked (Channel, Result);
      Release_Spinlock (Process_Queue_Spinlock);
   end Wake_Processes_Waiting_For_Channel;

end Processes.Scheduler;
