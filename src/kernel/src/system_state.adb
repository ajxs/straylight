-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Logging;        use Logging;
with Memory.Virtual; use Memory.Virtual;
with RISCV.Interrupts;
with Scheduler;

package body System_State is
   procedure Add_Process (New_Process : Process_Control_Block_Access) is
      Curr_Process : Process_Control_Block_Access := null;
      Prev_Process : Process_Control_Block_Access := null;
   begin
      Log_Debug
        ("Adding process id "
         & New_Process.all.Process_Id'Image
         & ", address "
         & New_Process.all'Address'Image,
         [Log_Tag_Processes]);
      if Current_System_State.Processes = null then
         Log_Debug
           ("No processes in list, setting new process as head",
            [Log_Tag_Processes]);
         Current_System_State.Processes := New_Process;
         return;
      end if;

      Curr_Process := Current_System_State.Processes;
      while Curr_Process /= null loop
         Prev_Process := Curr_Process;
         Curr_Process := Curr_Process.all.Next_Process;
      end loop;

      Prev_Process.all.Next_Process := New_Process;

      Log_Debug ("Added process to end of list", [Log_Tag_Processes]);

   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Add_Process");
   end Add_Process;

   procedure Allocate_Kernel_Memory
     (Size              : Positive;
      Allocated_Address : out Virtual_Address_T;
      Result            : out Function_Result;
      Alignment         : Storage_Offset := 1)
   is
      Allocation_Result : Memory_Allocation_Result;
   begin
      Current_System_State.Kernel_Heap.Allocate
        (Size, Allocation_Result, Result, Alignment);

      Allocated_Address := Allocation_Result.Virtual_Address;
   end Allocate_Kernel_Memory;

   procedure Allocate_Kernel_Physical_Memory
     (Size              : Positive;
      Allocation_Result : out Memory_Allocation_Result;
      Result            : out Function_Result;
      Alignment         : Storage_Offset := 1) is
   begin
      Current_System_State.Kernel_Heap.Allocate
        (Size, Allocation_Result, Result, Alignment);
   end Allocate_Kernel_Physical_Memory;

   procedure Allocate_Pages
     (Number_of_Pages   : Positive;
      Allocation_Result : out Memory_Allocation_Result;
      Result            : out Function_Result) is
   begin
      Current_System_State.Kernel_Page_Pool.Allocate
        (Number_of_Pages, Allocation_Result, Result);
   end Allocate_Pages;

   procedure Cleanup_Stopped_Processes (Result : out Function_Result) is
      Curr_Process : Process_Control_Block_Access := null;
      Prev_Process : Process_Control_Block_Access := null;

      Logging_Tags : constant Log_Tags := [Log_Tag_Idle];
   begin
      Log_Debug ("Cleaning up stopped processes...", Logging_Tags);

      Curr_Process := Current_System_State.Processes;
      if Curr_Process = null then
         Log_Debug ("No processes to clean up.", Logging_Tags);

         Panic ("STOP HERE FOR NOW.");
      end if;

      while Curr_Process /= null loop
         if Curr_Process.all.Status = Process_Stopped then
            Deallocate_Process (Curr_Process.all, Result);
            if Is_Error (Result) then
               return;
            end if;

            if Prev_Process = null then
               Log_Debug ("Cleaning up process at list head...", Logging_Tags);
               Current_System_State.Processes := Curr_Process.all.Next_Process;
            else
               Log_Debug ("Cleaning up process...", Logging_Tags);
               Prev_Process.all.Next_Process := Curr_Process.all.Next_Process;
            end if;
         else
            --  Only update the 'previous process' if the current one was not
            --  cleared, otherwise we'll break the linked list.
            Prev_Process := Curr_Process;
         end if;

         Curr_Process := Curr_Process.all.Next_Process;
      end loop;

      Result := Success;
   end Cleanup_Stopped_Processes;

   procedure Create_New_Process
     (New_Process : out Process_Control_Block_Access;
      Result      : out Function_Result)
   is
      Allocation_Result : Memory_Allocation_Result;
   begin
      Allocate_Kernel_Physical_Memory
        (Process_Control_Block_T'Size / 8, Allocation_Result, Result);
      if Is_Error (Result) then
         Log_Error ("Failed to allocate process memory");
         return;
      end if;

      New_Process :=
        Convert_Address_To_Process_Control_Block_Access
          (Allocation_Result.Virtual_Address);

      Allocate_Process_Id (New_Process.all.Process_Id, Result);
      --  Error already printed.
      if Is_Error (Result) then
         return;
      end if;

      Log_Debug
        ("Allocated new process: "
         & ASCII.LF
         & "  PID# "
         & New_Process.all.Process_Id'Image
         & ASCII.LF
         & "  Addr: "
         & Allocation_Result.Virtual_Address'Image,
         [Log_Tag_Processes]);

      Allocate_And_Map_New_Process_Memory (New_Process.all, Result);
      if Is_Error (Result) then
         Log_Error ("Failed to initialise process");
         return;
      end if;

      Copy_Canonical_Kernel_Memory_Mappings_Into_Address_Space
        (New_Process.all.Memory_Space, Result);
      if Is_Error (Result) then
         Log_Error ("Failed to copy kernel memory mappings");
         return;
      end if;

      New_Process.all.Memory_Space.Address_Space_ID :=
        Unsigned_16 (New_Process.all.Process_Id);

      New_Process.all.Kernel_Context (sp) :=
        Address_To_Unsigned_64
          (New_Process.all.Kernel_Stack_Virt_Addr
           + New_Process.all.Kernel_Stack_Size);

      --  The userspace stack pointer is set when the process is first
      --  scheduled, and control is passed to userspace.

      --  When the process is first scheduled, control will 'return' to this
      --  function once the scheduler procedure returns.
      New_Process.all.Kernel_Context (ra) :=
        Address_To_Unsigned_64 (Scheduler.Process_Start'Address);

      New_Process.all.Status := Process_Ready;

   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Create_New_Process");
         Result := Constraint_Exception;
   end Create_New_Process;

   procedure Find_File_Handle
     (Process_Id     : Process_Id_T;
      File_Handle_Id : Unsigned_64;
      File_Handle    : out Process_File_Handle_Access;
      Result         : out Function_Result) is
   begin
      for File_Handle_Entry of Current_System_State.Open_Files loop
         if File_Handle_Entry.Entry_Used
           and then File_Handle_Entry.File_Handle_Id = File_Handle_Id
           and then File_Handle_Entry.Process_Id = Process_Id
         then
            File_Handle := File_Handle_Entry'Access;
            Result := Success;
            return;
         end if;
      end loop;

      Result := Not_Found;
   end Find_File_Handle;

   procedure Free_Kernel_Memory
     (Allocated_Virtual_Address : Virtual_Address_T;
      Result                    : out Function_Result) is
   begin
      Current_System_State.Kernel_Heap.Free
        (Allocated_Virtual_Address, Result);
   end Free_Kernel_Memory;

   procedure Free_Pages
     (Virtual_Address : Virtual_Address_T;
      Page_Count      : Positive;
      Result          : out Function_Result) is
   begin
      Current_System_State.Kernel_Page_Pool.Free
        (Page_Count, Virtual_Address, Result);
   end Free_Pages;

   function Get_Current_Hart_Supervisor_Interrupt_Context return Integer is
   begin
      --  This is the supervisor interrupt context for the current hart.
      --  Each Hart has two contexts, one for machine mode, and one for
      --  supervisor mode.
      --  Refer to: https://stackoverflow.com/a/73171828/5931673
      return ((Get_Current_Hart_Id * 2) + 1);
   end Get_Current_Hart_Supervisor_Interrupt_Context;

   function Get_Current_Hart_State return Hart_State_Access is
   begin
      return Hart_States (Get_Current_Hart_Id)'Access;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Get_Current_Hart_State");
         return null;
   end Get_Current_Hart_State;

   function Get_Process_Running_On_Current_Hart
      return Process_Control_Block_Access
   is
      Current_Hart_State : Hart_State_Access := null;
   begin
      Current_Hart_State := Get_Current_Hart_State;

      if Current_Hart_State = null then
         Log_Error
           ("No current hart state in Get_Process_Running_On_Current_Hart");
         return null;
      end if;

      return Current_Hart_State.all.Current_Process;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Get_Process_Running_On_Current_Hart");
         return null;
   end Get_Process_Running_On_Current_Hart;

   procedure Idle is
      Logging_Tags : constant Log_Tags := [Log_Tag_Idle];
      Result       : Function_Result := Unset;
   begin
      loop
         Log_Debug ("System Idle.", Logging_Tags);
         RISCV.Interrupts.Disable_Supervisor_Interrupts;

         Cleanup_Stopped_Processes (Result);
         if Is_Error (Result) then
            Panic;
         end if;

         RISCV.Interrupts.Enable_Supervisor_Interrupts;
      end loop;
   end Idle;

   procedure Panic (Message : String := "Kernel Panic") is
   begin
      Log_Error ("Panic: " & Message);

      RISCV.Interrupts.Disable_Supervisor_Interrupts;

      --  @TODO: What should happen here?
      loop
         null;
      end loop;
   exception
      when Constraint_Error =>
         loop
            null;
         end loop;
   end Panic;

   procedure Pop_Interrupts_Off is
      Current_Hart_State : Hart_State_Access := null;
   begin
      Current_Hart_State := Get_Current_Hart_State;
      if Current_Hart_State = null then
         Panic ("Pop_Interrupts_Off: No current hart state");
      end if;

      if Current_Hart_State.all.Interrupts_Off_Counter = 0 then
         Panic ("Mismatched Push/Pop_Interrupts_Off calls");
      end if;

      Current_Hart_State.all.Interrupts_Off_Counter :=
        Current_Hart_State.all.Interrupts_Off_Counter - 1;

      if Current_Hart_State.all.Interrupts_Off_Counter = 0
        and then Current_Hart_State.all.Were_Interrupts_Enabled_Before_Push_Off
      then
         RISCV.Interrupts.Enable_Supervisor_Interrupts;
      end if;
   exception
      when Constraint_Error =>
         Panic ("Constraint_Error: Pop_Interrupts_Off");
   end Pop_Interrupts_Off;

   procedure Push_Interrupts_Off is
      Current_Hart_State : Hart_State_Access := null;
   begin
      Current_Hart_State := Get_Current_Hart_State;
      if Current_Hart_State = null then
         Panic ("Push_Interrupts_Off: No current hart state");
      end if;

      if Current_Hart_State.all.Interrupts_Off_Counter = 0 then
         Current_Hart_State.all.Were_Interrupts_Enabled_Before_Push_Off :=
           RISCV.Interrupts.Are_Supervisor_Interrupts_Enabled;
      end if;

      RISCV.Interrupts.Disable_Supervisor_Interrupts;

      Current_Hart_State.all.Interrupts_Off_Counter :=
        Current_Hart_State.all.Interrupts_Off_Counter + 1;
   exception
      when Constraint_Error =>
         Panic ("Constraint_Error: Push_Interrupts_Off");
   end Push_Interrupts_Off;

end System_State;
