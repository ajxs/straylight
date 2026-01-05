-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Hart_State;        use Hart_State;
with Memory.Allocators; use Memory.Allocators;
with Memory.Kernel;     use Memory.Kernel;
with Memory.Physical;   use Memory.Physical;
with RISCV.Interrupts;

package body Processes is
   procedure Allocate_And_Map_New_Process_Memory
     (New_Process : out Process_Control_Block_T; Result : out Function_Result)
   is
   begin
      Log_Debug ("Allocating new process resources...", Logging_Tags);

      Create_New_Process_Memory_Space (New_Process.Memory_Space, Result);
      if Is_Error (Result) then
         return;
      end if;

      Allocate_And_Map_New_Process_Stack (New_Process, Result);
      if Is_Error (Result) then
         return;
      end if;

      Allocate_And_Map_New_Process_Kernel_Stack (New_Process, Result);
      if Is_Error (Result) then
         return;
      end if;

      Allocate_And_Map_New_Process_Heap (New_Process, Result);
      if Is_Error (Result) then
         return;
      end if;
   end Allocate_And_Map_New_Process_Memory;

   procedure Allocate_And_Map_New_Process_Heap
     (New_Process : out Process_Control_Block_T; Result : out Function_Result)
   is
      Process_Heap_Physical_Address : Physical_Address_T :=
        Null_Physical_Address;
   begin
      Log_Debug ("Allocating process heap physical memory...", Logging_Tags);

      Allocate_Physical_Memory
        (Process_Heap_Starting_Size, Process_Heap_Physical_Address, Result);
      if Is_Error (Result) then
         Log_Error ("Error allocating new process heap: " & Result'Image);
         Result := Unhandled_Exception;
         return;
      end if;

      Log_Debug
        ("Allocated process heap physical memory: "
         & Process_Heap_Physical_Address'Image,
         Logging_Tags);

      New_Process.Heap.Add_Memory_Region_To_Heap
        (Process_Heap_Virtual_Address,
         Process_Heap_Physical_Address,
         Process_Heap_Starting_Size,
         Result);
      if Is_Error (Result) then
         Log_Error ("Error initialising process heap: " & Result'Image);
         Result := Unhandled_Exception;
         return;
      end if;

      New_Process.Memory_Space.Map
        (Process_Heap_Virtual_Address,
         Process_Heap_Physical_Address,
         Process_Heap_Starting_Size,
         (True, True, False, True),
         Result);
      if Is_Error (Result) then
         Log_Error ("Error mapping process heap: " & Result'Image);
         Result := Unhandled_Exception;
         return;
      end if;

      Log_Debug ("Allocated new process heap.", Logging_Tags);
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Allocate_And_Map_New_Process_Heap");
         Result := Constraint_Exception;
   end Allocate_And_Map_New_Process_Heap;

   procedure Allocate_And_Map_New_Process_Stack
     (New_Process : out Process_Control_Block_T; Result : out Function_Result)
   is
   begin
      Log_Debug ("Allocating process stack physical memory...", Logging_Tags);

      Allocate_Physical_Memory
        (Process_Stack_Starting_Size, New_Process.Stack_Phys_Addr, Result);
      if Is_Error (Result) then
         Log_Error
           ("Error allocating process stack physical memory: " & Result'Image);
         Result := Unhandled_Exception;
         return;
      end if;

      New_Process.Stack_Size := Process_Stack_Starting_Size;

      New_Process.Memory_Space.Map
        (Process_Stack_Virtual_Address,
         New_Process.Stack_Phys_Addr,
         Process_Stack_Starting_Size,
         (True, True, False, True),
         Result);
      if Is_Error (Result) then
         Log_Error ("Error mapping process stack: " & Result'Image);
         Result := Unhandled_Exception;
         return;
      end if;

      Log_Debug
        ("Allocated process stack:"
         & ASCII.LF
         & "  Virtual address: "
         & Process_Stack_Virtual_Address'Image
         & ASCII.LF
         & "  Physical address: "
         & New_Process.Stack_Phys_Addr'Image,
         Logging_Tags);
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Allocate_And_Map_New_Process_Stack");
         Result := Constraint_Exception;
   end Allocate_And_Map_New_Process_Stack;

   procedure Allocate_And_Map_New_Process_Kernel_Stack
     (New_Process : in out Process_Control_Block_T;
      Result      : out Function_Result)
   is
      Kernel_Stack_Phys_Address : Physical_Address_T := Null_Physical_Address;
   begin
      Log_Debug
        ("Allocating process kernel stack physical memory...", Logging_Tags);

      Allocate_Physical_Memory
        (Process_Kernel_Stack_Size, Kernel_Stack_Phys_Address, Result);
      if Is_Error (Result) then
         Log_Error
           ("Error allocating new process kernel stack physical memory: "
            & Result'Image);
         Result := Unhandled_Exception;
         return;
      end if;

      New_Process.Kernel_Stack_Phys_Addr := Kernel_Stack_Phys_Address;
      New_Process.Kernel_Stack_Size := Process_Kernel_Stack_Size;

      Get_Process_Kernel_Stack_Virtual_Address
        (New_Process.Process_Id, New_Process.Kernel_Stack_Virt_Addr, Result);

      --  Map the kernel stack into the kernel address space.
      --  It's important that this is mapped into the kernel address space,
      --  not the process' address space, because the kernel stack needs to be
      --  accessible before the process' page tables are switched in.
      Map_Kernel_Memory
        (New_Process.Kernel_Stack_Virt_Addr,
         Kernel_Stack_Phys_Address,
         Process_Kernel_Stack_Size,
         (True, True, False, True),
         Result);
      if Is_Error (Result) then
         Log_Error ("Error mapping process kernel stack: " & Result'Image);
         Result := Unhandled_Exception;
         return;
      end if;

      Log_Debug
        ("Allocated and mapped process kernel stack: "
         & New_Process.Kernel_Stack_Virt_Addr'Image,
         Logging_Tags);

   exception
      when Constraint_Error =>
         Log_Error
           ("Constraint_Error: Allocate_And_Map_New_Process_Kernel_Stack");
         Result := Constraint_Exception;
   end Allocate_And_Map_New_Process_Kernel_Stack;

   --  @TODO: Deallocate process id on error.
   procedure Allocate_Process_Id
     (New_Id : out Process_Id_T; Result : out Function_Result) is
   begin
      Acquire_Spinlock (Process_Id_Spinlock);

      New_Id := Next_Process_Id;
      Next_Process_Id := Next_Process_Id + 1;

      Log_Debug ("Allocated new process id:" & New_Id'Image, Logging_Tags);

      Result := Success;

      Release_Spinlock (Process_Id_Spinlock);
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Allocate_Process_Id");
         New_Id := 0;
         Result := Maximum_Process_Count_Reached;
         Release_Spinlock (Process_Id_Spinlock);
   end Allocate_Process_Id;

   procedure Deallocate_Process
     (Process : in out Process_Control_Block_T; Result : out Function_Result)
   is
   begin
      Log_Debug
        ("Deallocating process PID#" & Process.Process_Id'Image, Logging_Tags);

      Deallocate_Process_Resources (Process, Result);
      if Is_Error (Result) then
         return;
      end if;

      Log_Debug
        ("Deallocated process PID#" & Process.Process_Id'Image, Logging_Tags);

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Deallocate_Process");
         Result := Constraint_Exception;
   end Deallocate_Process;

   procedure Deallocate_Process_Heap
     (Process : in out Process_Control_Block_T; Result : out Function_Result)
   is
   begin
      Log_Debug ("Freeing process heap physical memory...", Logging_Tags);

      for Heap_Region of Process.Heap.Memory_Regions loop
         if Heap_Region.Entry_Used then
            Log_Debug
              ("Freeing process heap region physical memory with address: "
               & Heap_Region.Physical_Address'Image,
               Logging_Tags);

            Free_Physical_Memory (Heap_Region.Physical_Address, Result);
            if Is_Error (Result) then
               return;
            end if;

            Log_Debug ("Freed process heap region.", Logging_Tags);
         end if;
      end loop;

      Log_Debug ("Freed process heap physical memory.", Logging_Tags);

   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Deallocate_Process_Heap");
         Result := Constraint_Exception;
   end Deallocate_Process_Heap;

   procedure Deallocate_Process_Resources
     (Process : in out Process_Control_Block_T; Result : out Function_Result)
   is
   begin
      Log_Debug ("Freeing process stack physical memory...", Logging_Tags);
      Free_Physical_Memory (Process.Stack_Phys_Addr, Result);
      if Is_Error (Result) then
         return;
      end if;

      Log_Debug ("Freed process stack physical memory.", Logging_Tags);

      Log_Debug
        ("Freeing process kernel stack physical memory.", Logging_Tags);
      Free_Physical_Memory (Process.Kernel_Stack_Phys_Addr, Result);
      if Is_Error (Result) then
         return;
      end if;

      Deallocate_Process_Heap (Process, Result);
      if Is_Error (Result) then
         return;
      end if;

      Log_Debug ("Deallocating process address space...", Logging_Tags);

      Deallocate_Memory_Space (Process.Memory_Space, Result);
      if Is_Error (Result) then
         return;
      end if;

      Log_Debug ("Deallocated process address space.", Logging_Tags);

      Result := Success;
   end Deallocate_Process_Resources;

   procedure Exit_Process
     (Process : in out Process_Control_Block_T; Result : out Function_Result)
   is
   begin
      Log_Debug
        ("Exiting process PID#" & Process.Process_Id'Image, Logging_Tags);

      Process.Status := Process_Stopped;

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Exit_Process");
         Result := Constraint_Exception;
   end Exit_Process;

   function Find_Running_Process_With_Id
     (Process_Id : Process_Id_T) return Process_Control_Block_Access
   is
      Current_Process : Process_Control_Block_Access := Process_Queue;
   begin
      while Current_Process /= null loop
         if Current_Process.all.Process_Id = Process_Id then
            return Current_Process;
         end if;

         Current_Process := Current_Process.all.Next_Process;
      end loop;

      return null;
   end Find_Running_Process_With_Id;

   pragma
     Warnings
       (Off, "pragma Restrictions (No_Exception_Propagation) in effect");
   procedure Get_Process_Kernel_Stack_Virtual_Address
     (Process_Id             : Process_Id_T;
      Kernel_Stack_Virt_Addr : out Virtual_Address_T;
      Result                 : out Function_Result) is
   begin
      --  This calculation leaves a single unmapped guard page between
      --  each process' kernel stack.
      Kernel_Stack_Virt_Addr :=
        Kernel_Stack_Area_Virtual_Address
        + Storage_Offset (Process_Id * (Process_Kernel_Stack_Size + 16#1000#))
        + 16#1000#;

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error
           ("Constraint_Error: Get_Process_Kernel_Stack_Virtual_Address");
         Result := Constraint_Exception;
   end Get_Process_Kernel_Stack_Virtual_Address;
   pragma
     Warnings (On, "pragma Restrictions (No_Exception_Propagation) in effect");

   procedure Add_Process
     (New_Process : Process_Control_Block_Access; Result : out Function_Result)
   is
      Curr_Process : Process_Control_Block_Access := null;
      Prev_Process : Process_Control_Block_Access := null;
   begin
      Log_Debug
        ("Adding process id "
         & New_Process.all.Process_Id'Image
         & ", address "
         & New_Process.all'Address'Image,
         Logging_Tags);
      if Process_Queue = null then
         Log_Debug
           ("No processes in list, setting new process as head", Logging_Tags);
         Process_Queue := New_Process;
         Result := Success;
         return;
      end if;

      Curr_Process := Process_Queue;
      while Curr_Process /= null loop
         Prev_Process := Curr_Process;
         Curr_Process := Curr_Process.all.Next_Process;
      end loop;

      Prev_Process.all.Next_Process := New_Process;

      Log_Debug ("Added process to end of list", Logging_Tags);
      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Add_Process");
         Result := Constraint_Exception;
   end Add_Process;

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
        Address_To_Unsigned_64 (Process_Start'Address);

      New_Process.all.Status := Process_Ready;

   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Create_New_Process");
         Result := Constraint_Exception;
   end Create_New_Process;

   procedure Cleanup_Stopped_Processes (Result : out Function_Result) is
      Curr_Process : Process_Control_Block_Access := null;
      Prev_Process : Process_Control_Block_Access := null;

      Logging_Tags : constant Log_Tags := [Log_Tag_Idle];
   begin
      Log_Debug ("Cleaning up stopped processes...", Logging_Tags);

      Curr_Process := Process_Queue;
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
               Process_Queue := Curr_Process.all.Next_Process;
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

         --  It's important to explicitly re-enable interrupts here, rather
         --  than using the push/pop interrupt state functions, as the
         --  interrupts were previously disabled when entering the scheduler.
         --  Otherwise the kernel would get stuck in the idle loop.
         RISCV.Interrupts.Enable_Supervisor_Interrupts;
      end loop;
   end Idle;

   procedure Process_Start is
      procedure Enter_New_Process
        (Process_Address : Virtual_Address_T;
         Sepc            : Virtual_Address_T;
         Stack_Pointer   : Virtual_Address_T)
      with
        No_Return,
        Import,
        Convention    => Assembler,
        External_Name => "processes_enter_new_process";
   begin
      Curr_Process : constant Process_Control_Block_Access :=
        Get_Process_Running_On_Current_Hart;

      if Curr_Process = null then
         Panic ("Process_Start: No current process.");
      end if;

      Log_Debug
        ("New process starting with PID#" & Curr_Process.all.Process_Id'Image,
         Logging_Tags);

      --  Run the next process.
      Enter_New_Process
        (Curr_Process.all'Address,
         Curr_Process.all.Process_Entry_Point,
         Process_Stack_Virtual_Address + Curr_Process.all.Stack_Size);
   exception
      when others =>
         Panic ("Constraint_Error: Process_Start");
   end Process_Start;

   procedure Initialise_Hart_Idle_Process (Hart_Id : Integer) is
      Result : Function_Result := Unset;
   begin
      Create_New_Process (Hart_Idle_Processes (Hart_Id), Result);
      if Is_Error (Result) then
         --  Error already printed.
         Panic;
      end if;

      --  Set the idle process's return address to the idle function.
      --  When the scheduler switches to the idle process, it will 'return'
      --  to the idle function address.
      Hart_Idle_Processes (Hart_Id).all.Kernel_Context (ra) :=
        Address_To_Unsigned_64 (Idle'Address);
   exception
      when Constraint_Error =>
         Panic ("Constraint_Error: Initialise_Hart_Idle_Process");
   end Initialise_Hart_Idle_Process;

end Processes;
