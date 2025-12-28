-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Memory.Physical; use Memory.Physical;

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

      Log_Debug
        ("Allocated new process id:" & New_Id'Image, [Log_Tag_Processes]);

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

   procedure Find_Process_With_Id
     (Process_Queue_Head : Process_Control_Block_Access;
      Process_Id         : Process_Id_T;
      Process            : out Process_Control_Block_Access;
      Result             : out Function_Result)
   is
      Current_Process : Process_Control_Block_Access := Process_Queue_Head;
   begin
      while Current_Process /= null loop
         if Current_Process.all.Process_Id = Process_Id then
            Process := Current_Process;
            Result := Success;
            return;
         end if;

         Current_Process := Current_Process.all.Next_Process;
      end loop;

      Process := null;
      Result := Process_Not_Found;
   end Find_Process_With_Id;

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
end Processes;
