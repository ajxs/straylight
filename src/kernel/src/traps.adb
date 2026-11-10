-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with GNAT.Debug_Utilities;
with System;                  use System;
with System.Storage_Elements; use System.Storage_Elements;

with Devices;          use Devices;
with Devices.Virtio;
with Devices.PLIC;
with Devices.UART;
with Function_Results; use Function_Results;
with Logging;          use Logging;
with Processes.Scheduler;
with RISCV;            use RISCV;
with RISCV.SBI;        use RISCV.SBI;
with System_Calls;
with Hart_State;       use Hart_State;

package body Traps is
   procedure Handle_Supervisor_Mode_Exception
     (Trapping_Process_Addr : Virtual_Address_T;
      Cause                 : Unsigned_64;
      Sepc                  : Virtual_Address_T;
      Stval                 : Unsigned_64)
   is
      Hart_Id : constant Hart_Index_T := Get_Current_Hart_Id;
   begin
      if Trapping_Process_Addr = Null_Address then
         Panic
           ("Traps.Handle_Supervisor_Mode_Exception: "
            & "Null trapping process address");
      end if;

      Read_Process_Info : declare
         Trapping_Process : Process_Control_Block_T
         with Import, Alignment => 1, Address => Trapping_Process_Addr;

         function Is_Stack_Overflow return Boolean
         is ((Cause = Scause_Load_Page_Fault
              or else Cause = Scause_Store_AMO_Page_Fault)
             and then
               Stval
               < Address_To_Unsigned_64
                   (Trapping_Process.Kernel_Stack_Virt_Addr)
             and then
               Stval
               > Address_To_Unsigned_64
                   (Trapping_Process.Kernel_Stack_Virt_Addr - 16#1000#));
      begin
         Is_Idle_Process : constant Boolean :=
           Trapping_Process.Process_Id
           = Hart_Idle_Processes (Hart_Id).all.Process_Id;

         if Is_Stack_Overflow then
            Panic
              ("Traps.Handle_Supervisor_Mode_Exception: Stack Overflow:"
               & " stval="
               & GNAT.Debug_Utilities.Image_C (Unsigned_64_To_Address (Stval))
               & " stack_base="
               & GNAT.Debug_Utilities.Image_C
                   (Trapping_Process.Kernel_Stack_Virt_Addr));
         end if;

         Panic
           ("Unhandled supervisor mode exception: pid="
            & Trapping_Process.Process_Id'Image
                (2 .. Trapping_Process.Process_Id'Image'Length)
            & " idle="
            & Is_Idle_Process'Image
            & " cause="
            & Cause'Image (2 .. Cause'Image'Length)
            & " sepc="
            & GNAT.Debug_Utilities.Image_C (Sepc)
            & " stval="
            & GNAT.Debug_Utilities.Image_C (Unsigned_64_To_Address (Stval)));

      end Read_Process_Info;

   exception
      when Constraint_Error =>
         Panic_Constraint_Error;
   end Handle_Supervisor_Mode_Exception;

   procedure Handle_External_Interrupt is
      Interrupt_ID : Unsigned_32 := 0;

      Supervisor_Interrupt_Context : constant Integer :=
        Get_Current_Hart_Supervisor_Interrupt_Context;

      PLIC_Device renames Devices.System_Devices (1);

      Result : Function_Result := Unset;
   begin
      Interrupt_ID :=
        Devices.PLIC.Claim_Supervisor_Interrupt
          (PLIC_Device, Supervisor_Interrupt_Context);

      pragma
        Debug
          (Debug_Traps,
           Log_Debug
             ("External Interrupt: context="
              & Supervisor_Interrupt_Context'Image
                  (2 .. Supervisor_Interrupt_Context'Image'Length)
              & " irq="
              & Interrupt_ID'Image (2 .. Interrupt_ID'Image'Length)));

      if Interrupt_ID = 0 then
         Log_Error ("Unable to claim interrupt: Interrupt ID is zero");
         return;
      end if;

      for System_Device of System_Devices loop
         if System_Device.Record_Used
           and then System_Device.Interrupt_Line = Integer (Interrupt_ID)
         then
            pragma
              Debug
                (Debug_Traps,
                 Log_Debug
                   ("Handling Interrupt for Device: addr_virt="
                    & GNAT.Debug_Utilities.Image_C
                        (System_Device.Virtual_Address)));

            if System_Device.Device_Bus = Device_Bus_Virtio_MMIO then
               Devices.Virtio.Acknowledge_Interrupt (System_Device, Result);
            elsif System_Device.Device_Class = Device_Class_Serial then
               Devices.UART.Acknowledge_Interrupt (System_Device, Result);
            else
               Log_Error ("Unable to handle interrupt for unknown device.");
               Result := Success;
            end if;

            if Is_Error (Result) then
               Panic
                 ("Error acknowledging device interrupt: result="
                  & Result'Image);
            end if;
         end if;
      end loop;

      Devices.PLIC.Complete_Supervisor_Interrupt
        (PLIC_Device, Supervisor_Interrupt_Context, Interrupt_ID);
   exception
      when Constraint_Error =>
         Panic_Constraint_Error;
   end Handle_External_Interrupt;

   procedure Handle_Supervisor_Mode_Interrupt
     (Cause : Unsigned_64; Sepc : Virtual_Address_T; Stval : Unsigned_64)
   is
      procedure Log_Cause is
      begin
         Log_Debug
           ("Supervisor Mode: Other Interrupt: cause="
            & Cause'Image (2 .. Cause'Image'Length));
      exception
         when Constraint_Error =>
            null;
      end Log_Cause;
   begin
      pragma Unreferenced (Sepc, Stval);

      case Cause is
         when Scause_Supervisor_Timer_Interrupt    =>
            Handle_Timer_Interrupt;

         when Scause_Supervisor_External_Interrupt =>
            Handle_External_Interrupt;

         when others                               =>
            pragma Debug (Debug_Traps, Log_Cause);
      end case;
   end Handle_Supervisor_Mode_Interrupt;

   procedure Log_Trap_Info
     (Supervisor_Mode       : Boolean;
      Trapping_Process_Addr : Virtual_Address_T;
      Scause                : Unsigned_64;
      Sepc                  : Virtual_Address_T;
      Stval                 : Unsigned_64;
      Entering              : Boolean)
   is
      Cause : constant Unsigned_64 := Get_Cause (Scause);

      Trap_Is_Interrupt : constant Boolean := Is_Interrupt (Scause);

      Process_Id : Process_Id_T := 0;
   begin
      if Trapping_Process_Addr /= Null_Address then
         Get_Process_Id : declare
            Trapping_Process : Process_Control_Block_T
            with Import, Alignment => 1, Address => Trapping_Process_Addr;
         begin
            Process_Id := Trapping_Process.Process_Id;
         end Get_Process_Id;
      end if;

      if Entering then
         Log_Debug
           ((if Supervisor_Mode
             then "Traps.Handle_Supervisor_Mode_Trap:"
             else "Traps.Handle_User_Mode_Trap:")
            & " pid="
            & (if Trapping_Process_Addr = Null_Address
               then "none"
               else Process_Id'Image (2 .. Process_Id'Image'Length))
            & " interrupt="
            & Trap_Is_Interrupt'Image
            & " cause="
            & Cause'Image (2 .. Cause'Image'Length)
            & " sepc="
            & GNAT.Debug_Utilities.Image_C (Sepc)
            & " stval="
            & GNAT.Debug_Utilities.Image_C (Unsigned_64_To_Address (Stval)));
      else
         Log_Debug
           ((if Supervisor_Mode
             then "Traps.Handle_Supervisor_Mode_Trap: Returning from trap:"
             else "Traps.Handle_User_Mode_Trap: Returning from trap:")
            & " pid="
            & (if Trapping_Process_Addr = Null_Address
               then "none"
               else Process_Id'Image (2 .. Process_Id'Image'Length))
            & " cause="
            & Cause'Image (2 .. Cause'Image'Length));
      end if;
   exception
      when Constraint_Error =>
         null;
   end Log_Trap_Info;

   procedure Handle_Supervisor_Mode_Trap
     (Trapping_Process_Addr : Virtual_Address_T;
      Scause                : Unsigned_64;
      Sepc                  : Virtual_Address_T;
      Stval                 : Unsigned_64)
   is
      Cause : constant Unsigned_64 := Get_Cause (Scause);
   begin
      pragma
        Debug
          (Debug_Traps,
           Log_Trap_Info
             (True, Trapping_Process_Addr, Scause, Sepc, Stval, True));

      if Is_Interrupt (Scause) then
         Handle_Supervisor_Mode_Interrupt (Cause, Sepc, Stval);
      else
         Handle_Supervisor_Mode_Exception
           (Trapping_Process_Addr, Cause, Sepc, Stval);
      end if;

      pragma
        Debug
          (Debug_Traps,
           Log_Trap_Info
             (True, Trapping_Process_Addr, Scause, Sepc, Stval, False));
   end Handle_Supervisor_Mode_Trap;

   procedure Log_Timer_Interrupt_Scheduling (Entering : Boolean) is
   begin
      if Entering then
         Log_Debug ("Scheduling from timer IRQ");
      else
         Log_Debug ("Returning from timer IRQ");
      end if;
   end Log_Timer_Interrupt_Scheduling;

   procedure Handle_Timer_Interrupt is
   begin
      Setup_Next_Timer_Interrupt;

      pragma Debug (Debug_Traps, Log_Timer_Interrupt_Scheduling (True));

      Scheduler.Run (Process_Ready);

      pragma Debug (Debug_Traps, Log_Timer_Interrupt_Scheduling (False));
   end Handle_Timer_Interrupt;

   procedure Handle_User_Mode_Exception
     (Trapping_Process : in out Process_Control_Block_T;
      Cause            : Unsigned_64;
      Sepc             : Virtual_Address_T;
      Stval            : Unsigned_64)
   is
      Result : Function_Result := Unset;

      Trap_Context : Process_Context_T
      with
        Import,
        Convention => C,
        Alignment  => 1,
        Address    => Trapping_Process.Trap_Context_Addr;
   begin
      case Cause is
         when Scause_U_Mode_Ecall =>
            System_Calls.Handle_User_Mode_Syscall (Trapping_Process, Result);
            if Is_Error (Result) then
               Panic ("Error handling syscall: result=" & Result'Image);
            end if;

            --  Since Sepc points to the address at the time of the trap,
            --  this points to the instruction that raised the system call.
            --  Increment Sepc to the next instruction to advance past it.
            --  This is safe to do because there's no compressed version of
            --  the ecall instruction. It's always 4 bytes long.
            Trap_Context.Sepc := Sepc + 4;

         when others              =>
            Panic
              ("Unhandled user mode exception: pid="
               & Trapping_Process.Process_Id'Image
                   (2 .. Trapping_Process.Process_Id'Image'Length)
               & " cause="
               & Cause'Image (2 .. Cause'Image'Length)
               & " sepc="
               & GNAT.Debug_Utilities.Image_C (Sepc)
               & " stval="
               & GNAT.Debug_Utilities.Image_C
                   (Unsigned_64_To_Address (Stval)));
      end case;
   exception
      when Constraint_Error =>
         Panic_Constraint_Error;
   end Handle_User_Mode_Exception;

   procedure Handle_User_Mode_Interrupt (Cause : Unsigned_64) is
      procedure Log_Cause is
      begin
         Log_Debug
           ("User Mode: Other Interrupt: cause="
            & Cause'Image (2 .. Cause'Image'Length));
      exception
         when Constraint_Error =>
            null;
      end Log_Cause;
   begin
      case Cause is
         when Scause_Supervisor_Timer_Interrupt    =>
            Handle_Timer_Interrupt;

         when Scause_Supervisor_External_Interrupt =>
            Handle_External_Interrupt;

         when others                               =>
            pragma Debug (Debug_Traps, Log_Cause);
      end case;
   end Handle_User_Mode_Interrupt;

   procedure Handle_User_Mode_Trap
     (Trapping_Process_Addr : Virtual_Address_T;
      Scause                : Unsigned_64;
      Sepc                  : Virtual_Address_T;
      Stval                 : Unsigned_64)
   is
      Cause : constant Unsigned_64 := Get_Cause (Scause);
   begin
      --  If we're in user mode, and there's no actual process running, this
      --  indicates some serious problem has occurred. In this case, panic.
      if Trapping_Process_Addr = Null_Address then
         Panic ("Traps.Handle_User_Mode_Trap: Null trapping process address");
      end if;

      Read_Process_Info : declare
         Trapping_Process : Process_Control_Block_T
         with Import, Alignment => 1, Address => Trapping_Process_Addr;
      begin
         pragma
           Debug
             (Debug_Traps,
              Log_Trap_Info
                (False, Trapping_Process_Addr, Scause, Sepc, Stval, True));

         if Is_Interrupt (Scause) then
            Handle_User_Mode_Interrupt (Cause);
         else
            Handle_User_Mode_Exception (Trapping_Process, Cause, Sepc, Stval);
         end if;

         pragma
           Debug
             (Debug_Traps,
              Log_Trap_Info
                (False, Trapping_Process_Addr, Scause, Sepc, Stval, False));
      end Read_Process_Info;
   end Handle_User_Mode_Trap;

   procedure Setup_Next_Timer_Interrupt is
   begin
      SBI_Ret : constant SBI_Result_T :=
        RISCV.SBI.Set_Timer (RISCV.Get_System_Time + System_Tick_Interval);
      if SBI_Ret.Error /= 0 then
         Panic ("Error setting next event time: error=" & SBI_Ret.Error'Image);
      end if;

   exception
      when Constraint_Error =>
         Panic_Constraint_Error;
   end Setup_Next_Timer_Interrupt;

   procedure Ensure_No_Locks_Held_In_Trap is
      Hart_Id : constant Hart_Index_T := Get_Current_Hart_Id;
   begin
      if Hart_States (Hart_Id).Interrupts_Off_Counter /= 0 then
         Report_Locks : declare
            Counter_Image : constant String :=
              Hart_States (Hart_Id).Interrupts_Off_Counter'Image;
         begin
            Panic
              ("Hart still has active locks prior to trap: interrupts_off="
               & Counter_Image (2 .. Counter_Image'Last));
         end Report_Locks;
      end if;
   exception
      when Constraint_Error =>
         Panic_Constraint_Error;
   end Ensure_No_Locks_Held_In_Trap;

end Traps;
