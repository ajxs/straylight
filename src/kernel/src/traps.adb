-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with System;                  use System;
with System.Storage_Elements; use System.Storage_Elements;

with Devices;          use Devices;
with Devices.VirtIO;
with Devices.PLIC;
with Devices.UART;
with Function_Results; use Function_Results;
with Processes.Scheduler;
with RISCV;
with RISCV.SBI;        use RISCV.SBI;
with System_Calls;
with Hart_State;       use Hart_State;
with Utilities;        use Utilities;

package body Traps is
   procedure Handle_Supervisor_Mode_Exception
     (Trapping_Process_Addr : Virtual_Address_T;
      Cause                 : Unsigned_64;
      Stval                 : Unsigned_64) is
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
         is ((Cause = 13 or else Cause = 15)
             and then
               Stval
               < Address_To_Unsigned_64
                   (Trapping_Process.Kernel_Stack_Virt_Addr));
      begin
         if Is_Stack_Overflow then
            Panic
              ("Traps.Handle_Supervisor_Mode_Exception: Stack Overflow: "
               & ASCII.LF
               & "  Stval:      "
               & Stval'Image
               & ASCII.LF
               & "  Stack Base: "
               & Trapping_Process.Kernel_Stack_Virt_Addr'Image);
         end if;
      end Read_Process_Info;

      Panic
        ("Traps.Handle_Supervisor_Mode_Exception: "
         & "Supervisor Mode: Exception");

   exception
      when Constraint_Error =>
         Panic ("Constraint_Error: Handle_Supervisor_Mode_Exception");
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

      Log_Debug
        ("External Interrupt: "
         & ASCII.LF
         & "  Context:      "
         & Supervisor_Interrupt_Context'Image
         & ASCII.LF
         & "  Interrupt ID: "
         & Interrupt_ID'Image,
         Logging_Tags);

      if Interrupt_ID = 0 then
         Log_Error ("Unable to claim interrupt: Interrupt ID is zero");
         return;
      end if;

      for System_Device of System_Devices loop
         if System_Device.Record_Used
           and then System_Device.Interrupt_Line = Integer (Interrupt_ID)
         then
            Log_Debug
              ("Handling Interrupt for Device at Virtual Address: "
               & System_Device.Virtual_Address'Image,
               Logging_Tags);

            if System_Device.Device_Bus = Device_Bus_VirtIO_MMIO then
               Devices.VirtIO.Acknowledge_Interrupt (System_Device, Result);
            elsif System_Device.Device_Class = Device_Class_Serial then
               Devices.UART.Acknowledge_Interrupt (System_Device, Result);
            else
               Log_Error ("Unable to handle interrupt for unknown device.");
               Result := Success;
            end if;

            if Is_Error (Result) then
               Panic ("Error acknowledging device interrupt: " & Result'Image);
            end if;
         end if;
      end loop;

      Devices.PLIC.Complete_Supervisor_Interrupt
        (PLIC_Device, Supervisor_Interrupt_Context, Interrupt_ID);
   exception
      when Constraint_Error =>
         Panic ("Constraint_Error: Handle_External_Interrupt");
   end Handle_External_Interrupt;

   procedure Handle_Supervisor_Mode_Interrupt
     (Cause : Unsigned_64; Sepc : Virtual_Address_T; Stval : Unsigned_64) is
   begin
      pragma Unreferenced (Sepc, Stval);

      case Cause is
         when 5      =>
            Handle_Timer_Interrupt;

         when 9      =>
            Handle_External_Interrupt;

         when others =>
            Log_Debug
              ("Supervisor Mode: Other Interrupt: " & Cause'Image,
               Logging_Tags);
      end case;
   exception
      when Constraint_Error =>
         Panic ("Constraint_Error: Handle_Supervisor_Mode_Interrupt");
   end Handle_Supervisor_Mode_Interrupt;

   procedure Handle_Supervisor_Mode_Trap
     (Trapping_Process_Addr : Virtual_Address_T;
      Scause                : Unsigned_64;
      Sepc                  : Virtual_Address_T;
      Stval                 : Unsigned_64)
   is
      Hart_Id : constant Hart_Index_T := Get_Current_Hart_Id;

      Cause : constant Unsigned_64 := Get_Cause (Scause);

      Trap_Is_Interrupt : constant Boolean := Is_Interrupt (Scause);

      Process_Id_String : String (1 .. 24) := "None                    ";
   begin
      if Trapping_Process_Addr /= Null_Address then
         Get_Process_Id : declare
            Trapping_Process : Process_Control_Block_T
            with Import, Alignment => 1, Address => Trapping_Process_Addr;
         begin
            Set_Fixed_Length_String
              (Trapping_Process.Process_Id'Image, Process_Id_String);
         end Get_Process_Id;
      end if;

      Log_Debug
        ("Traps.Handle_Supervisor_Mode_Trap:"
         & ASCII.LF
         & "  Hart#      "
         & Hart_Id'Image
         & ASCII.LF
         & "  PID#       "
         & Process_Id_String
         & ASCII.LF
         & "  Interrupt: "
         & Trap_Is_Interrupt'Image
         & ASCII.LF
         & "  Cause:     "
         & Cause'Image
         & ASCII.LF
         & "  Sepc:      "
         & Sepc'Image
         & ASCII.LF
         & "  Stval:     "
         & Stval'Image,
         Logging_Tags);

      if Trap_Is_Interrupt then
         Handle_Supervisor_Mode_Interrupt (Cause, Sepc, Stval);
      else
         Handle_Supervisor_Mode_Exception
           (Trapping_Process_Addr, Cause, Stval);
      end if;

      Log_Debug
        ("Traps.Handle_Supervisor_Mode_Trap: Returning from trap:"
         & ASCII.LF
         & "  Hart#  "
         & Hart_Id'Image
         & ASCII.LF
         & "  PID#   "
         & Process_Id_String
         & ASCII.LF
         & "  Cause: "
         & Get_Cause (Scause)'Image,
         Logging_Tags);

   exception
      when Constraint_Error =>
         Panic ("Constraint_Error: Handle_Supervisor_Mode_Trap");
   end Handle_Supervisor_Mode_Trap;

   procedure Handle_Timer_Interrupt is
      Hart_Id : constant Hart_Index_T := Get_Current_Hart_Id;
   begin
      Setup_Next_Timer_Interrupt;

      Log_Debug
        ("Hart#" & Hart_Id'Image & ": Scheduling from timer IRQ",
         Logging_Tags);

      Scheduler.Run (Process_Ready);

      Log_Debug
        ("Hart#" & Hart_Id'Image & ": Returning from timer IRQ", Logging_Tags);
   exception
      when Constraint_Error =>
         Panic ("Constraint_Error: Handle_Timer_Interrupt");
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
         when 8      =>
            System_Calls.Handle_User_Mode_Syscall (Trapping_Process, Result);
            if Is_Error (Result) then
               Panic ("Error handling syscall: " & Result'Image);
            end if;

            --  Since Sepc points to the address at the time of the trap,
            --  this points to the instruction that raised the system call.
            --  Increment Sepc to the next instruction to advance past it.
            --  This is safe to do because there's no compressed version of
            --  the ecall instruction. It's always 4 bytes long.
            Trap_Context.Sepc := Sepc + 4;

         when others =>
            Panic
              ("Unhandled user mode exception: "
               & ASCII.LF
               & "  Cause: "
               & Cause'Image
               & ASCII.LF
               & "  Sepc: "
               & Sepc'Image
               & ASCII.LF
               & "  Stval: "
               & Stval'Image);
      end case;
   exception
      when Constraint_Error =>
         Panic ("Constraint_Error: Handle_User_Mode_Exception");
   end Handle_User_Mode_Exception;

   procedure Handle_User_Mode_Interrupt (Cause : Unsigned_64) is
   begin
      case Cause is
         when 5      =>
            Handle_Timer_Interrupt;

         when 9      =>
            Handle_External_Interrupt;

         when others =>
            Log_Debug
              ("User Mode: Other Interrupt: " & Cause'Image, Logging_Tags);
      end case;
   exception
      when Constraint_Error =>
         Panic ("Constraint_Error: Handle_User_Mode_Interrupt");
   end Handle_User_Mode_Interrupt;

   procedure Handle_User_Mode_Trap
     (Trapping_Process_Addr : Virtual_Address_T;
      Scause                : Unsigned_64;
      Sepc                  : Virtual_Address_T;
      Stval                 : Unsigned_64)
   is
      Hart_Id : constant Hart_Index_T := Get_Current_Hart_Id;

      Cause : constant Unsigned_64 := Get_Cause (Scause);

      Trap_Is_Interrupt : constant Boolean := Is_Interrupt (Scause);
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
         Log_Debug
           ("Traps.Handle_User_Mode_Trap:"
            & ASCII.LF
            & "  Hart#      "
            & Hart_Id'Image
            & ASCII.LF
            & "  Interrupt: "
            & Trap_Is_Interrupt'Image
            & ASCII.LF
            & "  Cause:     "
            & Cause'Image
            & ASCII.LF
            & "  PID#       "
            & Trapping_Process.Process_Id'Image
            & ASCII.LF
            & "  Sepc:      "
            & Sepc'Image
            & ASCII.LF
            & "  Stval:     "
            & Stval'Image,
            Logging_Tags);

         if Trap_Is_Interrupt then
            Handle_User_Mode_Interrupt (Cause);
         else
            Handle_User_Mode_Exception (Trapping_Process, Cause, Sepc, Stval);
         end if;

         Log_Debug
           ("Traps.Handle_User_Mode_Trap: Returning from trap"
            & ASCII.LF
            & "  Hart#  "
            & Hart_Id'Image
            & ASCII.LF
            & "  PID#   "
            & Trapping_Process.Process_Id'Image,
            Logging_Tags);
      end Read_Process_Info;
   exception
      when Constraint_Error =>
         Panic ("Constraint_Error: Handle_User_Mode_Trap");
   end Handle_User_Mode_Trap;

   procedure Setup_Next_Timer_Interrupt is
      SBI_Ret : SBI_Result_T;
   begin
      SBI_Ret :=
        RISCV.SBI.Set_Timer (RISCV.Get_System_Time + System_Tick_Interval);
      if SBI_Ret.Error /= 0 then
         Log_Error ("Error setting next event time: " & SBI_Ret.Error'Image);
      end if;

   exception
      when Constraint_Error =>
         Panic ("Constraint_Error: Setup_Next_Timer_Interrupt");
   end Setup_Next_Timer_Interrupt;

end Traps;
