-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Logging; use Logging;
with RISCV.Interrupts;

package body Hart_State is
   function Get_Current_Hart_Supervisor_Interrupt_Context return Natural is
   begin
      --  This is the supervisor interrupt context for the current hart.
      --  Each Hart has two contexts, one for machine mode, and one for
      --  supervisor mode.
      --  Refer to: https://stackoverflow.com/a/73171828/5931673
      return ((Get_Current_Hart_Id * 2) + 1);
   end Get_Current_Hart_Supervisor_Interrupt_Context;

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
        and then
          Current_Hart_State.all.Interrupts_Enabled_Before_Initial_Push_Off
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
         Current_Hart_State.all.Interrupts_Enabled_Before_Initial_Push_Off :=
           RISCV.Interrupts.Are_Supervisor_Interrupts_Enabled;
      end if;

      RISCV.Interrupts.Disable_Supervisor_Interrupts;

      Current_Hart_State.all.Interrupts_Off_Counter :=
        Current_Hart_State.all.Interrupts_Off_Counter + 1;
   exception
      when Constraint_Error =>
         Panic ("Constraint_Error: Push_Interrupts_Off");
   end Push_Interrupts_Off;

end Hart_State;
