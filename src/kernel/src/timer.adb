-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Interfaces; use Interfaces;

with Logging;      use Logging;
with RISCV;        use RISCV;
with RISCV.SBI;    use RISCV.SBI;
with System_State; use System_State;

package body Timer is
   procedure Set_Interval_For_Next_Timer_Interrupt is
      SBI_Ret : SBI_Result_T;
   begin
      SBI_Ret :=
        RISCV.SBI.Set_Timer
          (Current_System_State.System_Time + System_Tick_Interval);
      if SBI_Ret.Error /= 0 then
         Log_Error ("Error setting next event time: " & SBI_Ret.Error'Image);
      end if;

   exception
      when Constraint_Error =>
         null;
   end Set_Interval_For_Next_Timer_Interrupt;

   procedure Update_System_Time is
   begin
      Current_System_State.System_Time := Get_System_Time;
   end Update_System_Time;

end Timer;
