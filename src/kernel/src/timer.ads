-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

package Timer is
   pragma Preelaborate;

   procedure Update_System_Time;

   procedure Set_Interval_For_Next_Timer_Interrupt;

private
   ----------------------------------------------------------------------------
   --  The kernel's 'tick' interval.
   --  The timebase in QEMU's RISC-V virt machine is 10_000_000 Hz,
   --  so a tick interval of 10_000 corresponds to 1 ms.
   --  The current tick rate is about 100 Hz.
   ----------------------------------------------------------------------------
   System_Tick_Interval : constant := 100_000;
end Timer;
