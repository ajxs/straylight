-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

package RISCV.Interrupts is
   pragma Preelaborate;

   procedure Disable_Supervisor_Interrupts
   with
     Import,
     Convention    => Assembler,
     External_Name => "riscv_interrupts_disable_supervisor_interrupts";

   procedure Enable_Supervisor_Interrupts
   with
     Import,
     Convention    => Assembler,
     External_Name => "riscv_interrupts_enable_supervisor_interrupts";

   function Are_Supervisor_Interrupts_Enabled return Boolean
   with
     Import,
     Convention    => Assembler,
     External_Name => "riscv_interrupts_are_supervisor_interrupts_enabled";

end RISCV.Interrupts;
