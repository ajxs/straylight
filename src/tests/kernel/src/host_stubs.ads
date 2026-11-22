-------------------------------------------------------------------------------
--  Copyright (c) 2026, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Interfaces;

with Hart_State;
with Memory;
with Processes;
with RISCV.SBI;

-------------------------------------------------------------------------------
--  The kernel implements RISC-V-specific functionality in assembler.
--  This RISC-V-specific functionality is required by a large amount of
--  kernel code, and the host stubs provide dummy implementations which are
--  compatible with the host system running the unit tests.
--  A routine that only makes sense in the target context raises Program_Error,
--  so a test that reaches one fails.
-------------------------------------------------------------------------------

package Host_Stubs is
   --  Hart state: there's one hart, number 0, with no state or process.
   function Get_Process_Running_On_Current_Hart
      return Processes.Process_Control_Block_Access
   is (null)
   with
     Export,
     Convention    => Assembler,
     External_Name => "hart_state_get_current_running_process";

   function Get_Current_Hart_Id return Hart_State.Hart_Index_T
   is (Hart_State.Hart_Index_T'First)
   with
     Export,
     Convention    => Assembler,
     External_Name => "hart_state_get_hart_id";

   function Get_Current_Hart_State return Hart_State.Hart_State_Access
   is (null)
   with
     Export,
     Convention    => Assembler,
     External_Name => "hart_state_get_hart_state";

   --  Processes and scheduling: processes can't run on the host.
   procedure Enter_New_Process
     (Process_Address : Memory.Virtual_Address_T;
      Sepc            : Memory.Virtual_Address_T;
      Stack_Pointer   : Memory.Virtual_Address_T)
   with
     No_Return,
     Export,
     Convention    => Assembler,
     External_Name => "processes_enter_new_process";

   procedure Load_Kernel_Context
     (SATP        : Interfaces.Unsigned_64;
      ASID        : Interfaces.Unsigned_16;
      New_Process : Processes.Process_Control_Block_T)
   with
     No_Return,
     Export,
     Convention    => Assembler,
     External_Name => "scheduler_load_kernel_context";

   procedure Switch_Kernel_Context
     (SATP                : Interfaces.Unsigned_64;
      ASID                : Interfaces.Unsigned_16;
      New_Process         : Processes.Process_Control_Block_T;
      Old_Process_Context : in out Processes.Kernel_Context_T)
   with
     Export,
     Convention    => Assembler,
     External_Name => "scheduler_switch_kernel_context";

   --  The CPU: interrupts are never enabled, and the clock stays at 0.
   procedure Fence
   with Export, Convention => C, External_Name => "riscv_atomics_fence";

   function Get_System_Time return Interfaces.Unsigned_64
   is (0)
   with
     Export,
     Convention    => Assembler,
     External_Name => "riscv_get_system_time";

   function Are_Supervisor_Interrupts_Enabled return Boolean
   is (False)
   with
     Export,
     Convention    => Assembler,
     External_Name => "riscv_interrupts_are_supervisor_interrupts_enabled";

   procedure Disable_Supervisor_Interrupts
   with
     Export,
     Convention    => Assembler,
     External_Name => "riscv_interrupts_disable_supervisor_interrupts";

   procedure Enable_Supervisor_Interrupts
   with
     Export,
     Convention    => Assembler,
     External_Name => "riscv_interrupts_enable_supervisor_interrupts";

   --  SBI calls: there's no firmware on the host.
   function System_Reset
     (Reset_Type   : Interfaces.Unsigned_32;
      Reset_Reason : Interfaces.Unsigned_32) return RISCV.SBI.SBI_Result_T
   with Export, Convention => Assembler, External_Name => "sbi_system_reset";

   --  The section boundaries from the kernel's linker script. Only their
   --  addresses are used, to map the kernel's sections.
   Text_Start   : constant Integer := 0
   with Export, Convention => Assembler, External_Name => "__text_start";
   Text_End     : constant Integer := 0
   with Export, Convention => Assembler, External_Name => "__text_end";
   Rodata_Start : constant Integer := 0
   with Export, Convention => Assembler, External_Name => "__rodata_start";
   Rodata_End   : constant Integer := 0
   with Export, Convention => Assembler, External_Name => "__rodata_end";
   Data_End     : constant Integer := 0
   with Export, Convention => Assembler, External_Name => "__data_end";
   Bss_End      : constant Integer := 0
   with Export, Convention => Assembler, External_Name => "__bss_end";

end Host_Stubs;
