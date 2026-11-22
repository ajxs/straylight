-------------------------------------------------------------------------------
--  Copyright (c) 2026, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

package body Host_Stubs is
   procedure Enter_New_Process
     (Process_Address : Memory.Virtual_Address_T;
      Sepc            : Memory.Virtual_Address_T;
      Stack_Pointer   : Memory.Virtual_Address_T)
   is
      pragma Unreferenced (Process_Address, Sepc, Stack_Pointer);
   begin
      raise Program_Error with "Processes can't run on the host";
   end Enter_New_Process;

   procedure Load_Kernel_Context
     (SATP        : Interfaces.Unsigned_64;
      ASID        : Interfaces.Unsigned_16;
      New_Process : Processes.Process_Control_Block_T)
   is
      pragma Unreferenced (SATP, ASID, New_Process);
   begin
      raise Program_Error with "Processes can't run on the host";
   end Load_Kernel_Context;

   procedure Switch_Kernel_Context
     (SATP                : Interfaces.Unsigned_64;
      ASID                : Interfaces.Unsigned_16;
      New_Process         : Processes.Process_Control_Block_T;
      Old_Process_Context : in out Processes.Kernel_Context_T)
   is
      pragma Unreferenced (SATP, ASID, New_Process, Old_Process_Context);
   begin
      raise Program_Error with "Processes can't run on the host";
   end Switch_Kernel_Context;

   procedure Fence is
   begin
      null;
   end Fence;

   procedure Disable_Supervisor_Interrupts is
   begin
      null;
   end Disable_Supervisor_Interrupts;

   procedure Enable_Supervisor_Interrupts is
   begin
      null;
   end Enable_Supervisor_Interrupts;

   function System_Reset
     (Reset_Type   : Interfaces.Unsigned_32;
      Reset_Reason : Interfaces.Unsigned_32) return RISCV.SBI.SBI_Result_T
   is
      pragma Unreferenced (Reset_Type, Reset_Reason);
   begin
      raise Program_Error with "SBI calls aren't available on the host";
      return (Error => 0, Value => 0);
   end System_Reset;

end Host_Stubs;
