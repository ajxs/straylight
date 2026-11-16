-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------
limited with Devices.Attached_System_Devices;

with Function_Results; use Function_Results;
with Hart_State;

package Devices.PLIC
  with Preelaborate
is
   --  The number of interrupt sources on QEMU's virt PLIC ('riscv,ndev').
   --  Source 0 is reserved, and means that no interrupt is pending.
   Maximum_Interrupt_Line : constant := 95;

   subtype Interrupt_Line_T is Unsigned_32 range 1 .. Maximum_Interrupt_Line;

   type Interrupt_Device_Array_T is
     array (Interrupt_Line_T)
     of access Devices.Attached_System_Devices.Device_T;

   type Hart_Context_Array_T is array (Hart_State.Hart_Index_T) of Natural;

   type PLIC_Device_State_T is record
      --  The device that raises each interrupt line, so that a claimed
      --  interrupt can be dispatched to its device.
      Interrupt_Devices : Interrupt_Device_Array_T := [others => null];

      --  The context that receives each hart's supervisor-mode external
      --  interrupts, from the Devicetree. A hart without one keeps 0, which
      --  is never used, since only the boot hart enables PLIC interrupts,
      --  and Boot checks that the boot hart has one.
      Supervisor_Contexts : Hart_Context_Array_T := [others => 0];
   end record;

   function Claim_Supervisor_Interrupt
     (Device : in out Device_Common_State_T; Context : Natural)
      return Unsigned_32;

   procedure Complete_Supervisor_Interrupt
     (Device       : in out Device_Common_State_T;
      Context      : Natural;
      Interrupt_ID : Unsigned_32);

   procedure Set_Interrupt_Priority
     (Device       : in out Device_Common_State_T;
      IRQ          : Natural;
      IRQ_Priority : Natural;
      Result       : out Function_Result);

   procedure Set_IRQ_Priority_Threshold
     (Device    : in out Device_Common_State_T;
      Context   : Natural;
      Threshold : Natural);

   procedure Set_IRQ_Enable_State
     (Device         : in out Device_Common_State_T;
      Context        : Natural;
      Interrupt_Line : Natural;
      State          : Boolean);

private
   function Get_Interrupt_Claim_Address
     (Device : Device_Common_State_T; Context : Natural)
      return Virtual_Address_T
   is (Device.Virtual_Address
       + 16#20_0004#
       + (Storage_Offset (Context) * 16#1000#))
   with Inline, Pure_Function;

end Devices.PLIC;
