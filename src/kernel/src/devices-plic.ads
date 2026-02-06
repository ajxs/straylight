-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------
with Function_Results; use Function_Results;

package Devices.PLIC is
   pragma Preelaborate;

   function Claim_Supervisor_Interrupt
     (Device : Device_T; Context : Natural) return Unsigned_32;

   procedure Complete_Supervisor_Interrupt
     (Device : Device_T; Context : Natural; Interrupt_ID : Unsigned_32);

   procedure Set_Interrupt_Priority
     (Device       : Device_T;
      IRQ          : Natural;
      IRQ_Priority : Natural;
      Result       : out Function_Result);

   procedure Set_IRQ_Priority_Threshold
     (Device : Device_T; Context : Natural; Threshold : Natural);

   procedure Set_IRQ_Enable_State
     (Device         : Device_T;
      Context        : Natural;
      Interrupt_Line : Natural;
      State          : Boolean;
      Result         : out Function_Result);

private
   function Get_Interrupt_Claim_Address
     (Device : Device_T; Context : Natural) return Virtual_Address_T
   is (Device.Virtual_Address
       + 16#20_0004#
       + (Storage_Offset (Context) * 16#1000#))
   with Inline, Pure_Function;

end Devices.PLIC;
