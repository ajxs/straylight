-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

package Devices.PLIC is
   pragma Preelaborate;

   function Claim_Supervisor_Interrupt
     (Device_Address : Address; Context : Integer) return Unsigned_32;

   procedure Complete_Supervisor_Interrupt
     (Device_Address : Address; Context : Integer; Interrupt_ID : Unsigned_32);

   procedure Set_Interrupt_Priority
     (Device_Address : Address; IRQ : Integer; IRQ_Priority : Integer);

   procedure Set_IRQ_Priority_Threshold
     (Device_Address : Address; Context : Integer; Threshold : Integer);

   procedure Set_IRQ_Enable_State
     (Device_Address : Address;
      Context        : Integer;
      Interrupt_Line : Integer;
      State          : Boolean);

private
   function Get_Interrupt_Claim_Address
     (Device_Address : Virtual_Address_T; Context : Integer)
      return Virtual_Address_T
   is (Device_Address + 16#20_0004# + (Storage_Offset (Context) * 16#1000#))
   with Inline, Pure_Function;

end Devices.PLIC;
