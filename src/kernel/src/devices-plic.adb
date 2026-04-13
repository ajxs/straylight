-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with MMIO;

package body Devices.PLIC is
   function Claim_Supervisor_Interrupt
     (Device : in out Device_T; Context : Natural) return Unsigned_32 is
   begin
      Acquire_Spinlock (Device.Spinlock);
      Result : constant Unsigned_32 :=
        MMIO.Read_Unsigned_32 (Get_Interrupt_Claim_Address (Device, Context));
      Release_Spinlock (Device.Spinlock);

      return Result;
   end Claim_Supervisor_Interrupt;

   procedure Complete_Supervisor_Interrupt
     (Device : in out Device_T; Context : Natural; Interrupt_ID : Unsigned_32)
   is
   begin
      Acquire_Spinlock (Device.Spinlock);
      MMIO.Write_Unsigned_32
        (Get_Interrupt_Claim_Address (Device, Context), Interrupt_ID);
      Release_Spinlock (Device.Spinlock);
   end Complete_Supervisor_Interrupt;

   procedure Set_Interrupt_Priority_Unlocked
     (Device       : Device_T;
      IRQ          : Natural;
      IRQ_Priority : Natural;
      Result       : out Function_Result)
   is
      type Interrupt_Source_Priority_Array is
        array (0 .. 1023) of aliased Unsigned_32
      with Convention => C;

      Interrupt_Source_Priority : Interrupt_Source_Priority_Array
      with Import, Alignment => 1, Address => Device.Virtual_Address, Volatile;
   begin
      if IRQ > 1023 then
         Result := Invalid_Argument;
         return;
      end if;

      Interrupt_Source_Priority (IRQ) := Unsigned_32 (IRQ_Priority);

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Set_Interrupt_Priority_Unlocked");
         Result := Constraint_Exception;
   end Set_Interrupt_Priority_Unlocked;

   procedure Set_Interrupt_Priority
     (Device       : in out Device_T;
      IRQ          : Natural;
      IRQ_Priority : Natural;
      Result       : out Function_Result) is
   begin
      Acquire_Spinlock (Device.Spinlock);
      Set_Interrupt_Priority_Unlocked (Device, IRQ, IRQ_Priority, Result);
      Release_Spinlock (Device.Spinlock);
   end Set_Interrupt_Priority;

   pragma
     Warnings
       (Off, "pragma Restrictions (No_Exception_Propagation) in effect");
   procedure Set_IRQ_Enable_State_Unlocked
     (Device         : Device_T;
      Context        : Natural;
      Interrupt_Line : Natural;
      State          : Boolean;
      Result         : out Function_Result)
   is
      Interrupt_Enable_Address : constant System.Address :=
        Device.Virtual_Address + 16#2000#;

      Current_IRQ_Enable_State : Unsigned_32 := 0;
   begin
      --  The status of each individual interrupt line is represented by
      --  an individual bit in a 32-bit word. This variable holds the offset
      --  of the 32-bit word holding the specified interrupt line.
      Register_Offset : constant Storage_Offset :=
        Storage_Offset ((Interrupt_Line / 32) * 4);

      --  Each context is 0x80 bytes in size.
      Context_Offset : constant Storage_Offset :=
        Storage_Offset (Context) * 16#80#;

      --  Read the current IRQ enable state, OR it with the new state,
      --  and write it back.
      Current_IRQ_Enable_State :=
        MMIO.Read_Unsigned_32
          (Interrupt_Enable_Address + Context_Offset + Register_Offset);

      if State then
         Current_IRQ_Enable_State :=
           Current_IRQ_Enable_State or Shift_Left (1, Interrupt_Line mod 32);
      else
         Current_IRQ_Enable_State :=
           Current_IRQ_Enable_State
           and not (Shift_Left (1, Interrupt_Line mod 32));
      end if;

      MMIO.Write_Unsigned_32
        (Interrupt_Enable_Address + Context_Offset + Register_Offset,
         Current_IRQ_Enable_State);

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Set_IRQ_Enable_State_Unlocked");
         Result := Constraint_Exception;
   end Set_IRQ_Enable_State_Unlocked;
   pragma
     Warnings (On, "pragma Restrictions (No_Exception_Propagation) in effect");

   procedure Set_IRQ_Enable_State
     (Device         : in out Device_T;
      Context        : Natural;
      Interrupt_Line : Natural;
      State          : Boolean;
      Result         : out Function_Result) is
   begin
      Acquire_Spinlock (Device.Spinlock);
      Set_IRQ_Enable_State_Unlocked
        (Device, Context, Interrupt_Line, State, Result);
      Release_Spinlock (Device.Spinlock);
   end Set_IRQ_Enable_State;

   procedure Set_IRQ_Priority_Threshold
     (Device : in out Device_T; Context : Natural; Threshold : Natural)
   is
      Priority_Threshold_Address : constant System.Address :=
        Device.Virtual_Address + 16#20_0000#;

      Context_Offset : constant Storage_Offset :=
        Storage_Offset (Context) * 16#1000#;
   begin
      Acquire_Spinlock (Device.Spinlock);

      MMIO.Write_Unsigned_32
        (Priority_Threshold_Address + Context_Offset, Unsigned_32 (Threshold));
      Release_Spinlock (Device.Spinlock);
   end Set_IRQ_Priority_Threshold;
end Devices.PLIC;
