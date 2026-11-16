-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Interfaces; use Interfaces;

with Memory;    use Memory;
with Processes; use Processes;

-------------------------------------------------------------------------------
--  This package contains definitions related to handling traps.
-------------------------------------------------------------------------------

package Traps
  with Preelaborate
is
   procedure Handle_Supervisor_Mode_Trap
     (Trapping_Process_Addr : Virtual_Address_T;
      Scause                : Unsigned_64;
      Sepc                  : Virtual_Address_T;
      Stval                 : Unsigned_64)
   with
     Export,
     Convention    => Assembler,
     External_Name => "trap_handler_supervisor_mode";

   procedure Handle_User_Mode_Trap
     (Trapping_Process_Addr : Virtual_Address_T;
      Scause                : Unsigned_64;
      Sepc                  : Virtual_Address_T;
      Stval                 : Unsigned_64)
   with
     Export,
     Convention    => Assembler,
     External_Name => "trap_handler_user_mode";

   procedure Setup_Next_Timer_Interrupt;

   --  The kernel's 'tick' rate, in Hz.
   System_Tick_Rate : constant := 100;

   ----------------------------------------------------------------------------
   --  The kernel's 'tick' interval, in timebase units. The boot hart sets
   --  this from the Devicetree's timebase frequency before any timer is set.
   --  The timebase in QEMU's RISC-V virt machine is 10_000_000 Hz, so the
   --  interval there is 100_000.
   ----------------------------------------------------------------------------
   System_Tick_Interval : Unsigned_64 := 0;

private

   procedure Handle_Supervisor_Mode_Interrupt
     (Cause : Unsigned_64; Sepc : Virtual_Address_T; Stval : Unsigned_64);

   procedure Handle_Supervisor_Mode_Exception
     (Trapping_Process_Addr : Virtual_Address_T;
      Cause                 : Unsigned_64;
      Sepc                  : Virtual_Address_T;
      Stval                 : Unsigned_64);

   procedure Handle_External_Interrupt;

   function Is_Exception (Cause : Unsigned_64) return Boolean
   is ((Cause and 16#8000_0000_0000_0000#) = 0)
   with Inline, Pure_Function;

   function Is_Interrupt (Cause : Unsigned_64) return Boolean
   is ((Cause and 16#8000_0000_0000_0000#) /= 0)
   with Inline, Pure_Function;

   function Get_Cause (Cause : Unsigned_64) return Unsigned_64
   is (Cause and 16#7FFF_FFFF_FFFF_FFFF#)
   with Inline, Pure_Function;

   procedure Clear_Pending_Supervisor_Software_Interrupt
   with
     Import,
     Convention    => Assembler,
     External_Name => "traps_clear_pending_supervisor_software_interrupt";

   procedure Handle_User_Mode_Interrupt (Cause : Unsigned_64);

   procedure Handle_User_Mode_Exception
     (Trapping_Process : in out Process_Control_Block_T;
      Cause            : Unsigned_64;
      Sepc             : Virtual_Address_T;
      Stval            : Unsigned_64);

   procedure Handle_Timer_Interrupt;

   procedure Ensure_No_Locks_Held_In_Trap;

end Traps;
