-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

package RISCV.SBI is
   pragma Preelaborate;

   type SBI_Result_T is record
      Error : Integer_32;
      Value : Integer_32;
   end record
   with Convention => C;

   --  SBI Error codes.
   --  Refer to:
   --  https://github.com/
   --    riscv-non-isa/riscv-sbi-doc/blob/master/src/binary-encoding.adoc
   SBI_SUCCESS               : constant := 0; --  Completed successfully
   SBI_ERR_FAILED            : constant := -1; --  Failed
   SBI_ERR_NOT_SUPPORTED     : constant := -2; --  Not supported
   SBI_ERR_INVALID_PARAM     : constant := -3; --  Invalid parameter(s)
   SBI_ERR_DENIED            : constant := -4; --  Denied or not allowed
   SBI_ERR_INVALID_ADDRESS   : constant := -5; --  Invalid address(s)
   SBI_ERR_ALREADY_AVAILABLE : constant := -6; --  Already available
   SBI_ERR_ALREADY_STARTED   : constant := -7; --  Already started
   SBI_ERR_ALREADY_STOPPED   : constant := -8; --  Already stopped
   SBI_ERR_NO_SHMEM          : constant := -9; --  Shared memory not available
   SBI_ERR_INVALID_STATE     : constant := -10; --  Invalid state
   SBI_ERR_BAD_RANGE         : constant := -11; --  Bad (or invalid) range
   SBI_ERR_TIMEOUT           : constant := -12; --  Failed due to timeout
   SBI_ERR_IO                : constant := -13; --  Input/Output error
   SBI_ERR_DENIED_LOCKED     : constant :=
     -14; --  Denied or not allowed due to lock status

   function Debug_Console_Print
     (String_Length : Unsigned_64;
      Address_Low   : Unsigned_64;
      Address_High  : Unsigned_64) return SBI_Result_T
   with
     Import,
     Convention    => Assembler,
     External_Name => "sbi_debug_console_print";

   function Set_Timer (Next_Event_Time : Unsigned_64) return SBI_Result_T
   with Import, Convention => Assembler, External_Name => "sbi_set_timer";

   function Hart_Start
     (Hart_Id : Unsigned_32; Start_Address : Unsigned_64; Opaque : Unsigned_64)
      return SBI_Result_T
   with Import, Convention => Assembler, External_Name => "sbi_hart_start";

   function Get_Hart_Status (Hart_Id : Unsigned_32) return SBI_Result_T
   with
     Import,
     Convention    => Assembler,
     External_Name => "sbi_get_hart_status";

end RISCV.SBI;
