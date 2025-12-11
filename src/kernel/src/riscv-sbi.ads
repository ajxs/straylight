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

end RISCV.SBI;
