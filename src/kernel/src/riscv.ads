-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Interfaces;              use Interfaces;
with System;                  use System;
with System.Storage_Elements; use System.Storage_Elements;

package RISCV is
   pragma Pure;

   type GP_Register_List_T is
     (zero,
      ra,
      sp,
      gp,
      tp,
      t0,
      t1,
      t2,
      s0,
      s1,
      a0,
      a1,
      a2,
      a3,
      a4,
      a5,
      a6,
      a7,
      s2,
      s3,
      s4,
      s5,
      s6,
      s7,
      s8,
      s9,
      s10,
      s11,
      t3,
      t4,
      t5,
      t6);

   SV39_Mode_Flags_Mask : constant := 16#8000000000000000#;

   function Create_SATP
     (Page_Table_Address : Address; ASID : Unsigned_16) return Unsigned_64
   is (Shift_Right (Unsigned_64 (To_Integer (Page_Table_Address)), 12)
       or (Shift_Left (Unsigned_64 (ASID), 44))
       or SV39_Mode_Flags_Mask)
   with Inline, Pure_Function;

   function Get_System_Time return Unsigned_64
   with
     Import,
     Convention    => Assembler,
     External_Name => "riscv_get_system_time";

end RISCV;
