-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

package RISCV.Atomics is
   pragma Pure;

   function Atomic_Swap_And_Return_Unsigned_32
     (Target : not null access Unsigned_32; Value : Unsigned_32)
      return Unsigned_32
   with
     Import,
     Convention    => C,
     External_Name => "riscv_atomics_atomic_swap_and_return_unsigned_32",
     Volatile_Function;

   function Atomic_Swap_And_Return_Unsigned_64
     (Target : not null access Unsigned_64; Value : Unsigned_64)
      return Unsigned_64
   with
     Import,
     Convention    => C,
     External_Name => "riscv_atomics_atomic_swap_and_return_unsigned_64",
     Volatile_Function;

   procedure Atomic_Clear_Unsigned_32 (Target : not null access Unsigned_32)
   with
     Import,
     Convention    => C,
     External_Name => "riscv_atomics_atomic_clear_unsigned_32";

   procedure Atomic_Clear_Unsigned_64 (Target : not null access Unsigned_64)
   with
     Import,
     Convention    => C,
     External_Name => "riscv_atomics_atomic_clear_unsigned_64";

   procedure Fence
   with Import, Convention => C, External_Name => "riscv_atomics_fence";

end RISCV.Atomics;
