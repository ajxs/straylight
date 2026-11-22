-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Interfaces; use Interfaces;
with System;

-------------------------------------------------------------------------------
--  Contains functionality for memory-mapped I/O on the RISC-V platform.
--  Functions are included for inputting and outputting data to
--  memory-mapped addresses, useful for interacting with system peripherals.
--  They're Inline_Always rather than Inline: GNAT only inlines a call to
--  another unit's subprogram if the kernel is built with -gnatn.
-------------------------------------------------------------------------------

package MMIO
  with Preelaborate
is
   ----------------------------------------------------------------------------
   --  Reads a byte from a memory address.
   ----------------------------------------------------------------------------
   function Read_Unsigned_8 (Addr : System.Address) return Unsigned_8
   with Volatile_Function, Inline_Always;

   ----------------------------------------------------------------------------
   --  Writes a byte to a memory address.
   ----------------------------------------------------------------------------
   procedure Write_Unsigned_8 (Addr : System.Address; Data : Unsigned_8)
   with Inline_Always;

   ----------------------------------------------------------------------------
   --  Reads a 32-bit word from a memory address.
   ----------------------------------------------------------------------------
   function Read_Unsigned_32 (Addr : System.Address) return Unsigned_32
   with Volatile_Function, Inline_Always;

   ----------------------------------------------------------------------------
   --  Writes a 32-bit word to a memory address.
   ----------------------------------------------------------------------------
   procedure Write_Unsigned_32 (Addr : System.Address; Data : Unsigned_32)
   with Inline_Always;

   ----------------------------------------------------------------------------
   --  Reads a qword from a memory address.
   ----------------------------------------------------------------------------
   function Read_Unsigned_64 (Addr : System.Address) return Unsigned_64
   with Volatile_Function, Inline_Always;

   ----------------------------------------------------------------------------
   --  Writes a qword to a memory address.
   ----------------------------------------------------------------------------
   procedure Write_Unsigned_64 (Addr : System.Address; Data : Unsigned_64)
   with Inline_Always;

end MMIO;
