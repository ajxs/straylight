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
-------------------------------------------------------------------------------

package MMIO is
   pragma Preelaborate;

   ----------------------------------------------------------------------------
   --  This function reads a byte from a memory address.
   ----------------------------------------------------------------------------
   function Read_Unsigned_8 (Addr : System.Address) return Unsigned_8
   with
     Volatile_Function,
     Import,
     Convention    => Assembler,
     External_Name => "mmio_read_unsigned_8";

   ----------------------------------------------------------------------------
   --  Writes a byte to a memory address.
   ----------------------------------------------------------------------------
   procedure Write_Unsigned_8 (Addr : System.Address; Data : Unsigned_8)
   with
     Import,
     Convention    => Assembler,
     External_Name => "mmio_write_unsigned_8";

   ----------------------------------------------------------------------------
   --  Reads a qword from a memory address.
   ----------------------------------------------------------------------------
   function Read_Unsigned_32 (Addr : System.Address) return Unsigned_32
   with
     Volatile_Function,
     Import,
     Convention    => Assembler,
     External_Name => "mmio_read_unsigned_32";

   ----------------------------------------------------------------------------
   --  Writes a qword to a memory address.
   ----------------------------------------------------------------------------
   procedure Write_Unsigned_32 (Addr : System.Address; Data : Unsigned_32)
   with
     Import,
     Convention    => Assembler,
     External_Name => "mmio_write_unsigned_32";

   ----------------------------------------------------------------------------
   --  Reads a qword from a memory address.
   ----------------------------------------------------------------------------
   function Read_Unsigned_64 (Addr : System.Address) return Unsigned_64
   with
     Volatile_Function,
     Import,
     Convention    => Assembler,
     External_Name => "mmio_read_unsigned_64";

   ----------------------------------------------------------------------------
   --  Writes a qword to a memory address.
   ----------------------------------------------------------------------------
   procedure Write_Unsigned_64 (Addr : System.Address; Data : Unsigned_64)
   with
     Import,
     Convention    => Assembler,
     External_Name => "mmio_write_unsigned_64";

end MMIO;
