-------------------------------------------------------------------------------
--  Copyright (c) 2026, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

package body MMIO is
   function Read_Unsigned_8 (Addr : System.Address) return Unsigned_8 is
      Register : constant Unsigned_8
      with Import, Volatile_Full_Access, Alignment => 1, Address => Addr;
   begin
      return Register;
   end Read_Unsigned_8;

   procedure Write_Unsigned_8 (Addr : System.Address; Data : Unsigned_8) is
      Register : Unsigned_8
      with Import, Volatile_Full_Access, Alignment => 1, Address => Addr;
   begin
      Register := Data;
   end Write_Unsigned_8;

   function Read_Unsigned_32 (Addr : System.Address) return Unsigned_32 is
      Register : constant Unsigned_32
      with Import, Volatile_Full_Access, Alignment => 1, Address => Addr;
   begin
      return Register;
   end Read_Unsigned_32;

   procedure Write_Unsigned_32 (Addr : System.Address; Data : Unsigned_32) is
      Register : Unsigned_32
      with Import, Volatile_Full_Access, Alignment => 1, Address => Addr;
   begin
      Register := Data;
   end Write_Unsigned_32;

   function Read_Unsigned_64 (Addr : System.Address) return Unsigned_64 is
      Register : constant Unsigned_64
      with Import, Volatile_Full_Access, Alignment => 1, Address => Addr;
   begin
      return Register;
   end Read_Unsigned_64;

   procedure Write_Unsigned_64 (Addr : System.Address; Data : Unsigned_64) is
      Register : Unsigned_64
      with Import, Volatile_Full_Access, Alignment => 1, Address => Addr;
   begin
      Register := Data;
   end Write_Unsigned_64;

end MMIO;
