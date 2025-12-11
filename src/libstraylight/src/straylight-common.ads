-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  Authors:
--     Anthony <ajxs [at] panoptic.online>
-------------------------------------------------------------------------------

with System.Storage_Elements; use System.Storage_Elements;

package Straylight.Common is
   pragma Preelaborate;

   function Get_Address_Word_Low (Addr : Address) return Unsigned_32;

   function Get_Address_Word_High (Addr : Address) return Unsigned_32;

   function Convert_BEU32_To_LEU32 (BEU32 : Unsigned_32) return Unsigned_32
   with Pure_Function;

   function Convert_BEU64_To_LEU64 (BEU64 : Unsigned_64) return Unsigned_64
   with Pure_Function;

   function Unsigned_32_To_Address (U32 : Unsigned_32) return Address
   is (To_Address (Integer_Address (U32)))
   with Inline, Pure_Function;

   function To_Address (U64 : Unsigned_64) return Address
   is (To_Address (Integer_Address (U64)))
   with Inline, Pure_Function;

   function Address_To_Unsigned_64 (Addr : Address) return Unsigned_64
   is (Unsigned_64 (To_Integer (Addr)))
   with Inline, Pure_Function;
end Straylight.Common;
