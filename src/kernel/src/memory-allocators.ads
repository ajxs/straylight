-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Addresses; use Addresses;

package Memory.Allocators is
   pragma Preelaborate;

   type Memory_Allocation_Result is record
      Virtual_Address  : Virtual_Address_T := System.Null_Address;
      Physical_Address : Physical_Address_T := Null_Physical_Address;
   end record;

end Memory.Allocators;
