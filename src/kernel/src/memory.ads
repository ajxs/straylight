-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with System; use System;

package Memory is
   pragma Preelaborate;

   --  This is the upper limit of what physical memory can is mapped into
   --  the kernel's address space. Currently, this is 64GiB.
   Physical_Memory_Map_Limit : constant := 16#10_0000_0000#;

   --  64MiB kernel heap initial size.
   Kernel_Heap_Initial_Size : constant := 16#400_0000#;

   ----------------------------------------------------------------------------
   --  Generic memcpy implementation.
   --  Required by the runtime for default initialisation of package variables.
   ----------------------------------------------------------------------------
   procedure Copy (Dest : Address; Source : Address; Count : Integer)
   with Import, Convention => C, External_Name => "memcpy";

   ----------------------------------------------------------------------------
   --  Generic memmove import.
   ----------------------------------------------------------------------------
   procedure Move (Dest : Address; Source : Address; Count : Integer)
   with Import, Convention => C, External_Name => "memmove";

   ----------------------------------------------------------------------------
   --  Generic memset import.
   ----------------------------------------------------------------------------
   procedure Set (Dest : Address; Value : Integer; Size : Integer)
   with Import, Convention => C, External_Name => "memset";

end Memory;
