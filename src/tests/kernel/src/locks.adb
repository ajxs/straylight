-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

package body Locks is
   procedure Acquire_Spinlock (Lock : in out Spinlock_T) is
      pragma Unreferenced (Lock);
   begin
      null;
   end Acquire_Spinlock;

   procedure Release_Spinlock (Lock : in out Spinlock_T) is
      pragma Unreferenced (Lock);
   begin
      null;
   end Release_Spinlock;
end Locks;
