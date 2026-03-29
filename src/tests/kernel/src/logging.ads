-------------------------------------------------------------------------------
--  Copyright (c) 2026, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  Stub Logging package for unit tests. Provides the minimal interface
--  required by kernel modules.
--  It's possible to declare the logging procedures as null procedures, but
--  unfortunately this will attempt to pull in the logging package body from
--  the kernel project.
-------------------------------------------------------------------------------

package Logging is
   pragma Preelaborate;

   type Log_Tag_T is (Dummy_Tag);

   type Log_Tags is array (Natural range <>) of Log_Tag_T;

   Empty_Tag_List : constant Log_Tags := [1 .. 0 => <>];

   procedure Log_Error (Message : String; Tags : Log_Tags := Empty_Tag_List);

end Logging;
