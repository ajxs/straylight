-------------------------------------------------------------------------------
--  Copyright (c) 2026, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with AUnit.Test_Suites; use AUnit.Test_Suites;

--  This suite is a child of Filesystems, because only a descendant of
--  Filesystems can see its private child Filesystems.Paths.

function Filesystems.Paths_Test_Suite return Access_Test_Suite;
