-------------------------------------------------------------------------------
--  Copyright (c) 2026, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
--  This private package creates the initialisation process: the first running
--  process in the system which is responsible for mounting the disk
--  filesystems and starting the first shell.
-------------------------------------------------------------------------------

private package Boot.Init
  with Preelaborate
is
   procedure Initialise_Init_Process;

   procedure Start_Init_Process;

end Boot.Init;
