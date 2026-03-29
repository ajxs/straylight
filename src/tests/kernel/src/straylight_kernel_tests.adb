-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with AUnit.Reporter.Text;
with AUnit.Run;
with Utilities_Test_Suite;

procedure Straylight_Kernel_Tests is
   procedure Runner is new AUnit.Run.Test_Runner (Utilities_Test_Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;
begin
   AUnit.Reporter.Text.Set_Use_ANSI_Colors (Reporter, True);
   Runner (Reporter);
end Straylight_Kernel_Tests;
