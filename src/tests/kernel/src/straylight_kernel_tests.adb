-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with AUnit.Reporter.Text;
with AUnit.Run;

with Utilities_Test_Suite;
with Memory_Test_Suite;
with Memory_Allocators_Heap_Test_Suite;
with Memory_Allocators_Page_Test_Suite;

procedure Straylight_Kernel_Tests is
   procedure Utilities_Test_Runner is new
     AUnit.Run.Test_Runner (Utilities_Test_Suite);

   procedure Memory_Test_Runner is new
     AUnit.Run.Test_Runner (Memory_Test_Suite);

   procedure Heap_Test_Runner is new
     AUnit.Run.Test_Runner (Memory_Allocators_Heap_Test_Suite);

   procedure Page_Test_Runner is new
     AUnit.Run.Test_Runner (Memory_Allocators_Page_Test_Suite);

   Reporter : AUnit.Reporter.Text.Text_Reporter;
begin
   AUnit.Reporter.Text.Set_Use_ANSI_Colors (Reporter, True);
   Utilities_Test_Runner (Reporter);

   Memory_Test_Runner (Reporter);

   Heap_Test_Runner (Reporter);

   Page_Test_Runner (Reporter);

end Straylight_Kernel_Tests;
