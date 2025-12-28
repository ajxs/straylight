-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Processes; use Processes;

package System_State is
   pragma Preelaborate;

   Maximum_Harts : constant := 8;
   subtype Hart_Index_T is Natural range 0 .. (Maximum_Harts - 1);

   type Hart_State_T is record
      Current_Process                         : Process_Control_Block_Access :=
        null;
      Interrupts_Off_Counter                  : Natural := 0;
      Were_Interrupts_Enabled_Before_Push_Off : Boolean := True;
   end record;

   type Hart_State_Access is access all Hart_State_T;

   type Hart_State_Array_T is array (Hart_Index_T) of aliased Hart_State_T;

   Hart_States : Hart_State_Array_T;

   procedure Panic (Message : String := "Kernel Panic")
   with No_Return;

   --  @TODO: Implement.
   --  Currently the kernel only supports a single hart.
   function Get_Current_Hart_Id return Hart_Index_T
   is (0)
   with Inline, Volatile_Function;

   function Get_Current_Hart_State return Hart_State_Access
   with Volatile_Function;

   function Get_Process_Running_On_Current_Hart
      return Process_Control_Block_Access
   with Volatile_Function;

   --  These functions allow for turning interrupts on/off in nested critical
   --  sections.
   procedure Push_Interrupts_Off;

   procedure Pop_Interrupts_Off;

   function Get_Current_Hart_Supervisor_Interrupt_Context return Integer
   with Inline, Volatile_Function;

end System_State;
