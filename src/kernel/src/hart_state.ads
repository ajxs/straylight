-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Processes; use Processes;

package Hart_State is
   pragma Preelaborate;

   Maximum_Harts : constant := 8;
   subtype Hart_Index_T is Natural range 0 .. (Maximum_Harts - 1);

   type Hart_State_T is record
      Current_Process                                 :
        Process_Control_Block_Access := null;
      Hart_Id                                         : Hart_Index_T := 0;
      Interrupts_Off_Counter                          : Natural := 0;
      Were_Interrupts_Enabled_Before_Initial_Push_Off : Boolean := True;
   end record;
   for Hart_State_T use
     record
       Current_Process at 0 range 0 .. 63;
       Hart_Id at 8 range 0 .. 31;
       Interrupts_Off_Counter at 12 range 0 .. 31;
       Were_Interrupts_Enabled_Before_Initial_Push_Off at 16 range 0 .. 0;
     end record;

   type Hart_State_Access is access all Hart_State_T with Convention => C;

   type Hart_State_Array_T is array (Hart_Index_T) of aliased Hart_State_T;

   Hart_States : Hart_State_Array_T;

   procedure Panic (Message : String := "Kernel Panic")
   with No_Return;

   function Get_Current_Hart_Id return Hart_Index_T
   with
     Import,
     Volatile_Function,
     Convention    => Assembler,
     External_Name => "hart_state_get_hart_id";

   function Get_Current_Hart_State return Hart_State_Access
   with
     Import,
     Volatile_Function,
     Convention    => Assembler,
     External_Name => "hart_state_get_hart_state";

   function Get_Process_Running_On_Current_Hart
      return Process_Control_Block_Access
   with
     Import,
     Volatile_Function,
     Convention    => Assembler,
     External_Name => "hart_state_get_current_running_process";

   --  These functions allow for turning interrupts on/off in nested critical
   --  sections.
   procedure Push_Interrupts_Off;

   procedure Pop_Interrupts_Off;

   function Get_Current_Hart_Supervisor_Interrupt_Context return Integer
   with Inline, Volatile_Function;

end Hart_State;
