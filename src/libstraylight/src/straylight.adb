with System.Machine_Code;

with Straylight.Strings;

package body Straylight is
   procedure Log_Debug (Str : String) is
   begin
      --  @TODO: There is an issue with exception handlers that cannot
      --  be entered. This needs to be resolved.
      pragma Warnings (Off);
      Log_To_Kernel (Log_Message_Debug, Str'Address, Str'Length);
      pragma Warnings (On);
   end Log_Debug;

   procedure Log_Debug_C (String_Address : Address) is
      String_Length : Integer := 0;
   begin
      String_Length := Straylight.Strings.Get_String_Length (String_Address);

      Log_To_Kernel (Log_Message_Debug, String_Address, String_Length);
   end Log_Debug_C;

   procedure Log_Error (Str : String) is
   begin
      --  @TODO: There is an issue with exception handlers that cannot
      --  be entered. This needs to be resolved.
      pragma Warnings (Off);
      Log_To_Kernel (Log_Message_Error, Str'Address, Str'Length);
      pragma Warnings (On);
   end Log_Error;

   procedure Log_Error_C (String_Address : Address) is
      String_Length : Integer := 0;
   begin
      String_Length := Straylight.Strings.Get_String_Length (String_Address);

      Log_To_Kernel (Log_Message_Error, String_Address, String_Length);
   end Log_Error_C;

   procedure Log_To_Kernel
     (Message_Type   : Log_Message_Type_T;
      String_Address : Address;
      String_Length  : Integer)
   is
      Syscall_Number : Unsigned_64 := 0;
   begin
      case Message_Type is
         when Log_Message_Debug =>
            Syscall_Number := Syscall_Log_Debug;

         when Log_Message_Error =>
            Syscall_Number := Syscall_Log_Error;
      end case;

      System.Machine_Code.Asm
        (Template =>
           "mv a0, %0"
           & ASCII.LF
           & "mv a1, %1"
           & ASCII.LF
           & "mv a2, %2"
           & ASCII.LF
           & "ecall",
         Inputs   =>
           [Unsigned_64'Asm_Input ("r", Syscall_Number),
            Address'Asm_Input ("r", String_Address),
            Integer'Asm_Input ("r", String_Length)],
         Clobber  => "a0,a1,a2",
         Volatile => True);
   exception
      when Constraint_Error =>
         null;
   end Log_To_Kernel;

   procedure Print_To_Serial (Str : String) is
   begin
      pragma Warnings (Off);
      Print_To_Serial_Internal (Str'Address, Str'Length);
      pragma Warnings (On);
   end Print_To_Serial;

   procedure Print_To_Serial_C (String_Address : Address) is
      String_Length : Integer := 0;
   begin
      String_Length := Straylight.Strings.Get_String_Length (String_Address);

      Print_To_Serial_Internal (String_Address, String_Length);
   end Print_To_Serial_C;

   procedure Print_To_Serial_Internal
     (String_Address : Address; String_Length : Integer) is
   begin
      System.Machine_Code.Asm
        (Template =>
           "mv a0, %0"
           & ASCII.LF
           & "mv a1, %1"
           & ASCII.LF
           & "mv a2, %2"
           & ASCII.LF
           & "ecall",
         Inputs   =>
           [Unsigned_64'Asm_Input ("r", Syscall_Print_To_Serial),
            Address'Asm_Input ("r", String_Address),
            Integer'Asm_Input ("r", String_Length)],
         Clobber  => "a0,a1,a2",
         Volatile => True);
   end Print_To_Serial_Internal;

end Straylight;
