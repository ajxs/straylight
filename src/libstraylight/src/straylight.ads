with Interfaces; use Interfaces;

package Straylight is
   pragma Preelaborate;

   type Function_Result is new Integer;

   Function_Result_Success : constant Function_Result := 0;
   Function_Result_Failure : constant Function_Result := 1;

private
   Syscall_Update_Framebuffer : constant := 5446_0209;

   type Syscall_Result_T is new Integer with Size => 64;

   Syscall_Result_Success : constant Syscall_Result_T := 0;

   type Log_Message_Type_T is (Log_Message_Debug, Log_Message_Error);

   function Do_Syscall
     (Syscall_Number : Unsigned_64;
      Arg1           : Unsigned_64 := 0;
      Arg2           : Unsigned_64 := 0;
      Arg3           : Unsigned_64 := 0;
      Arg4           : Unsigned_64 := 0;
      Arg5           : Unsigned_64 := 0) return Syscall_Result_T
   with
     Volatile_Function,
     Import,
     Convention    => Assembler,
     External_Name => "straylight_syscall";

end Straylight;
