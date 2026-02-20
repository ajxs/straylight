with Interfaces; use Interfaces;
with System;     use System;

package Straylight is
   pragma Preelaborate;

   type Function_Result is new Integer;

   Function_Result_Success : constant Function_Result := 0;
   Function_Result_Failure : constant Function_Result := 1;

   procedure Log_Debug (Str : String);

   procedure Log_Error (Str : String);

   procedure Print_To_Serial (Str : String);

private
   Syscall_Exit_Process    : constant := 5446_0000;
   Syscall_Yield_Process   : constant := 5446_0001;
   Syscall_Log_Debug       : constant := 5446_0002;
   Syscall_Log_Error       : constant := 5446_0003;
   Syscall_Allocate_Memory : constant := 5446_0004;
   Syscall_Free_Memory     : constant := 5446_0005;
   Syscall_Print_To_Serial : constant := 5446_0006;

   Syscall_Open_File : constant := 5446_0107;
   Syscall_Read_File : constant := 5446_0108;
   Syscall_Seek_File : constant := 5446_0109;

   Syscall_Update_Framebuffer : constant := 5446_0209;

   type Syscall_Result_T is new Integer with Size => 64;

   Syscall_Result_Success : constant Syscall_Result_T := 0;

   type Log_Message_Type_T is (Log_Message_Debug, Log_Message_Error);

   procedure Log_Debug_C (String_Address : Address)
   with Export, Convention => C, External_Name => "log_debug";

   procedure Log_Error_C (String_Address : Address)
   with Export, Convention => C, External_Name => "log_error";

   procedure Log_To_Kernel
     (Message_Type   : Log_Message_Type_T;
      String_Address : Address;
      String_Length  : Integer);

   procedure Print_To_Serial_Internal
     (String_Address : Address; String_Length : Integer);

   procedure Print_To_Serial_C (String_Address : Address)
   with Export, Convention => C, External_Name => "print_to_serial";

   function Do_Syscall
     (Syscall_Number : Unsigned_64;
      Arg1           : Unsigned_64 := 0;
      Arg2           : Unsigned_64 := 0;
      Arg3           : Unsigned_64 := 0) return Syscall_Result_T
   with
     Volatile_Function,
     Import,
     Convention    => Assembler,
     External_Name => "straylight_syscall";

end Straylight;
