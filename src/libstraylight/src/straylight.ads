with Interfaces; use Interfaces;
with System;     use System;

package Straylight is
   pragma Preelaborate;

   subtype Syscall_Result_T is Unsigned_64;

   Syscall_Result_Success : constant Syscall_Result_T := 0;
   Syscall_Result_Failure : constant Syscall_Result_T := 1;

   subtype File_Handle_Id_T is Unsigned_64;
   type File_Open_Mode_T is new Integer;

   File_Open_Mode_Read  : constant := 0;
   File_Open_Mode_Write : constant := 1;

   procedure Allocate_Memory
     (Size      : Unsigned_64;
      Addr      : out Address;
      Result    : out Syscall_Result_T;
      Alignment : Unsigned_64 := 1);

   procedure Log_Debug (Str : String);

   procedure Log_Error (Str : String);

   procedure Exit_Process (Exit_Code : Unsigned_64)
   with Export, Convention => C, External_Name => "exit";

   procedure Yield_Process
   with Export, Convention => C, External_Name => "yield";

   procedure Print_To_Serial (Str : String);

   procedure Open_File
     (Path           : Wide_String;
      Mode           : File_Open_Mode_T;
      File_Handle_Id : out File_Handle_Id_T;
      Result         : out Syscall_Result_T);

   procedure Read_File
     (File_Handle_Id : File_Handle_Id_T;
      Buffer_Address : Address;
      Size           : Unsigned_64;
      Bytes_Read     : out Unsigned_64;
      Result         : out Syscall_Result_T);

   procedure Seek_File
     (File_Handle_Id : File_Handle_Id_T;
      New_Offset     : Unsigned_64;
      Result         : out Syscall_Result_T);

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

   type Log_Message_Type_T is (Log_Message_Debug, Log_Message_Error);

   procedure Log_Debug_C (String_Address : Address)
   with Export, Convention => C, External_Name => "log_debug";

   procedure Log_Error_C (String_Address : Address)
   with Export, Convention => C, External_Name => "log_error";

   function Malloc (Size : Unsigned_64) return System.Address
   with Export, Convention => C, External_Name => "malloc";

   procedure Log_To_Kernel
     (Message_Type   : Log_Message_Type_T;
      String_Address : Address;
      String_Length  : Integer);

   procedure Print_To_Serial_Internal
     (String_Address : Address; String_Length : Integer);

   procedure Print_To_Serial_C (String_Address : Address)
   with Export, Convention => C, External_Name => "print_to_serial";
end Straylight;
