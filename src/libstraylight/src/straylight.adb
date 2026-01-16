with System.Machine_Code;

with Straylight.Strings;
with Straylight.Common; use Straylight.Common;

package body Straylight is
   procedure Allocate_Memory
     (Size      : Unsigned_64;
      Addr      : out Address;
      Result    : out Function_Result;
      Alignment : Unsigned_64 := 1) is
   begin
      Syscall_Result : constant Syscall_Result_T :=
        Do_Syscall (Syscall_Allocate_Memory, Size, Alignment);

      if Syscall_Result = Syscall_Result_Success then
         Addr := To_Address (Unsigned_64 (Syscall_Result));
         Result := Function_Result_Success;
      else
         Addr := Null_Address;
         Result := Function_Result_Failure;
      end if;
   end Allocate_Memory;

   procedure Exit_Process (Exit_Code : Unsigned_64) is
   begin
      System.Machine_Code.Asm
        (Template => "mv a0, %0" & ASCII.LF & "mv a1, %1" & ASCII.LF & "ecall",
         Inputs   =>
           [Unsigned_64'Asm_Input ("r", Syscall_Exit_Process),
            Unsigned_64'Asm_Input ("r", Exit_Code)],
         Clobber  => "a0,a1",
         Volatile => True);
   end Exit_Process;

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

   procedure Open_File
     (Path           : Wide_String;
      Mode           : File_Open_Mode_T;
      File_Handle_Id : out File_Handle_Id_T;
      Result         : out Function_Result) is
   begin
      Syscall_Result : constant Syscall_Result_T :=
        Do_Syscall
          (Syscall_Open_File,
           Address_To_Unsigned_64 (Path'Address),
           Unsigned_64 (Path'Length),
           Unsigned_64 (Mode));

      if Syscall_Result = Syscall_Result_Success then
         File_Handle_Id := File_Handle_Id_T (Syscall_Result);
         Result := Function_Result_Success;
      else
         File_Handle_Id := 0;
         Result := Function_Result_Failure;
      end if;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: LibStraylight.Open_File");
         File_Handle_Id := 0;
         Result := Function_Result_Failure;
   end Open_File;

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

   procedure Read_File
     (File_Handle_Id : File_Handle_Id_T;
      Buffer_Address : Address;
      Size           : Unsigned_64;
      Bytes_Read     : out Unsigned_64;
      Result         : out Function_Result) is
   begin
      Syscall_Result : constant Syscall_Result_T :=
        Do_Syscall
          (Syscall_Read_File,
           File_Handle_Id,
           Address_To_Unsigned_64 (Buffer_Address),
           Size);

      if Syscall_Result = Syscall_Result_Success then
         Bytes_Read := Unsigned_64 (Syscall_Result);
         Result := Function_Result_Success;
      else
         Bytes_Read := 0;
         Result := Function_Result_Failure;
      end if;
   end Read_File;

   procedure Seek_File
     (File_Handle_Id : File_Handle_Id_T;
      New_Offset     : Unsigned_64;
      Result         : out Function_Result) is
   begin
      Syscall_Result : constant Syscall_Result_T :=
        Do_Syscall (Syscall_Read_File, File_Handle_Id, New_Offset);

      if Syscall_Result = Syscall_Result_Success then
         Result := Function_Result_Success;
      else
         Result := Function_Result_Failure;
      end if;
   end Seek_File;

   procedure Yield_Process is
   begin
      System.Machine_Code.Asm
        (Template => "mv a0, %0" & ASCII.LF & "ecall",
         Inputs   => [Unsigned_64'Asm_Input ("r", Syscall_Yield_Process)],
         Clobber  => "a0",
         Volatile => True);
   end Yield_Process;
end Straylight;
