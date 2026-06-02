-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with System;                  use System;
with System.Storage_Elements; use System.Storage_Elements;

with Devices;
with Devices.UART;
with Devices.Virtio.Graphics;
with Memory;             use Memory;
with Memory.Allocators;
with RISCV;              use RISCV;
with Processes.Scheduler;
with Hart_State;         use Hart_State;
with System_Calls.Files; use System_Calls.Files;

package body System_Calls is
   procedure Handle_Allocate_Memory_Syscall
     (Process : in out Process_Control_Block_T; Result : out Function_Result)
   is
      Trap_Context : Process_Context_T
      with
        Import,
        Convention => C,
        Alignment  => 1,
        Address    => Process.Trap_Context_Addr;

      Allocation_Result    : Memory.Allocators.Memory_Allocation_Result;
      Allocation_Size      : Natural := 0;
      Allocation_Alignment : Natural := 1;
   begin
      Allocation_Size := Natural (Trap_Context.Gp_Registers (a1));
      if Allocation_Size = 0 then
         Log_Error ("Allocation size is zero");

         Trap_Context.Gp_Registers (a0) :=
           Syscall_Error_Result_To_Unsigned_64
             (Syscall_Error_Invalid_Argument);

         goto Syscall_Unsuccessful_No_Kernel_Error;
      end if;

      Allocation_Alignment := Natural (Trap_Context.Gp_Registers (a2));
      if Allocation_Alignment < 1 then
         Log_Error ("Invalid allocation alignment");

         Trap_Context.Gp_Registers (a0) :=
           Syscall_Error_Result_To_Unsigned_64
             (Syscall_Error_Invalid_Argument);

         goto Syscall_Unsuccessful_No_Kernel_Error;
      end if;

      Log_Debug
        ("User Mode Syscall: Allocate Memory:"
         & ASCII.LF
         & "  PID:       "
         & Process.Process_Id'Image
         & ASCII.LF
         & "  Size:      "
         & Allocation_Size'Image
         & ASCII.LF
         & "  Alignment: "
         & Allocation_Alignment'Image,
         Logging_Tags);

      Process.Heap.Allocate
        (Allocation_Size,
         Allocation_Result,
         Result,
         Storage_Offset (Allocation_Alignment));
      if Is_Error (Result) then
         Log_Error ("Error allocating memory: " & Result'Image);

         Trap_Context.Gp_Registers (a0) :=
           Syscall_Error_Result_To_Unsigned_64 (Syscall_Error_No_Memory);

         goto Syscall_Unsuccessful_No_Kernel_Error;
      end if;

      Trap_Context.Gp_Registers (a0) :=
        Address_To_Unsigned_64 (Allocation_Result.Virtual_Address);
      Result := Success;
      return;

      <<Syscall_Unsuccessful_No_Kernel_Error>>
      Result := Syscall_Unsuccessful_Without_Kernel_Error;
   exception
      when Constraint_Error =>
         Log_Debug
           ("Constraint_Error: Handle_Allocate_Memory_Syscall", Logging_Tags);
         Result := Constraint_Exception;
   end Handle_Allocate_Memory_Syscall;

   procedure Handle_Free_Memory_Syscall
     (Process : in out Process_Control_Block_T; Result : out Function_Result)
   is
      Trap_Context : Process_Context_T
      with
        Import,
        Convention => C,
        Alignment  => 1,
        Address    => Process.Trap_Context_Addr;

      Address_To_Free : Virtual_Address_T := Null_Address;
   begin
      Address_To_Free :=
        Unsigned_64_To_Address (Trap_Context.Gp_Registers (a1));

      Log_Debug
        ("User Mode Syscall: Free Memory:"
         & ASCII.LF
         & "  PID:       "
         & Process.Process_Id'Image
         & ASCII.LF
         & "  Address:   "
         & Address_To_Free'Image,
         Logging_Tags);

      Process.Heap.Free (Address_To_Free, Result);
      if Is_Error (Result) then
         Trap_Context.Gp_Registers (a0) :=
           Syscall_Error_Result_To_Unsigned_64 (Syscall_Error_Invalid_Address);
         goto Syscall_Unsuccessful_No_Kernel_Error;
      end if;

      Trap_Context.Gp_Registers (a0) := Syscall_Result_Success;
      Result := Success;
      return;

      <<Syscall_Unsuccessful_No_Kernel_Error>>
      Result := Syscall_Unsuccessful_Without_Kernel_Error;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Handle_Free_Memory_Syscall");
         Result := Constraint_Exception;
   end Handle_Free_Memory_Syscall;

   procedure Handle_Logging_Syscall
     (Process   : in out Process_Control_Block_T;
      Log_Level : Log_Level_T;
      Result    : out Function_Result)
   is
      Trap_Context : Process_Context_T
      with
        Import,
        Convention => C,
        Alignment  => 1,
        Address    => Process.Trap_Context_Addr;

      String_Address : Virtual_Address_T := Null_Address;
      String_Length  : Integer := 0;

      Maximum_String_Length : constant Integer := 256;
   begin
      Log_Debug ("User Mode Syscall: Log", Logging_Tags);

      String_Address :=
        Unsigned_64_To_Address (Trap_Context.Gp_Registers (a1));
      String_Length := Integer (Trap_Context.Gp_Registers (a2));

      if not Is_Valid_Userspace_Address_Range (String_Address, String_Length)
      then
         Log_Error ("Invalid non-userspace address range", Logging_Tags);

         Trap_Context.Gp_Registers (a0) :=
           Syscall_Error_Result_To_Unsigned_64 (Syscall_Error_Invalid_Address);

         goto Syscall_Unsuccessful_No_Kernel_Error;
      end if;

      if String_Length > Maximum_String_Length or else String_Length < 0 then
         Log_Error
           ("Invalid string length: " & String_Length'Image, Logging_Tags);

         Trap_Context.Gp_Registers (a0) :=
           Syscall_Error_Result_To_Unsigned_64
             (Syscall_Error_Invalid_Argument);

         goto Syscall_Unsuccessful_No_Kernel_Error;
      end if;

      Read_String : declare
         String_Buffer : String (1 .. String_Length)
         with Import, Alignment => 1, Address => String_Address;
      begin
         if Log_Level = Log_Level_Debug then
            Log_Debug (String_Buffer (1 .. String_Length), Logging_Tags);
         elsif Log_Level = Log_Level_Error then
            Log_Error (String_Buffer (1 .. String_Length), Logging_Tags);
         end if;
      end Read_String;

      Trap_Context.Gp_Registers (a0) := Syscall_Result_Success;
      Result := Success;
      return;

      <<Syscall_Unsuccessful_No_Kernel_Error>>
      Result := Syscall_Unsuccessful_Without_Kernel_Error;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Handle_Logging_Syscall");
         Result := Constraint_Exception;
   end Handle_Logging_Syscall;

   procedure Handle_Print_To_Serial_Syscall
     (Process : in out Process_Control_Block_T; Result : out Function_Result)
   is
      Trap_Context : Process_Context_T
      with
        Import,
        Convention => C,
        Alignment  => 1,
        Address    => Process.Trap_Context_Addr;

      String_Address : Virtual_Address_T := Null_Address;
      String_Length  : Integer := 0;

      Maximum_String_Length : constant Integer := 256;

      --  @TODO: Revisit device discovery.
      --  This implementation is temporary.
      UART_Device renames Devices.System_Devices (2);
   begin
      Log_Debug ("User Mode Syscall: Print To Serial", Logging_Tags);

      String_Address :=
        Unsigned_64_To_Address (Trap_Context.Gp_Registers (a1));
      String_Length := Integer (Trap_Context.Gp_Registers (a2));

      if not Is_Valid_Userspace_Address_Range (String_Address, String_Length)
      then
         Log_Error ("Invalid non-userspace address range", Logging_Tags);

         Trap_Context.Gp_Registers (a0) :=
           Syscall_Error_Result_To_Unsigned_64 (Syscall_Error_Invalid_Address);

         goto Syscall_Unsuccessful_No_Kernel_Error;
      end if;

      if String_Length > Maximum_String_Length then
         Log_Error ("String length exceeds maximum length", Logging_Tags);

         Trap_Context.Gp_Registers (a0) :=
           Syscall_Error_Result_To_Unsigned_64
             (Syscall_Error_Invalid_Argument);

         goto Syscall_Unsuccessful_No_Kernel_Error;
      end if;

      Print_String : declare
         String_Buffer : String (1 .. String_Length)
         with Import, Alignment => 1, Address => String_Address;
      begin
         Devices.UART.Put_String
           (UART_Device, String_Buffer (1 .. String_Length));
      end Print_String;

      Trap_Context.Gp_Registers (a0) := Syscall_Result_Success;
      Result := Success;
      return;

      <<Syscall_Unsuccessful_No_Kernel_Error>>
      Result := Syscall_Unsuccessful_Without_Kernel_Error;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Handle_Print_To_Serial_Syscall");
         Result := Constraint_Exception;
   end Handle_Print_To_Serial_Syscall;

   procedure Handle_Process_Exit_Syscall is
   begin
      Log_Debug ("User Mode Syscall: Exit", Logging_Tags);

      --  Exit the process by calling the scheduler, which will set this
      --  process's status to 'stopped', and switch to the next ready process.
      Processes.Scheduler.Run (Process_Stopped);

      Panic ("Exited process still running");
   end Handle_Process_Exit_Syscall;

   procedure Handle_Process_Yield_Syscall is
   begin
      Log_Debug ("User Mode Syscall: Yield", Logging_Tags);

      Processes.Scheduler.Run (Process_Ready);
   end Handle_Process_Yield_Syscall;

   procedure Handle_User_Mode_Syscall
     (Process : in out Process_Control_Block_T; Result : out Function_Result)
   is
      Syscall_Number : Unsigned_64 := 0;

      Trap_Context : Process_Context_T
      with
        Import,
        Convention => C,
        Alignment  => 1,
        Address    => Process.Trap_Context_Addr;
   begin
      --  Retrieve the Syscall number from a0.
      Syscall_Number := Trap_Context.Gp_Registers (a0);

      case Syscall_Number is
         when Syscall_Exit_Process       =>
            --  This function does not return, so it doesn't need to set
            --  a result value. In the case that control ever returns to this
            --  point, it indicates the process didn't exit, so the kernel
            --  will panic.
            Handle_Process_Exit_Syscall;

         when Syscall_Yield_Process      =>
            Handle_Process_Yield_Syscall;
            Result := Success;

         when Syscall_Log_Debug          =>
            Handle_Logging_Syscall (Process, Log_Level_Debug, Result);

         when Syscall_Log_Error          =>
            Handle_Logging_Syscall (Process, Log_Level_Error, Result);

         when Syscall_Print_To_Serial    =>
            Handle_Print_To_Serial_Syscall (Process, Result);

         when Syscall_Allocate_Memory    =>
            Handle_Allocate_Memory_Syscall (Process, Result);

         when Syscall_Free_Memory        =>
            Handle_Free_Memory_Syscall (Process, Result);

         when Syscall_Open_File          =>
            Handle_Open_File_Syscall (Process, Result);

         when Syscall_Read_File          =>
            Handle_Read_File_Syscall (Process, Result);

         when Syscall_Seek_File          =>
            Handle_Seek_File_Syscall (Process, Result);

         when Syscall_Write_File         =>
            Handle_Write_File_Syscall (Process, Result);

         when Syscall_Close_File         =>
            Handle_Close_File_Syscall (Process, Result);

         when Syscall_Update_Framebuffer =>
            Handle_Update_Framebuffer_Syscall (Process, Result);

         when others                     =>
            Panic ("Unknown Syscall Number: " & Syscall_Number'Image);
      end case;

      if Is_Error (Result) then
         Panic ("Kernel Error in Syscall Handler: " & Result'Image);
      end if;

   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Handle_User_Mode_Syscall");
         Result := Constraint_Exception;
   end Handle_User_Mode_Syscall;

   procedure Handle_Update_Framebuffer_Syscall
     (Process : in out Process_Control_Block_T; Result : out Function_Result)
   is
      Trap_Context : Process_Context_T
      with
        Import,
        Convention => C,
        Alignment  => 1,
        Address    => Process.Trap_Context_Addr;

      User_Framebuffer_Address : Virtual_Address_T := Null_Address;

      Kernel_Framebuffer_Size : Natural := 0;

      Graphics_Device renames Devices.System_Devices (6);
   begin
      Log_Debug ("User Mode Syscall: Update Framebuffer", Logging_Tags);

      User_Framebuffer_Address :=
        Unsigned_64_To_Address (Trap_Context.Gp_Registers (a1));

      Kernel_Framebuffer_Size :=
        Integer
          (Graphics_Device.Framebuffer_Width
           * Graphics_Device.Framebuffer_Height)
        * 4;

      if not Is_Valid_Userspace_Address_Range
               (User_Framebuffer_Address, Kernel_Framebuffer_Size)
      then
         Log_Error ("Invalid non-userspace address range", Logging_Tags);

         Trap_Context.Gp_Registers (a0) :=
           Syscall_Error_Result_To_Unsigned_64 (Syscall_Error_Invalid_Address);

         goto Syscall_Unsuccessful_No_Kernel_Error;
      end if;

      Copy
        (Source => User_Framebuffer_Address,
         Dest   => Graphics_Device.Framebuffer_Addresses.Virtual_Address,
         Count  => Kernel_Framebuffer_Size);

      Devices.Virtio.Graphics.Transfer_To_Host_2d
        (Process,
         Graphics_Device,
         Graphics_Device.Bus_Info.Virtio.Resource_Id,
         0,
         0,
         Graphics_Device.Framebuffer_Width,
         Graphics_Device.Framebuffer_Height,
         Result);
      if Is_Error (Result) then
         --  Error already logged.
         return;
      end if;

      Devices.Virtio.Graphics.Resource_Flush
        (Process,
         Graphics_Device,
         Graphics_Device.Bus_Info.Virtio.Resource_Id,
         0,
         0,
         Graphics_Device.Framebuffer_Width,
         Graphics_Device.Framebuffer_Height,
         Result);
      if Is_Error (Result) then
         Log_Error ("Error transferring: " & Result'Image);
         return;
      end if;

      Trap_Context.Gp_Registers (a0) := Syscall_Result_Success;
      Result := Success;
      return;

      <<Syscall_Unsuccessful_No_Kernel_Error>>
      Result := Syscall_Unsuccessful_Without_Kernel_Error;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Handle_Update_Framebuffer_Syscall");
         Result := Constraint_Exception;
   end Handle_Update_Framebuffer_Syscall;

end System_Calls;
