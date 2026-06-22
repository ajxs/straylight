-------------------------------------------------------------------------------
--  Copyright (c) 2026, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------
with System; use System;

with Memory; use Memory;
with RISCV;  use RISCV;

package body System_Calls.Files is
   procedure Handle_Open_File_Syscall
     (Process : in out Process_Control_Block_T; Result : out Function_Result)
   is
      Trap_Context : Process_Context_T
      with
        Import,
        Convention => C,
        Alignment  => 1,
        Address    => Process.Trap_Context_Addr;

      Path_String_Address : Virtual_Address_T := Null_Address;
      Path_String_Length  : Integer := 0;

      Maximum_Path_String_Length : constant Integer := 256;

      File_Open_Flags : File_Open_Flags_T;
      File_Handle     : Process_File_Handle_Access := null;
   begin
      Log_Debug ("User Mode Syscall: Open File", Logging_Tags);

      Path_String_Address :=
        Unsigned_64_To_Address (Trap_Context.Gp_Registers (a1));
      Path_String_Length := Integer (Trap_Context.Gp_Registers (a2));

      File_Open_Flags :=
        Unsigned_64_To_File_Open_Flags (Trap_Context.Gp_Registers (a3));

      Log_Debug
        ("File_Open_Flags: " & Trap_Context.Gp_Registers (a3)'Image,
         Logging_Tags);

      if not Is_Valid_Userspace_Address_Range
               (Path_String_Address, Path_String_Length)
      then
         Log_Error ("Invalid non-userspace address range");

         Trap_Context.Gp_Registers (a0) :=
           Syscall_Error_Result_To_Unsigned_64 (-EFAULT);

         goto Syscall_Unsuccessful_No_Kernel_Error;
      end if;

      if Path_String_Length > Maximum_Path_String_Length then
         Log_Error
           ("Path length exceeds maximum length: " & Path_String_Length'Image,
            Logging_Tags);

         Trap_Context.Gp_Registers (a0) :=
           Syscall_Error_Result_To_Unsigned_64 (-ENAMETOOLONG);

         goto Syscall_Unsuccessful_No_Kernel_Error;
      end if;

      Read_Path_String_And_Open_File : declare
         User_Path_String : Filesystem_Path_T (1 .. Path_String_Length)
         with
           Import,
           Convention => C,
           Alignment  => 1,
           Address    => Path_String_Address;

         --  Copy the userland path string into a new variable to ensure it is
         --  in the correct format, and within the kernel address space.
         New_Path_String : constant Filesystem_Path_T :=
           User_Path_String (1 .. Path_String_Length);
      begin
         Filesystems.Open_File
           (Process, New_Path_String, File_Open_Flags, File_Handle, Result);
         if Result = File_Not_Found then
            Log_Error ("File not found");

            Trap_Context.Gp_Registers (a0) :=
              Syscall_Error_Result_To_Unsigned_64 (-ENOENT);

            goto Syscall_Unsuccessful_No_Kernel_Error;
         elsif Is_Error (Result) then
            Log_Error ("Error opening file: " & Result'Image, Logging_Tags);
            return;
         end if;

         Log_Debug ("Opened file handle", Logging_Tags);
      end Read_Path_String_And_Open_File;

      Trap_Context.Gp_Registers (a0) :=
        Unsigned_64 (File_Handle.all.File_Handle_Id);
      Result := Success;
      return;

      <<Syscall_Unsuccessful_No_Kernel_Error>>
      Result := Syscall_Unsuccessful_Without_Kernel_Error;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Handle_Open_File_Syscall");
         Result := Constraint_Exception;
   end Handle_Open_File_Syscall;

   procedure Handle_Seek_File_Syscall
     (Process : in out Process_Control_Block_T; Result : out Function_Result)
   is
      Trap_Context : Process_Context_T
      with
        Import,
        Convention => C,
        Alignment  => 1,
        Address    => Process.Trap_Context_Addr;

      File_Handle_Id : Process_File_Handle_Id_T := 0;

      File_Handle : Process_File_Handle_Access := null;
      New_Offset  : Unsigned_64 := 0;
   begin
      File_Handle_Id :=
        Process_File_Handle_Id_T (Trap_Context.Gp_Registers (a1));
      New_Offset := Trap_Context.Gp_Registers (a2);

      --  This function returns 'Not_Found' if the file handle is not found.
      --  any other error is treated as a kernel error.
      Find_File_Handle
        (Process.Process_Id, File_Handle_Id, File_Handle, Result);
      if Result = Not_Found then
         Log_Error ("File handle not found");

         Trap_Context.Gp_Registers (a0) :=
           Syscall_Error_Result_To_Unsigned_64 (-EBADF);

         goto Syscall_Unsuccessful_No_Kernel_Error;
      elsif Is_Error (Result) then
         --  Error already printed.
         return;
      end if;

      --  In the case of an invalid offset beyond the end of the file, the
      --  Filesystems.Seek_File procedure will adjust the offset to the end
      --  of the file, and return a success result.
      Seek_File (File_Handle, New_Offset, Result);
      if Is_Error (Result) then
         Log_Error ("Error seeking file: " & Result'Image);
         return;
      end if;

      Trap_Context.Gp_Registers (a0) := Syscall_Result_Success;
      Result := Success;
      return;

      <<Syscall_Unsuccessful_No_Kernel_Error>>
      Result := Syscall_Unsuccessful_Without_Kernel_Error;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Handle_Seek_File_Syscall");
         Result := Constraint_Exception;
   end Handle_Seek_File_Syscall;

   procedure Handle_Read_File_Syscall
     (Process : in out Process_Control_Block_T; Result : out Function_Result)
   is
      Trap_Context : Process_Context_T
      with
        Import,
        Convention => C,
        Alignment  => 1,
        Address    => Process.Trap_Context_Addr;

      File_Handle_Id : Process_File_Handle_Id_T := 0;
      Buffer_Address : Virtual_Address_T := Null_Address;
      Bytes_To_Read  : Natural := 0;
      Bytes_Read     : Natural := 0;

      File_Handle : Process_File_Handle_Access := null;
   begin
      Log_Debug ("User Mode Syscall: Read File", Logging_Tags);

      File_Handle_Id :=
        Process_File_Handle_Id_T (Trap_Context.Gp_Registers (a1));

      Buffer_Address :=
        Unsigned_64_To_Address (Trap_Context.Gp_Registers (a2));

      Bytes_To_Read := Natural (Trap_Context.Gp_Registers (a3));
      if Bytes_To_Read = 0 then
         Log_Error ("Invalid bytes to read: " & Bytes_To_Read'Image);
         Bytes_Read := 0;
         goto Return_Bytes_Read;
      end if;

      if not Is_Valid_Userspace_Address_Range (Buffer_Address, Bytes_To_Read)
      then
         Log_Error ("Invalid non-userspace address range");

         Trap_Context.Gp_Registers (a0) :=
           Syscall_Error_Result_To_Unsigned_64 (-EFAULT);

         goto Syscall_Unsuccessful_No_Kernel_Error;
      end if;

      Find_File_Handle
        (Process.Process_Id, File_Handle_Id, File_Handle, Result);
      if Is_Error (Result) then
         Log_Error ("Error finding file handle: " & Result'Image);

         Trap_Context.Gp_Registers (a0) :=
           Syscall_Error_Result_To_Unsigned_64 (-EBADF);

         goto Syscall_Unsuccessful_No_Kernel_Error;
      end if;

      Filesystems.Read_File
        (Process,
         File_Handle,
         Buffer_Address,
         Bytes_To_Read,
         Bytes_Read,
         Result);
      if Is_Error (Result) then
         Log_Error ("Error reading file: " & Result'Image);
         return;
      end if;

      <<Return_Bytes_Read>>
      Trap_Context.Gp_Registers (a0) := Unsigned_64 (Bytes_Read);
      Result := Success;
      return;

      <<Syscall_Unsuccessful_No_Kernel_Error>>
      Result := Syscall_Unsuccessful_Without_Kernel_Error;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Handle_Read_File_Syscall");
         Result := Constraint_Exception;
   end Handle_Read_File_Syscall;

   procedure Handle_Close_File_Syscall
     (Process : in out Process_Control_Block_T; Result : out Function_Result)
   is
      Trap_Context : Process_Context_T
      with
        Import,
        Convention => C,
        Alignment  => 1,
        Address    => Process.Trap_Context_Addr;

      File_Handle_Id : Process_File_Handle_Id_T := 0;
      File_Handle    : Process_File_Handle_Access := null;
   begin
      Log_Debug ("User Mode Syscall: Close File", Logging_Tags);

      File_Handle_Id :=
        Process_File_Handle_Id_T (Trap_Context.Gp_Registers (a1));

      Find_File_Handle
        (Process.Process_Id, File_Handle_Id, File_Handle, Result);
      if Is_Error (Result) then
         Log_Error ("Error finding file handle: " & Result'Image);

         Trap_Context.Gp_Registers (a0) :=
           Syscall_Error_Result_To_Unsigned_64 (-EBADF);

         goto Syscall_Unsuccessful_No_Kernel_Error;
      end if;

      Close_File (File_Handle, Result);
      if Is_Error (Result) then
         Log_Error ("Error closing file: " & Result'Image);
         return;
      end if;

      Log_Debug ("Closed file handle", Logging_Tags);

      Trap_Context.Gp_Registers (a0) := Syscall_Result_Success;
      Result := Success;
      return;

      <<Syscall_Unsuccessful_No_Kernel_Error>>
      Result := Syscall_Unsuccessful_Without_Kernel_Error;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Handle_Close_File_Syscall");
         Result := Constraint_Exception;
   end Handle_Close_File_Syscall;

   procedure Handle_Write_File_Syscall
     (Process : in out Process_Control_Block_T; Result : out Function_Result)
   is
      Trap_Context : Process_Context_T
      with
        Import,
        Convention => C,
        Alignment  => 1,
        Address    => Process.Trap_Context_Addr;

      File_Handle_Id : Process_File_Handle_Id_T := 0;
      Buffer_Address : Virtual_Address_T := Null_Address;
      Bytes_To_Write : Natural := 0;
      Bytes_Written  : Natural := 0;

      File_Handle : Process_File_Handle_Access := null;
   begin
      Log_Debug ("User Mode Syscall: Write File", Logging_Tags);

      File_Handle_Id :=
        Process_File_Handle_Id_T (Trap_Context.Gp_Registers (a1));

      Buffer_Address :=
        Unsigned_64_To_Address (Trap_Context.Gp_Registers (a2));

      Bytes_To_Write := Natural (Trap_Context.Gp_Registers (a3));
      if Bytes_To_Write = 0 then
         Log_Error ("Invalid bytes to write: " & Bytes_To_Write'Image);
         Bytes_Written := 0;
         goto Return_Bytes_Written;
      end if;

      if not Is_Valid_Userspace_Address_Range (Buffer_Address, Bytes_To_Write)
      then
         Log_Error ("Invalid non-userspace address range");

         Trap_Context.Gp_Registers (a0) :=
           Syscall_Error_Result_To_Unsigned_64 (-EFAULT);

         goto Syscall_Unsuccessful_No_Kernel_Error;
      end if;

      Find_File_Handle
        (Process.Process_Id, File_Handle_Id, File_Handle, Result);
      if Is_Error (Result) then
         Log_Error ("Error finding file handle: " & Result'Image);

         Trap_Context.Gp_Registers (a0) :=
           Syscall_Error_Result_To_Unsigned_64 (-EBADF);

         goto Syscall_Unsuccessful_No_Kernel_Error;
      end if;

      Filesystems.Write_File
        (Process,
         File_Handle,
         Buffer_Address,
         Bytes_To_Write,
         Bytes_Written,
         Result);
      if Is_Error (Result) then
         return;
      end if;

      <<Return_Bytes_Written>>
      Trap_Context.Gp_Registers (a0) := Unsigned_64 (Bytes_Written);
      Result := Success;
      return;

      <<Syscall_Unsuccessful_No_Kernel_Error>>
      Result := Syscall_Unsuccessful_Without_Kernel_Error;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Handle_Write_File_Syscall");
         Result := Constraint_Exception;
   end Handle_Write_File_Syscall;

   procedure Handle_Truncate_File_Syscall
     (Process : in out Process_Control_Block_T; Result : out Function_Result)
   is
      Trap_Context : Process_Context_T
      with
        Import,
        Convention => C,
        Alignment  => 1,
        Address    => Process.Trap_Context_Addr;

      File_Handle_Id  : Process_File_Handle_Id_T := 0;
      New_End_Of_File : Unsigned_64 := 0;
      File_Handle     : Process_File_Handle_Access := null;
   begin
      Log_Debug ("User Mode Syscall: Truncate File", Logging_Tags);

      File_Handle_Id :=
        Process_File_Handle_Id_T (Trap_Context.Gp_Registers (a1));

      Find_File_Handle
        (Process.Process_Id, File_Handle_Id, File_Handle, Result);
      if Is_Error (Result) then
         Log_Error ("Error finding file handle: " & Result'Image);

         Trap_Context.Gp_Registers (a0) :=
           Syscall_Error_Result_To_Unsigned_64 (-EBADF);

         Result := Syscall_Unsuccessful_Without_Kernel_Error;
         return;
      end if;

      New_End_Of_File := Trap_Context.Gp_Registers (a2);

      Filesystems.Truncate_File
        (Process, File_Handle, New_End_Of_File, Result);
      if Is_Error (Result) then
         return;
      end if;

      Trap_Context.Gp_Registers (a0) := Unsigned_64 (0);
      Result := Success;
      return;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Handle_Truncate_File_Syscall");
         Result := Constraint_Exception;
   end Handle_Truncate_File_Syscall;
end System_Calls.Files;
