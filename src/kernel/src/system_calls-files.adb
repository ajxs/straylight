-------------------------------------------------------------------------------
--  Copyright (c) 2026, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------
with Memory; use Memory;
with RISCV;  use RISCV;

package body System_Calls.Files is
   procedure Handle_Open_File_Syscall
     (Process        : in out Process_Control_Block_T;
      Syscall_Result : out Unsigned_64;
      Result         : out Function_Result)
   is
      Trap_Context : constant Process_Context_T
      with
        Import,
        Convention => C,
        Alignment  => 1,
        Address    => Process.Trap_Context_Addr;

      Maximum_Path_String_Length : constant Integer := 256;

      File_Handle : Process_File_Handle_Access := null;
   begin
      Log_Debug ("User Mode Syscall: Open File", Logging_Tags);

      Path_String_Address : constant Virtual_Address_T :=
        Unsigned_64_To_Address (Trap_Context.Gp_Registers (a1));
      Path_String_Length : constant Integer :=
        Integer (Trap_Context.Gp_Registers (a2));
      File_Open_Flags : constant File_Open_Flags_T :=
        Unsigned_64_To_File_Open_Flags (Trap_Context.Gp_Registers (a3));

      if not Is_Valid_Userspace_Address_Range
               (Path_String_Address, Path_String_Length)
      then
         Log_Error ("Invalid non-userspace address range");

         Syscall_Result := Syscall_Error_Result_To_Unsigned_64 (-EFAULT);
         Result := Syscall_Unsuccessful_Without_Kernel_Error;
         return;
      end if;

      if Path_String_Length > Maximum_Path_String_Length then
         Log_Error
           ("Path length exceeds maximum length: " & Path_String_Length'Image,
            Logging_Tags);

         Syscall_Result := Syscall_Error_Result_To_Unsigned_64 (-ENAMETOOLONG);
         Result := Syscall_Unsuccessful_Without_Kernel_Error;
         return;
      end if;

      Read_Path_String_And_Open_File : declare
         User_Path_String :
           constant Filesystem_Path_T (1 .. Path_String_Length)
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
            Syscall_Result := Syscall_Error_Result_To_Unsigned_64 (-ENOENT);
            Result := Syscall_Unsuccessful_Without_Kernel_Error;
            return;
         elsif Is_Error (Result) then
            return;
         end if;

         Log_Debug ("Opened file handle", Logging_Tags);
      end Read_Path_String_And_Open_File;

      Syscall_Result := Unsigned_64 (File_Handle.all.File_Handle_Id);
      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Handle_Open_File_Syscall");
         Result := Constraint_Exception;
   end Handle_Open_File_Syscall;

   procedure Handle_Seek_File_Syscall
     (Process        : in out Process_Control_Block_T;
      Syscall_Result : out Unsigned_64;
      Result         : out Function_Result)
   is
      Trap_Context : constant Process_Context_T
      with
        Import,
        Convention => C,
        Alignment  => 1,
        Address    => Process.Trap_Context_Addr;

      File_Handle : Process_File_Handle_Access := null;
   begin
      File_Handle_Id : constant Process_File_Handle_Id_T :=
        Process_File_Handle_Id_T (Trap_Context.Gp_Registers (a1));
      New_Offset : constant Unsigned_64 := Trap_Context.Gp_Registers (a2);

      --  This function returns 'Not_Found' if the file handle is not found.
      --  any other error is treated as a kernel error.
      Find_File_Handle
        (Process.Process_Id, File_Handle_Id, File_Handle, Result);
      if Result = Not_Found then
         Log_Error ("File handle not found");

         Syscall_Result := Syscall_Error_Result_To_Unsigned_64 (-EBADF);
         Result := Syscall_Unsuccessful_Without_Kernel_Error;
         return;
      elsif Is_Error (Result) then
         --  Error already printed.
         return;
      end if;

      --  In the case of an invalid offset beyond the end of the file, the
      --  Filesystems.Seek_File procedure will adjust the offset to the end
      --  of the file, and return a success result.
      --  Result set by this call.
      Seek_File (File_Handle, New_Offset, Result);

      Syscall_Result := 0;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Handle_Seek_File_Syscall");
         Result := Constraint_Exception;
   end Handle_Seek_File_Syscall;

   procedure Handle_Read_File_Syscall
     (Process        : in out Process_Control_Block_T;
      Syscall_Result : out Unsigned_64;
      Result         : out Function_Result)
   is
      Trap_Context : constant Process_Context_T
      with
        Import,
        Convention => C,
        Alignment  => 1,
        Address    => Process.Trap_Context_Addr;

      Bytes_Read : Natural := 0;

      File_Handle : Process_File_Handle_Access := null;
   begin
      Log_Debug ("User Mode Syscall: Read File", Logging_Tags);

      File_Handle_Id : constant Process_File_Handle_Id_T :=
        Process_File_Handle_Id_T (Trap_Context.Gp_Registers (a1));

      Buffer_Address : constant Virtual_Address_T :=
        Unsigned_64_To_Address (Trap_Context.Gp_Registers (a2));

      Bytes_To_Read : constant Natural :=
        Natural (Trap_Context.Gp_Registers (a3));
      if Bytes_To_Read = 0 then
         Log_Error ("Invalid bytes to read: " & Bytes_To_Read'Image);
         Syscall_Result := Unsigned_64 (Bytes_Read);
         Result := Success;
         return;
      end if;

      if not Is_Valid_Userspace_Address_Range (Buffer_Address, Bytes_To_Read)
      then
         Log_Error ("Invalid non-userspace address range");

         Syscall_Result := Syscall_Error_Result_To_Unsigned_64 (-EFAULT);
         Result := Syscall_Unsuccessful_Without_Kernel_Error;
         return;
      end if;

      Find_File_Handle
        (Process.Process_Id, File_Handle_Id, File_Handle, Result);
      if Is_Error (Result) then
         Log_Error ("Error finding file handle: " & Result'Image);

         Syscall_Result := Syscall_Error_Result_To_Unsigned_64 (-EBADF);
         Result := Syscall_Unsuccessful_Without_Kernel_Error;
         return;
      end if;

      --  Result set by this call.
      Filesystems.Read_File
        (Process,
         File_Handle,
         Buffer_Address,
         Bytes_To_Read,
         Bytes_Read,
         Result);

      Syscall_Result := Unsigned_64 (Bytes_Read);
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Handle_Read_File_Syscall");
         Result := Constraint_Exception;
   end Handle_Read_File_Syscall;

   procedure Handle_Close_File_Syscall
     (Process        : in out Process_Control_Block_T;
      Syscall_Result : out Unsigned_64;
      Result         : out Function_Result)
   is
      Trap_Context : constant Process_Context_T
      with
        Import,
        Convention => C,
        Alignment  => 1,
        Address    => Process.Trap_Context_Addr;

      File_Handle : Process_File_Handle_Access := null;
   begin
      Log_Debug ("User Mode Syscall: Close File", Logging_Tags);

      File_Handle_Id : constant Process_File_Handle_Id_T :=
        Process_File_Handle_Id_T (Trap_Context.Gp_Registers (a1));

      Find_File_Handle
        (Process.Process_Id, File_Handle_Id, File_Handle, Result);
      if Is_Error (Result) then
         Log_Error ("Error finding file handle: " & Result'Image);

         Syscall_Result := Syscall_Error_Result_To_Unsigned_64 (-EBADF);
         Result := Syscall_Unsuccessful_Without_Kernel_Error;
         return;
      end if;

      --  Result set by this call.
      Close_File (File_Handle, Result);

      Syscall_Result := 0;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Handle_Close_File_Syscall");
         Result := Constraint_Exception;
   end Handle_Close_File_Syscall;

   procedure Handle_Write_File_Syscall
     (Process        : in out Process_Control_Block_T;
      Syscall_Result : out Unsigned_64;
      Result         : out Function_Result)
   is
      Trap_Context : constant Process_Context_T
      with
        Import,
        Convention => C,
        Alignment  => 1,
        Address    => Process.Trap_Context_Addr;

      Bytes_Written : Natural := 0;

      File_Handle : Process_File_Handle_Access := null;
   begin
      Log_Debug ("User Mode Syscall: Write File", Logging_Tags);

      File_Handle_Id : constant Process_File_Handle_Id_T :=
        Process_File_Handle_Id_T (Trap_Context.Gp_Registers (a1));

      Buffer_Address : constant Virtual_Address_T :=
        Unsigned_64_To_Address (Trap_Context.Gp_Registers (a2));

      Bytes_To_Write : constant Natural :=
        Natural (Trap_Context.Gp_Registers (a3));
      if Bytes_To_Write = 0 then
         Log_Error ("Invalid bytes to write: " & Bytes_To_Write'Image);
         Bytes_Written := 0;
         Syscall_Result := Unsigned_64 (Bytes_Written);
         Result := Success;
         return;
      end if;

      if not Is_Valid_Userspace_Address_Range (Buffer_Address, Bytes_To_Write)
      then
         Log_Error ("Invalid non-userspace address range");
         Syscall_Result := Syscall_Error_Result_To_Unsigned_64 (-EFAULT);
         Result := Syscall_Unsuccessful_Without_Kernel_Error;
         return;
      end if;

      Find_File_Handle
        (Process.Process_Id, File_Handle_Id, File_Handle, Result);
      if Is_Error (Result) then
         Log_Error ("Error finding file handle: " & Result'Image);
         Syscall_Result := Syscall_Error_Result_To_Unsigned_64 (-EBADF);
         Result := Syscall_Unsuccessful_Without_Kernel_Error;
         return;
      end if;

      --  Result set by this call.
      Filesystems.Write_File
        (Process,
         File_Handle,
         Buffer_Address,
         Bytes_To_Write,
         Bytes_Written,
         Result);

      Syscall_Result := Unsigned_64 (Bytes_Written);
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Handle_Write_File_Syscall");
         Result := Constraint_Exception;
   end Handle_Write_File_Syscall;

   procedure Handle_Truncate_File_Syscall
     (Process        : in out Process_Control_Block_T;
      Syscall_Result : out Unsigned_64;
      Result         : out Function_Result)
   is
      Trap_Context : constant Process_Context_T
      with
        Import,
        Convention => C,
        Alignment  => 1,
        Address    => Process.Trap_Context_Addr;

      File_Handle : Process_File_Handle_Access := null;
   begin
      Log_Debug ("User Mode Syscall: Truncate File", Logging_Tags);

      File_Handle_Id : constant Process_File_Handle_Id_T :=
        Process_File_Handle_Id_T (Trap_Context.Gp_Registers (a1));

      Find_File_Handle
        (Process.Process_Id, File_Handle_Id, File_Handle, Result);
      if Is_Error (Result) then
         Log_Error ("Error finding file handle: " & Result'Image);
         Syscall_Result := Syscall_Error_Result_To_Unsigned_64 (-EBADF);
         Result := Syscall_Unsuccessful_Without_Kernel_Error;
         return;
      end if;

      New_End_Of_File : constant Unsigned_64 := Trap_Context.Gp_Registers (a2);

      --  Result set by this call.
      Filesystems.Truncate_File
        (Process, File_Handle, New_End_Of_File, Result);

      Syscall_Result := 0;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Handle_Truncate_File_Syscall");
         Result := Constraint_Exception;
   end Handle_Truncate_File_Syscall;
end System_Calls.Files;
