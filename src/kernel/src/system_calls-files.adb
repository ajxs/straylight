-------------------------------------------------------------------------------
--  Copyright (c) 2026, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------
with System.Storage_Elements; use System.Storage_Elements;

with Logging;            use Logging;
with Memory;             use Memory;
with RISCV;              use RISCV;
with System_Calls.Errno; use System_Calls.Errno;
with Utilities;          use Utilities;

package body System_Calls.Files is
   Logging_Enabled : constant Boolean :=
     Debug_Processes or else Debug_System_Calls;

   ----------------------------------------------------------------------------
   --  Copies a path argument from userspace to kernel memory.
   --  Returns -ENAMETOOLONG if the full path is longer than the output
   --  variable can hold.
   --  Returns -EFAULT if its address range is outside userspace.
   --  On these errors, Syscall_Result holds the errno and Result is
   --  Syscall_Unsuccessful_Without_Kernel_Error.
   ----------------------------------------------------------------------------
   procedure Copy_Path_From_Userspace
     (Path_String_Address : Virtual_Address_T;
      Path_String_Length  : Unsigned_64;
      Path                : out Fixed_Length_String_T;
      Syscall_Result      : out Unsigned_64;
      Result              : out Function_Result) is
   begin
      --  Perform this check first, so that we can ensure that the path string
      --  length won't overflow any further type conversions.
      if Path_String_Length > Unsigned_64 (Path.Max_Length) then
         Log_Error
           ("Path length exceeds maximum length: " & Path_String_Length'Image);

         Syscall_Result := Syscall_Error_Result_To_Unsigned_64 (-ENAMETOOLONG);
         Result := Syscall_Unsuccessful_Without_Kernel_Error;
         return;
      end if;

      if not Is_Valid_Userspace_Address_Range
               (Path_String_Address, Storage_Count (Path_String_Length))
      then
         Log_Error ("Invalid non-userspace address range");

         Syscall_Result := Syscall_Error_Result_To_Unsigned_64 (-EFAULT);
         Result := Syscall_Unsuccessful_Without_Kernel_Error;
         return;
      end if;

      Copy_User_Path_String : declare
         Path_Length : constant Natural := Natural (Path_String_Length);

         User_Path_String : constant Filesystem_Path_T (1 .. Path_Length)
         with
           Import,
           Convention => C,
           Alignment  => 1,
           Address    => Path_String_Address;
      begin
         Path.Value (1 .. Path_Length) := User_Path_String;
         Path.Byte_Length := Path_Length;
      end Copy_User_Path_String;

      Syscall_Result := 0;
      Result := Success;
   exception
      when Constraint_Error =>
         Log_Constraint_Error;
         Result := Constraint_Exception;
   end Copy_Path_From_Userspace;

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

      File_Handle : Process_File_Handle_Access := null;

      Path : Fixed_Length_String_T (Maximum_Path_String_Argument_Length);
   begin
      pragma
        Debug (Logging_Enabled, Log_Debug ("User Mode Syscall: Open File"));

      File_Open_Flags : constant File_Open_Flags_T :=
        Unsigned_64_To_File_Open_Flags (Trap_Context.Gp_Registers (a3));

      Copy_Path_From_Userspace
        (Unsigned_64_To_Address (Trap_Context.Gp_Registers (a1)),
         Trap_Context.Gp_Registers (a2),
         Path,
         Syscall_Result,
         Result);
      if Result /= Success then
         return;
      end if;

      Filesystems.Open_File
        (Process,
         Path.Value (1 .. Path.Byte_Length),
         File_Open_Flags,
         File_Handle,
         Result);
      if Is_Error (Result) then
         Error_Number : constant Syscall_Error_Result_T :=
           (case Result is
              --  Passing an empty path, the path of a file that already
              --  exists, or attempting to create a file whose parent
              --  isn't a directory, will return an Invalid_Filename
              --  result.
              when File_Not_Found | Invalid_Filename    => ENOENT,
              when Path_Too_Long                        => ENAMETOOLONG,
              when Not_Supported                        => ENOTSUP,
              --  Truncating a file that is opened read-only.
              when Invalid_File_Permissions             => EINVAL,
              --  The open file table, the node cache, or a FAT directory
              --  is full.
              when No_Free_Entries                      => ENFILE,
              when Device_IO_Error | Invalid_Filesystem => EIO,
              --  Any other result can only be caused by a kernel error.
              when others                               => 0);

         --  If the result can't be mapped to a POSIX errno, it indicates
         --  a kernel error. In this case return immediately.
         if Error_Number = 0 then
            return;
         end if;

         Syscall_Result := Syscall_Error_Result_To_Unsigned_64 (-Error_Number);
         Result := Syscall_Unsuccessful_Without_Kernel_Error;
         return;
      end if;

      pragma Debug (Logging_Enabled, Log_Debug ("Opened file handle"));

      Syscall_Result := Unsigned_64 (File_Handle.all.File_Handle_Id);
      Result := Success;
   exception
      when Constraint_Error =>
         Log_Constraint_Error;
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
         Log_Constraint_Error;
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
      pragma
        Debug (Logging_Enabled, Log_Debug ("User Mode Syscall: Read File"));

      File_Handle_Id : constant Process_File_Handle_Id_T :=
        Process_File_Handle_Id_T (Trap_Context.Gp_Registers (a1));

      Buffer_Address : constant Virtual_Address_T :=
        Unsigned_64_To_Address (Trap_Context.Gp_Registers (a2));

      Bytes_To_Read : constant Unsigned_64 := Trap_Context.Gp_Registers (a3);
      if Bytes_To_Read = 0 then
         Log_Error ("Invalid bytes to read: " & Bytes_To_Read'Image);
         Syscall_Result := Unsigned_64 (Bytes_Read);
         Result := Success;
         return;
      end if;

      if Bytes_To_Read > Filesystem_Max_Read_Write_Byte_Count then
         Log_Error ("Invalid bytes to read: " & Bytes_To_Read'Image);

         Syscall_Result := Syscall_Error_Result_To_Unsigned_64 (-EINVAL);
         Result := Syscall_Unsuccessful_Without_Kernel_Error;
         return;
      end if;

      if not Is_Valid_Userspace_Address_Range
               (Buffer_Address, Storage_Count (Bytes_To_Read))
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

      Filesystems.Read_File
        (Process,
         File_Handle,
         Buffer_Address,
         Integer (Bytes_To_Read),
         Bytes_Read,
         Result);
      if Result = Not_Supported then
         Syscall_Result := Syscall_Error_Result_To_Unsigned_64 (-ENOTSUP);
         Result := Syscall_Unsuccessful_Without_Kernel_Error;
         return;
      elsif Result = Invalid_File_Permissions then
         Syscall_Result := Syscall_Error_Result_To_Unsigned_64 (-EBADF);
         Result := Syscall_Unsuccessful_Without_Kernel_Error;
         return;
      elsif Result = Invalid_File_Size then
         Syscall_Result := Syscall_Error_Result_To_Unsigned_64 (-EINVAL);
         Result := Syscall_Unsuccessful_Without_Kernel_Error;
         return;
      elsif Result = Device_IO_Error or else Result = Invalid_Filesystem then
         Syscall_Result := Syscall_Error_Result_To_Unsigned_64 (-EIO);
         Result := Syscall_Unsuccessful_Without_Kernel_Error;
         return;
      elsif Is_Error (Result) then
         return;
      end if;

      Syscall_Result := Unsigned_64 (Bytes_Read);
   exception
      when Constraint_Error =>
         Log_Constraint_Error;
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
      pragma
        Debug (Logging_Enabled, Log_Debug ("User Mode Syscall: Close File"));

      File_Handle_Id : constant Process_File_Handle_Id_T :=
        Process_File_Handle_Id_T (Trap_Context.Gp_Registers (a1));

      Find_File_Handle
        (Process.Process_Id, File_Handle_Id, File_Handle, Result);
      if Is_Error (Result) then
         Syscall_Result := Syscall_Error_Result_To_Unsigned_64 (-EBADF);
         Result := Syscall_Unsuccessful_Without_Kernel_Error;
         return;
      end if;

      --  Result set by this call.
      Close_File (File_Handle, Result);

      Syscall_Result := 0;
   exception
      when Constraint_Error =>
         Log_Constraint_Error;
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
      pragma
        Debug (Logging_Enabled, Log_Debug ("User Mode Syscall: Write File"));

      File_Handle_Id : constant Process_File_Handle_Id_T :=
        Process_File_Handle_Id_T (Trap_Context.Gp_Registers (a1));

      Buffer_Address : constant Virtual_Address_T :=
        Unsigned_64_To_Address (Trap_Context.Gp_Registers (a2));

      Bytes_To_Write : constant Unsigned_64 := Trap_Context.Gp_Registers (a3);

      --  It's important to validate the number of bytes to write before
      --  attempting to cast it to any other type, otherwise it could raise
      --  a Constraint_Error exception, and lead to a panic.
      if Bytes_To_Write = 0 then
         Bytes_Written := 0;
         Syscall_Result := 0;
         Result := Success;
         return;
      elsif Bytes_To_Write > Filesystem_Max_Read_Write_Byte_Count then
         Syscall_Result := Syscall_Error_Result_To_Unsigned_64 (-EINVAL);
         Result := Syscall_Unsuccessful_Without_Kernel_Error;
         return;
      end if;

      if not Is_Valid_Userspace_Address_Range
               (Buffer_Address, Storage_Count (Bytes_To_Write))
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
         Integer (Bytes_To_Write),
         Bytes_Written,
         Result);
      if Result = Not_Supported then
         Syscall_Result := Syscall_Error_Result_To_Unsigned_64 (-ENOTSUP);
         Result := Syscall_Unsuccessful_Without_Kernel_Error;
         return;
      elsif Result = Invalid_File_Permissions then
         Syscall_Result := Syscall_Error_Result_To_Unsigned_64 (-EBADF);
         Result := Syscall_Unsuccessful_Without_Kernel_Error;
         return;
      elsif Result = Invalid_File_Size then
         Syscall_Result := Syscall_Error_Result_To_Unsigned_64 (-EINVAL);
         Result := Syscall_Unsuccessful_Without_Kernel_Error;
         return;
      elsif Result = Device_IO_Error or else Result = Invalid_Filesystem then
         Syscall_Result := Syscall_Error_Result_To_Unsigned_64 (-EIO);
         Result := Syscall_Unsuccessful_Without_Kernel_Error;
         return;
      elsif Is_Error (Result) then
         return;
      end if;

      Syscall_Result := Unsigned_64 (Bytes_Written);
   exception
      when Constraint_Error =>
         Log_Constraint_Error;
         Result := Constraint_Exception;
   end Handle_Write_File_Syscall;

   --  Determines whether an incoming file size argument is safe to cast to
   --  the type used internally to denote file sizes.
   function Is_Incoming_File_Size_Argument_Safe_To_Cast
     (Incoming_File_Size : Unsigned_64) return Boolean
   is (Incoming_File_Size <= Unsigned_64 (Storage_Count'Last));

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
      pragma
        Debug
          (Logging_Enabled, Log_Debug ("User Mode Syscall: Truncate File"));

      File_Handle_Id : constant Process_File_Handle_Id_T :=
        Process_File_Handle_Id_T (Trap_Context.Gp_Registers (a1));

      Find_File_Handle
        (Process.Process_Id, File_Handle_Id, File_Handle, Result);
      if Is_Error (Result) then
         Syscall_Result := Syscall_Error_Result_To_Unsigned_64 (-EBADF);
         Result := Syscall_Unsuccessful_Without_Kernel_Error;
         return;
      end if;

      New_End_Of_File : constant Unsigned_64 := Trap_Context.Gp_Registers (a2);

      --  Validate that the incoming new file size won't raise a constraint
      --  error before casting it.
      if not Is_Incoming_File_Size_Argument_Safe_To_Cast (New_End_Of_File) then
         Syscall_Result := Syscall_Error_Result_To_Unsigned_64 (-EINVAL);
         Result := Syscall_Unsuccessful_Without_Kernel_Error;
         return;
      end if;

      Filesystems.Truncate_File
        (Process, File_Handle, Storage_Count (New_End_Of_File), Result);
      if Result = Invalid_File_Type or else Result = Invalid_File_Size then
         Syscall_Result := Syscall_Error_Result_To_Unsigned_64 (-EINVAL);
         Result := Syscall_Unsuccessful_Without_Kernel_Error;
         return;
      elsif Result = Invalid_File_Permissions then
         Syscall_Result := Syscall_Error_Result_To_Unsigned_64 (-EBADF);
         Result := Syscall_Unsuccessful_Without_Kernel_Error;
         return;
      elsif Result = Not_Supported then
         Syscall_Result := Syscall_Error_Result_To_Unsigned_64 (-ENOTSUP);
         Result := Syscall_Unsuccessful_Without_Kernel_Error;
         return;
      elsif Result = Device_IO_Error or else Result = Invalid_Filesystem then
         Syscall_Result := Syscall_Error_Result_To_Unsigned_64 (-EIO);
         Result := Syscall_Unsuccessful_Without_Kernel_Error;
         return;
      elsif Is_Error (Result) then
         return;
      end if;

      Syscall_Result := 0;
   exception
      when Constraint_Error =>
         Log_Constraint_Error;
         Result := Constraint_Exception;
   end Handle_Truncate_File_Syscall;

   procedure Handle_Get_Working_Directory_Syscall
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
   begin
      pragma
        Debug
          (Logging_Enabled,
           Log_Debug ("User Mode Syscall: Get Working Directory"));

      Buffer_Address : constant Virtual_Address_T :=
        Unsigned_64_To_Address (Trap_Context.Gp_Registers (a1));

      Buffer_Size : constant Unsigned_64 := Trap_Context.Gp_Registers (a2);

      Path_Length : constant Natural := Process.Working_Directory.Byte_Length;

      if Trap_Context.Gp_Registers (a1) = 0 then
         Log_Error ("Null buffer address");

         Syscall_Result := Syscall_Error_Result_To_Unsigned_64 (-EFAULT);
         Result := Syscall_Unsuccessful_Without_Kernel_Error;
         return;
      end if;

      if Buffer_Size < Unsigned_64 (Path_Length) then
         Log_Error ("Buffer size too small for working directory path");

         Syscall_Result := Syscall_Error_Result_To_Unsigned_64 (-ERANGE);
         Result := Syscall_Unsuccessful_Without_Kernel_Error;
         return;
      end if;

      --  Validate that the buffer address and size describe a range within the
      --  valid userspace range.
      if not Is_Valid_Userspace_Address_Range
               (Buffer_Address, Storage_Count (Path_Length))
      then
         Log_Error ("Invalid non-userspace address range");

         Syscall_Result := Syscall_Error_Result_To_Unsigned_64 (-EFAULT);
         Result := Syscall_Unsuccessful_Without_Kernel_Error;
         return;
      end if;

      --  Copy the working directory path into the user-provided buffer.
      declare
         User_Buffer : String (1 .. Path_Length)
         with Import, Alignment => 1, Address => Buffer_Address;
      begin
         User_Buffer := Process.Working_Directory.Value (1 .. Path_Length);
      end;

      Syscall_Result := Unsigned_64 (Path_Length);
      Result := Success;
   exception
      when Constraint_Error =>
         Log_Constraint_Error;
         Result := Constraint_Exception;
   end Handle_Get_Working_Directory_Syscall;

   procedure Handle_Change_Working_Directory_Syscall
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

      Path : Fixed_Length_String_T (Maximum_Path_String_Argument_Length);
   begin
      pragma
        Debug
          (Logging_Enabled,
           Log_Debug ("User Mode Syscall: Change Working Directory"));

      Copy_Path_From_Userspace
        (Unsigned_64_To_Address (Trap_Context.Gp_Registers (a1)),
         Trap_Context.Gp_Registers (a2),
         Path,
         Syscall_Result,
         Result);
      if Result /= Success then
         return;
      end if;

      Filesystems.Change_Working_Directory
        (Process, Path.Value (1 .. Path.Byte_Length), Result);
      if Is_Error (Result) then
         Error_Number : constant Syscall_Error_Result_T :=
           (case Result is
              --  An empty path returns an Invalid_Filename result.
              when File_Not_Found | Invalid_Filename    => ENOENT,
              when Invalid_File_Type                    => ENOTDIR,
              when Path_Too_Long                        => ENAMETOOLONG,
              --  The open file table, or the node cache is full.
              when No_Free_Entries                      => ENFILE,
              when Device_IO_Error | Invalid_Filesystem => EIO,
              --  Handles unsupported filesystems.
              when Not_Supported                        => ENOTSUP,
              --  Any other result can only be caused by a kernel error.
              when others                               => 0);

         if Error_Number = 0 then
            return;
         end if;

         Syscall_Result := Syscall_Error_Result_To_Unsigned_64 (-Error_Number);
         Result := Syscall_Unsuccessful_Without_Kernel_Error;
         return;
      end if;

      Syscall_Result := 0;
      Result := Success;
   exception
      when Constraint_Error =>
         Log_Constraint_Error;
         Result := Constraint_Exception;
   end Handle_Change_Working_Directory_Syscall;

end System_Calls.Files;
