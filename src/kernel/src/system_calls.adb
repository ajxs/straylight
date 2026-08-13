-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with System;                  use System;
with System.Storage_Elements; use System.Storage_Elements;

with Devices;
with Devices.Virtio.Graphics;
with Memory;             use Memory;
with RISCV;              use RISCV;
with Processes.Scheduler;
with Hart_State;         use Hart_State;
with System_Calls.Files; use System_Calls.Files;

package body System_Calls is
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

   procedure Handle_Grow_Process_Heap_Syscall
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

      Heap_New_Memory_Amount : constant := 4 * 1024 * 1024;
   begin
      Grow_Process_Heap (Process, Heap_New_Memory_Amount, Result);
      if Is_Error (Result) then
         Syscall_Result := Syscall_Error_Result_To_Unsigned_64 (-ENOMEM);
         Result := Syscall_Unsuccessful_Without_Kernel_Error;
         return;
      end if;

      Syscall_Result := 0;
      Result := Success;
   end Handle_Grow_Process_Heap_Syscall;

   procedure Handle_User_Mode_Syscall
     (Process : in out Process_Control_Block_T; Result : out Function_Result)
   is
      Syscall_Result : Unsigned_64 := 0;

      Trap_Context : Process_Context_T
      with
        Import,
        Convention => C,
        Alignment  => 1,
        Address    => Process.Trap_Context_Addr;
   begin
      --  Retrieve the Syscall number passed in a0.
      Syscall_Number : constant Unsigned_64 := Trap_Context.Gp_Registers (a0);

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

         when Syscall_Open_File          =>
            Handle_Open_File_Syscall (Process, Syscall_Result, Result);

         when Syscall_Read_File          =>
            Handle_Read_File_Syscall (Process, Syscall_Result, Result);

         when Syscall_Seek_File          =>
            Handle_Seek_File_Syscall (Process, Syscall_Result, Result);

         when Syscall_Write_File         =>
            Handle_Write_File_Syscall (Process, Syscall_Result, Result);

         when Syscall_Close_File         =>
            Handle_Close_File_Syscall (Process, Syscall_Result, Result);

         when Syscall_Truncate_File      =>
            Handle_Truncate_File_Syscall (Process, Syscall_Result, Result);

         when Syscall_Update_Framebuffer =>
            Handle_Update_Framebuffer_Syscall
              (Process, Syscall_Result, Result);

         when Syscall_Grow_Process_Heap  =>
            Handle_Grow_Process_Heap_Syscall (Process, Syscall_Result, Result);

         when others                     =>
            Panic ("Unknown Syscall Number: " & Syscall_Number'Image);
      end case;

      if Is_Error (Result) then
         Panic ("Kernel Error in Syscall Handler: " & Result'Image);
      end if;

      Trap_Context.Gp_Registers (a0) := Syscall_Result;

   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Handle_User_Mode_Syscall");
         Result := Constraint_Exception;
   end Handle_User_Mode_Syscall;

   procedure Handle_Update_Framebuffer_Syscall
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

      Graphics_Device renames Devices.System_Devices (6);
   begin
      Log_Debug ("User Mode Syscall: Update Framebuffer", Logging_Tags);

      User_Framebuffer_Address : constant Virtual_Address_T :=
        Unsigned_64_To_Address (Trap_Context.Gp_Registers (a1));

      User_Pixel_Data_X : constant Unsigned_32 :=
        Unsigned_32 (Trap_Context.Gp_Registers (a2));
      User_Pixel_Data_Y : constant Unsigned_32 :=
        Unsigned_32 (Trap_Context.Gp_Registers (a3));
      User_Pixel_Data_Width : constant Unsigned_32 :=
        Unsigned_32 (Trap_Context.Gp_Registers (a4));
      User_Pixel_Data_Height : constant Unsigned_32 :=
        Unsigned_32 (Trap_Context.Gp_Registers (a5));

      --  Reject any rectangle that extends beyond the framebuffer bounds.
      --  The rectangle dimensions are untrusted userspace values that drive
      --  both the row copy below and the GPU transfer offset, so an
      --  out-of-bounds rectangle would read/write past the framebuffer.
      --  Validated first so the offset arithmetic below cannot overflow, and
      --  computed in Unsigned_64 to avoid wraparound on the additions.
      if Unsigned_64 (User_Pixel_Data_X) + Unsigned_64 (User_Pixel_Data_Width)
        > Unsigned_64 (Graphics_Device.Framebuffer_Width)
        or else
          Unsigned_64 (User_Pixel_Data_Y)
          + Unsigned_64 (User_Pixel_Data_Height)
          > Unsigned_64 (Graphics_Device.Framebuffer_Height)
      then
         Log_Error ("Framebuffer rectangle out of bounds", Logging_Tags);

         Syscall_Result := Syscall_Error_Result_To_Unsigned_64 (-EINVAL);
         Result := Syscall_Unsuccessful_Without_Kernel_Error;
         return;
      end if;

      --  An empty rectangle reads and writes nothing, so there is no work to
      --  do. Returning here also guarantees the width and height are non-zero
      --  below, keeping the row loop and the offset arithmetic well-defined.
      if User_Pixel_Data_Width = 0 or else User_Pixel_Data_Height = 0 then
         Syscall_Result := 0;
         Result := Success;
         return;
      end if;

      --  Confirm the region actually read lies within userspace. The pixel
      --  data is laid out at the full framebuffer stride, so the highest byte
      --  read is the end of the rectangle's bottom-most row; validating only
      --  that extent avoids assuming the caller mapped a whole framebuffer.
      Validate_User_Buffer : declare
         Bytes_Per_Pixel : constant := 4;
         Stride          : constant Storage_Count :=
           Storage_Count (Graphics_Device.Framebuffer_Width) * Bytes_Per_Pixel;
         End_Offset      : constant Storage_Count :=
           (Storage_Count (User_Pixel_Data_Y)
            + Storage_Count (User_Pixel_Data_Height)
            - 1)
           * Stride
           + (Storage_Count (User_Pixel_Data_X)
              + Storage_Count (User_Pixel_Data_Width))
             * Bytes_Per_Pixel;
      begin
         if not Is_Valid_Userspace_Address_Range
                  (User_Framebuffer_Address, End_Offset)
         then
            Log_Error ("Invalid non-userspace address range", Logging_Tags);
            Syscall_Result := Syscall_Error_Result_To_Unsigned_64 (-EFAULT);
            Result := Syscall_Unsuccessful_Without_Kernel_Error;
            return;
         end if;
      end Validate_User_Buffer;

      Copy_Pixel_Data : declare
         Bytes_Per_Pixel : constant := 4;
         Stride          : constant Storage_Offset :=
           Storage_Offset (Graphics_Device.Framebuffer_Width)
           * Bytes_Per_Pixel;
         Row_Bytes       : constant Integer :=
           Integer (User_Pixel_Data_Width) * Bytes_Per_Pixel;
      begin
         for Row in 0 .. User_Pixel_Data_Height - 1 loop
            declare
               Row_Offset : constant Storage_Offset :=
                 Storage_Offset (User_Pixel_Data_Y + Row) * Stride
                 + Storage_Offset (User_Pixel_Data_X) * Bytes_Per_Pixel;
            begin
               Copy
                 (Source => User_Framebuffer_Address + Row_Offset,
                  Dest   =>
                    Graphics_Device.Framebuffer_Addresses.Virtual_Address
                    + Row_Offset,
                  Count  => Row_Bytes);
            end;
         end loop;
      end Copy_Pixel_Data;

      Devices.Virtio.Graphics.Transfer_To_Host_2d
        (Process,
         Graphics_Device,
         Graphics_Device.Bus_Info.Virtio.Resource_Id,
         User_Pixel_Data_X,
         User_Pixel_Data_Y,
         User_Pixel_Data_Width,
         User_Pixel_Data_Height,
         Result);
      if Is_Error (Result) then
         return;
      end if;

      Devices.Virtio.Graphics.Resource_Flush
        (Process,
         Graphics_Device,
         Graphics_Device.Bus_Info.Virtio.Resource_Id,
         User_Pixel_Data_X,
         User_Pixel_Data_Y,
         User_Pixel_Data_Width,
         User_Pixel_Data_Height,
         Result);
      if Is_Error (Result) then
         return;
      end if;

      Syscall_Result := 0;
      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Handle_Update_Framebuffer_Syscall");
         Result := Constraint_Exception;
   end Handle_Update_Framebuffer_Syscall;

end System_Calls;
