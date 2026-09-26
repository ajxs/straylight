with Devices.UART;
with Logging;           use Logging;
with Memory.Allocators; use Memory.Allocators;
with Memory.Kernel;     use Memory.Kernel;

package body Devices.Console is
   function Is_Correct_Device_Class (Device : Device_T) return Boolean is
   begin
      return
        Device.Device_Class = Device_Class_Console
        and then Device.Console_Backend_Device /= null;
   exception
      when Constraint_Error =>
         Log_Constraint_Error;
         return False;
   end Is_Correct_Device_Class;

   function Is_Device_Initialised (Device : Device_T) return Boolean is
   begin
      return
        Device.Line_Buffer_Address /= Null_Address
        and then Device.Line_Buffer_Size /= 0;
   exception
      when Constraint_Error =>
         Log_Constraint_Error;
         return False;
   end Is_Device_Initialised;

   function Is_Device_Valid_And_Initialised (Device : Device_T) return Boolean
   is (Is_Correct_Device_Class (Device)
       and then Is_Device_Initialised (Device));

   procedure Allocate_Line_Buffer
     (Device : in out Device_T; Result : out Function_Result)
   is
      Allocation_Result : Memory_Allocation_Result;
   begin
      Allocate_Pages (1, Allocation_Result, Result);
      if Is_Error (Result) then
         return;
      end if;

      Device.Line_Buffer_Address := Allocation_Result.Virtual_Address;
      Device.Line_Buffer_Size := 4096;
      Device.Line_Buffer_Offset_Read := 0;
      Device.Line_Buffer_Offset_Write := 0;

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Constraint_Error;
         Result := Constraint_Exception;
   end Allocate_Line_Buffer;

   procedure Read_Bytes
     (Device     : in out Device_T;
      Process    : in out Process_Control_Block_T;
      Buffer     : out Storage_Array;
      Bytes_Read : out Storage_Count;
      Result     : out Function_Result) is
   begin
      if not Is_Device_Valid_And_Initialised (Device) then
         Result := Not_Initialised;
         Bytes_Read := 0;
         return;
      end if;

      case Device.Console_Backend_Device.all.Device_Class is
         when Device_Class_Serial =>
            Devices.UART.Read_Bytes
              (Device.Console_Backend_Device.all,
               Process,
               Buffer,
               Bytes_Read,
               Result);

         when others              =>
            Bytes_Read := 0;
            Result := Not_Supported;
      end case;

   exception
      when Constraint_Error =>
         Log_Constraint_Error;
         Bytes_Read := 0;
         Result := Constraint_Exception;
   end Read_Bytes;

   procedure Put_Bytes_Unlocked (Device : Device_T; Data : Storage_Array) is
   begin
      if Device.Device_Class /= Device_Class_Console
        or else Device.Console_Backend_Device = null
      then
         return;
      end if;

      case Device.Console_Backend_Device.all.Device_Class is
         when Device_Class_Serial =>
            Devices.UART.Put_Bytes (Device.Console_Backend_Device.all, Data);

         when others              =>
            null;

      end case;
   exception
      when Constraint_Error =>
         Log_Constraint_Error;
   end Put_Bytes_Unlocked;

   procedure Put_Bytes (Device : in out Device_T; Data : Storage_Array) is
   begin
      Acquire_Spinlock (Device.Spinlock);
      Put_Bytes_Unlocked (Device, Data);
      Release_Spinlock (Device.Spinlock);
   end Put_Bytes;

   procedure Initialise
     (Device : in out Device_T; Result : out Function_Result) is
   begin
      if not Is_Correct_Device_Class (Device) then
         Result := Invalid_Argument;
         return;
      end if;

      Allocate_Line_Buffer (Device, Result);
      if Is_Error (Result) then
         return;
      end if;
   end Initialise;
end Devices.Console;
