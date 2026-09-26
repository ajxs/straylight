with Devices.UART;
with Logging; use Logging;

package body Devices.Console is
   procedure Read_Bytes
     (Device     : in out Device_T;
      Process    : in out Process_Control_Block_T;
      Buffer     : out Storage_Array;
      Bytes_Read : out Storage_Count;
      Result     : out Function_Result) is
   begin
      if Device.Device_Class /= Device_Class_Console
        or else Device.Console_Backend_Device = null
      then
         Result := Invalid_Argument;
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
            Result := Not_Supported;
      end case;

   exception
      when Constraint_Error =>
         Log_Constraint_Error;
         Result := Constraint_Exception;
   end Read_Bytes;

   procedure Put_Bytes (Device : Device_T; Data : Storage_Array) is
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
   end Put_Bytes;
end Devices.Console;
