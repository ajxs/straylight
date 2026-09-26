with Function_Results; use Function_Results;

package Devices.Console
  with Preelaborate
is
   procedure Read_Bytes
     (Device     : in out Device_T;
      Process    : in out Process_Control_Block_T;
      Buffer     : out Storage_Array;
      Bytes_Read : out Storage_Count;
      Result     : out Function_Result);

   procedure Put_Bytes (Device : Device_T; Data : Storage_Array);

end Devices.Console;
