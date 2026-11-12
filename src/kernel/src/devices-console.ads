limited with Devices.Attached_System_Devices;

with Function_Results; use Function_Results;
with Locks.Sleeplocks; use Locks.Sleeplocks;
with Processes;        use Processes;

package Devices.Console
  with Preelaborate
is
   type Console_Device_State_T is record
      Line_Buffer_Address          : Virtual_Address_T := Null_Address;
      Line_Buffer_Size             : Storage_Count := 0;
      Line_Buffer_Offset_Read      : Storage_Offset := 0;
      Line_Buffer_Offset_Write     : Storage_Offset := 0;
      Line_Buffer_Offset_Committed : Storage_Offset := 0;

      --  This lock is used to control access to the line read operation.
      --  Because the line reading procedure releases the spinlock while
      --  it's waiting for input, the spinlock alone isn't enough to
      --  ensure that another process doesn't 'steal' incoming bytes from
      --  another process that's waiting to receive input.
      --  Once one process is waiting to receive a line, another
      --  process must sleep on this lock until the current read call
      --  returns.
      Line_Read_Lock : Sleeplock_T;

      --  The backend device responsible for handling the actual byte
      --  input and output for the console.
      Backend : access Devices.Attached_System_Devices.Device_T := null;
   end record;

   procedure Initialise
     (State : in out Console_Device_State_T; Result : out Function_Result);

   procedure Read_Bytes
     (Device     : in out Device_Common_State_T;
      State      : in out Console_Device_State_T;
      Process    : in out Process_Control_Block_T;
      Buffer     : out Storage_Array;
      Bytes_Read : out Storage_Count;
      Result     : out Function_Result);

   procedure Put_Bytes
     (Device : in out Device_Common_State_T;
      State  : Console_Device_State_T;
      Data   : Storage_Array);

end Devices.Console;
