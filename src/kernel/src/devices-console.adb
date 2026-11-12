with Devices.Attached_System_Devices; use Devices.Attached_System_Devices;
with Devices.UART;
with Logging;                         use Logging;
with Memory.Kernel;                   use Memory.Kernel;

package body Devices.Console is
   function Has_Backend (State : Console_Device_State_T) return Boolean
   is (State.Backend /= null);

   function Is_Device_Initialised
     (State : Console_Device_State_T) return Boolean
   is (State.Line_Buffer_Address /= Null_Address
       and then State.Line_Buffer_Size /= 0);

   function Is_Device_Valid_And_Initialised
     (State : Console_Device_State_T) return Boolean
   is (Has_Backend (State) and then Is_Device_Initialised (State));

   procedure Allocate_Line_Buffer
     (State : in out Console_Device_State_T; Result : out Function_Result)
   is
      Allocation_Result : Virtual_Physical_Address_Pair_T;
   begin
      Allocate_Pages (1, Allocation_Result, Result);
      if Is_Error (Result) then
         return;
      end if;

      State.Line_Buffer_Address := Allocation_Result.Virtual_Address;
      State.Line_Buffer_Size := 4096;
      State.Line_Buffer_Offset_Read := 0;
      State.Line_Buffer_Offset_Write := 0;
      State.Line_Buffer_Offset_Committed := 0;

      Result := Success;
   end Allocate_Line_Buffer;

   procedure Put_Bytes_Unlocked
     (State : Console_Device_State_T; Data : Storage_Array) is
   begin
      if not Has_Backend (State) then
         return;
      end if;

      Backend : Device_T renames State.Backend.all;

      case Backend.Driver_Type is
         when Driver_Type_UART =>
            Devices.UART.Put_Bytes (Backend.Common, Backend.UART, Data);

         when others           =>
            null;
      end case;
   exception
      when Constraint_Error =>
         Log_Constraint_Error;
   end Put_Bytes_Unlocked;

   function Is_Printable (Byte : Unsigned_8) return Boolean
   is (Byte in 16#20# .. 16#7E#);

   ----------------------------------------------------------------------------
   --  Implementation Notes:
   --   - Must be called with the device spinlock held.
   --     This ensures that the echoed output doesn't land in another process'
   --     console output.
   --   - Control characters aren't echoed, since a serial terminal would act
   --     on them instead of displaying them.
   ----------------------------------------------------------------------------
   procedure Echo_Byte (State : Console_Device_State_T; Byte : Unsigned_8) is
   begin
      if Character'Val (Byte) = ASCII.LF then
         Put_Bytes_Unlocked
           (State, [Character'Pos (ASCII.CR), Character'Pos (ASCII.LF)]);
      elsif Is_Printable (Byte) then
         Put_Bytes_Unlocked (State, [1 => Storage_Element (Byte)]);
      end if;
   end Echo_Byte;

   function Is_Backspace (Byte : Unsigned_8) return Boolean
   is (Byte in Character'Pos (ASCII.BS) | Character'Pos (ASCII.DEL));

   ----------------------------------------------------------------------------
   --  Implementation Notes:
   --   - Must be called with the device spinlock and the line read sleeplock
   --     held. The spinlock is released while waiting for input.
   ----------------------------------------------------------------------------
   procedure Read_Line_Unlocked
     (Device     : in out Device_Common_State_T;
      State      : in out Console_Device_State_T;
      Process    : in out Process_Control_Block_T;
      Buffer     : out Storage_Array;
      Bytes_Read : out Storage_Count;
      Result     : out Function_Result)
   is
      Incoming_Byte : Unsigned_8 := 0;
   begin
      Bytes_Read := 0;

      Read_Line_Buffer : declare
         Line_Buffer : Storage_Array (0 .. State.Line_Buffer_Size - 1)
         with Import, Alignment => 1, Address => State.Line_Buffer_Address;
      begin
         --  If there is no unread committed line, read a new line from the
         --  backend device. The line is only made available to the caller
         --  once it has been terminated with a newline or terminated with
         --  Ctrl+D.
         if State.Line_Buffer_Offset_Read = State.Line_Buffer_Offset_Committed
         then
            State.Line_Buffer_Offset_Read := 0;
            State.Line_Buffer_Offset_Write := 0;
            State.Line_Buffer_Offset_Committed := 0;

            loop
               Release_Spinlock (Device.Spinlock);

               Read_Backend_Byte : declare
                  Backend : Device_T renames State.Backend.all;
               begin
                  case Backend.Driver_Type is
                     when Driver_Type_UART =>
                        Devices.UART.Read_Byte
                          (Backend.Common,
                           Backend.UART,
                           Process,
                           Incoming_Byte,
                           Result);

                     when others           =>
                        Result := Not_Supported;
                  end case;
               end Read_Backend_Byte;

               Acquire_Spinlock (Device.Spinlock);

               if Is_Error (Result) then
                  return;
               end if;

               --  Terminals send a carriage return for the Enter key.
               --  Convert that to a line feed character.
               if Character'Val (Incoming_Byte) = ASCII.CR then
                  Incoming_Byte := Character'Pos (ASCII.LF);
               end if;

               --  Ctrl+D ends the read without being stored. On an empty line
               --  this returns zero bytes, which signals end-of-file.
               exit when Incoming_Byte = Character'Pos (ASCII.EOT);

               if Is_Backspace (Incoming_Byte) then
                  if State.Line_Buffer_Offset_Write
                    > State.Line_Buffer_Offset_Committed
                  then
                     State.Line_Buffer_Offset_Write :=
                       State.Line_Buffer_Offset_Write - 1;

                     Put_Bytes_Unlocked
                       (State,
                        [Character'Pos (ASCII.BS),
                         Character'Pos (' '),
                         Character'Pos (ASCII.BS)]);
                  end if;
               elsif Character'Val (Incoming_Byte) = ASCII.LF
                 or else
                   (Is_Printable (Incoming_Byte)
                    and then
                      State.Line_Buffer_Offset_Write
                      < State.Line_Buffer_Size - 1)
               then
                  --  The last byte of the line buffer is reserved for the
                  --  newline, so that a line is always able to be terminated.
                  --  Any bytes received after the buffer is full are
                  --  discarded.

                  Line_Buffer (State.Line_Buffer_Offset_Write) :=
                    Storage_Element (Incoming_Byte);

                  State.Line_Buffer_Offset_Write :=
                    State.Line_Buffer_Offset_Write + 1;

                  Echo_Byte (State, Incoming_Byte);
               end if;

               exit when Character'Val (Incoming_Byte) = ASCII.LF;
            end loop;

            State.Line_Buffer_Offset_Committed :=
              State.Line_Buffer_Offset_Write;
         end if;

         --  Copy as much of the committed line as will fit in the caller's
         --  buffer. Any remainder is returned by subsequent calls.
         Number_Of_Bytes_To_Copy : constant Storage_Count :=
           Storage_Count'Min
             (State.Line_Buffer_Offset_Committed
              - State.Line_Buffer_Offset_Read,
              Buffer'Length);

         Buffer (Buffer'First .. Buffer'First + Number_Of_Bytes_To_Copy - 1) :=
           Line_Buffer
             (State.Line_Buffer_Offset_Read
              .. State.Line_Buffer_Offset_Read + Number_Of_Bytes_To_Copy - 1);

         State.Line_Buffer_Offset_Read :=
           State.Line_Buffer_Offset_Read + Number_Of_Bytes_To_Copy;

         Bytes_Read := Number_Of_Bytes_To_Copy;
      end Read_Line_Buffer;

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Constraint_Error;
         Bytes_Read := 0;
         Result := Constraint_Exception;
   end Read_Line_Unlocked;

   procedure Read_Line
     (Device     : in out Device_Common_State_T;
      State      : in out Console_Device_State_T;
      Process    : in out Process_Control_Block_T;
      Buffer     : out Storage_Array;
      Bytes_Read : out Storage_Count;
      Result     : out Function_Result) is
   begin
      if not Is_Device_Valid_And_Initialised (State) then
         Result := Not_Initialised;
         Bytes_Read := 0;
         return;
      end if;

      Acquire_Spinlock (Device.Spinlock);
      Acquire_Sleeplock (State.Line_Read_Lock, Device.Spinlock, Process);

      Read_Line_Unlocked (Device, State, Process, Buffer, Bytes_Read, Result);

      Release_Sleeplock (State.Line_Read_Lock);
      Release_Spinlock (Device.Spinlock);
   end Read_Line;

   procedure Read_Bytes
     (Device     : in out Device_Common_State_T;
      State      : in out Console_Device_State_T;
      Process    : in out Process_Control_Block_T;
      Buffer     : out Storage_Array;
      Bytes_Read : out Storage_Count;
      Result     : out Function_Result) is
   begin
      Read_Line (Device, State, Process, Buffer, Bytes_Read, Result);
   end Read_Bytes;

   procedure Put_Bytes
     (Device : in out Device_Common_State_T;
      State  : Console_Device_State_T;
      Data   : Storage_Array) is
   begin
      Acquire_Spinlock (Device.Spinlock);
      Put_Bytes_Unlocked (State, Data);
      Release_Spinlock (Device.Spinlock);
   end Put_Bytes;

   procedure Initialise
     (State : in out Console_Device_State_T; Result : out Function_Result) is
   begin
      if not Has_Backend (State) then
         Result := Invalid_Argument;
         return;
      end if;

      Allocate_Line_Buffer (State, Result);
      if Is_Error (Result) then
         return;
      end if;
   end Initialise;
end Devices.Console;
