with Devices.UART;
with Hart_State;    use Hart_State;
with Logging;       use Logging;
with Memory.Kernel; use Memory.Kernel;

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
      Allocation_Result : Virtual_Physical_Address_Pair_T;
   begin
      Allocate_Pages (1, Allocation_Result, Result);
      if Is_Error (Result) then
         return;
      end if;

      Device.Line_Buffer_Address := Allocation_Result.Virtual_Address;
      Device.Line_Buffer_Size := 4096;
      Device.Line_Buffer_Offset_Read := 0;
      Device.Line_Buffer_Offset_Write := 0;
      Device.Line_Buffer_Offset_Committed := 0;

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Constraint_Error;
         Result := Constraint_Exception;
   end Allocate_Line_Buffer;

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
   procedure Echo_Byte (Device : Device_T; Byte : Unsigned_8) is
   begin
      if Character'Val (Byte) = ASCII.LF then
         Put_Bytes_Unlocked
           (Device, [Character'Pos (ASCII.CR), Character'Pos (ASCII.LF)]);
      elsif Is_Printable (Byte) then
         Put_Bytes_Unlocked (Device, [1 => Storage_Element (Byte)]);
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
     (Device     : in out Device_T;
      Process    : in out Process_Control_Block_T;
      Buffer     : out Storage_Array;
      Bytes_Read : out Storage_Count;
      Result     : out Function_Result)
   is
      Incoming_Byte : Unsigned_8 := 0;
   begin
      Bytes_Read := 0;

      Read_Line_Buffer : declare
         Line_Buffer : Storage_Array (0 .. Device.Line_Buffer_Size - 1)
         with Import, Alignment => 1, Address => Device.Line_Buffer_Address;
      begin
         --  If there is no unread committed line, read a new line from the
         --  backend device. The line is only made available to the caller
         --  once it has been terminated with a newline or terminated with
         --  Ctrl+D.
         if Device.Line_Buffer_Offset_Read
           = Device.Line_Buffer_Offset_Committed
         then
            Device.Line_Buffer_Offset_Read := 0;
            Device.Line_Buffer_Offset_Write := 0;
            Device.Line_Buffer_Offset_Committed := 0;

            loop
               Release_Spinlock (Device.Spinlock);

               case Device.Console_Backend_Device.all.Device_Class is
                  when Device_Class_Serial =>
                     Devices.UART.Read_Byte
                       (Device.Console_Backend_Device.all,
                        Process,
                        Incoming_Byte,
                        Result);

                  when others              =>
                     Result := Not_Supported;
               end case;

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
                  if Device.Line_Buffer_Offset_Write
                    > Device.Line_Buffer_Offset_Committed
                  then
                     Device.Line_Buffer_Offset_Write :=
                       Device.Line_Buffer_Offset_Write - 1;

                     Put_Bytes_Unlocked
                       (Device,
                        [Character'Pos (ASCII.BS),
                         Character'Pos (' '),
                         Character'Pos (ASCII.BS)]);
                  end if;
               elsif Character'Val (Incoming_Byte) = ASCII.LF
                 or else
                   (Is_Printable (Incoming_Byte)
                    and then
                      Device.Line_Buffer_Offset_Write
                      < Device.Line_Buffer_Size - 1)
               then
                  --  The last byte of the line buffer is reserved for the
                  --  newline, so that a line is always able to be terminated.
                  --  Any bytes received after the buffer is full are
                  --  discarded.

                  Line_Buffer (Device.Line_Buffer_Offset_Write) :=
                    Storage_Element (Incoming_Byte);

                  Device.Line_Buffer_Offset_Write :=
                    Device.Line_Buffer_Offset_Write + 1;

                  Echo_Byte (Device, Incoming_Byte);
               end if;

               exit when Character'Val (Incoming_Byte) = ASCII.LF;
            end loop;

            Device.Line_Buffer_Offset_Committed :=
              Device.Line_Buffer_Offset_Write;
         end if;

         --  Copy as much of the committed line as will fit in the caller's
         --  buffer. Any remainder is returned by subsequent calls.
         Number_Of_Bytes_To_Copy : constant Storage_Count :=
           Storage_Count'Min
             (Device.Line_Buffer_Offset_Committed
              - Device.Line_Buffer_Offset_Read,
              Buffer'Length);

         Buffer (Buffer'First .. Buffer'First + Number_Of_Bytes_To_Copy - 1) :=
           Line_Buffer
             (Device.Line_Buffer_Offset_Read
              .. Device.Line_Buffer_Offset_Read + Number_Of_Bytes_To_Copy - 1);

         Device.Line_Buffer_Offset_Read :=
           Device.Line_Buffer_Offset_Read + Number_Of_Bytes_To_Copy;

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

      Acquire_Spinlock (Device.Spinlock);
      Acquire_Sleeplock (Device.Line_Read_Lock, Device.Spinlock, Process);

      Read_Line_Unlocked (Device, Process, Buffer, Bytes_Read, Result);

      Release_Sleeplock (Device.Line_Read_Lock);
      Release_Spinlock (Device.Spinlock);
   exception
      when Constraint_Error =>
         --  Because a constraint error in this procedure would potentially
         --  deadlock other processes waiting for this device, panic.
         Panic_Constraint_Error;
   end Read_Line;

   procedure Read_Bytes
     (Device     : in out Device_T;
      Process    : in out Process_Control_Block_T;
      Buffer     : out Storage_Array;
      Bytes_Read : out Storage_Count;
      Result     : out Function_Result) is
   begin
      Read_Line (Device, Process, Buffer, Bytes_Read, Result);
   end Read_Bytes;

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
