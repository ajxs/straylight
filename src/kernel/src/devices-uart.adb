-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Memory.Allocators; use Memory.Allocators;
with Memory.Kernel;     use Memory.Kernel;
with Utilities;         use Utilities;

package body Devices.UART is
   procedure Handle_Rx_Data_Available_Interrupt
     (Device : in out Device_T; Result : out Function_Result)
   is
      Bytes_Read : Integer := 0;
   begin
      Read_All_Incoming_Data (Device, Bytes_Read, Result);
      if Is_Error (Result) then
         return;
      end if;

      Log_Debug
        ("UART Rx Data Available Interrupt: "
         & Integer'Image (Bytes_Read)
         & " bytes read.",
         Logging_Tags);

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Handle_Rx_Data_Available_Interrupt");
         Result := Constraint_Exception;
   end Handle_Rx_Data_Available_Interrupt;

   procedure Acknowledge_Interrupt
     (Device : in out Device_T; Result : out Function_Result) is
   begin
      Interrupt_Status : constant Interrupt_Ident_Register :=
        To_Interrupt_Ident_Register
          (Read_Unsigned_8
             (Device.Virtual_Address + UART_Reg_Interrupt_Ident_FIFO_Control));

      if Interrupt_Status.No_Interrupt_Pending then
         Log_Debug ("Spurious UART Interrupt.", Logging_Tags);
         Result := Success;
         return;
      end if;

      case Interrupt_Status.Interrupt_Source is
         when UART_IRQ_Modem_Status                              =>
            Log_Debug ("UART Modem Status Interrupt.", Logging_Tags);

            --  Clear the modem status interrupt by reading the modem
            --  status register.
            Modem_Status : constant Unsigned_8 :=
              Read_Unsigned_8 (Device.Virtual_Address + UART_Reg_Modem_Status);
            pragma Unreferenced (Modem_Status);

         when UART_IRQ_Tx_Holding_Empty                          =>
            Log_Debug ("UART Tx Holding Empty Interrupt.", Logging_Tags);

         --  Interrupt already cleared by read to the IIR register.

         when UART_IRQ_Rx_Data_Available | UART_IRQ_Char_Timeout =>
            Handle_Rx_Data_Available_Interrupt (Device, Result);
            if Is_Error (Result) then
               return;
            end if;

         when UART_IRQ_Rx_Line_Status                            =>
            Log_Debug ("UART Rx Line Status Interrupt.", Logging_Tags);

            --  Clear the line status interrupt by reading the line
            --  status register.
            Line_Status : constant Unsigned_8 :=
              Read_Unsigned_8 (Device.Virtual_Address + UART_Reg_Line_Status);
            pragma Unreferenced (Line_Status);
      end case;

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Acknowledge_Interrupt");
         Result := Constraint_Exception;
   end Acknowledge_Interrupt;

   procedure Allocate_Ring_Buffer
     (Device : in out Device_T; Result : out Function_Result)
   is
      Allocation_Result : Memory_Allocation_Result;
   begin
      Allocate_Pages (1, Allocation_Result, Result);
      if Is_Error (Result) then
         return;
      end if;

      Device.Ring_Buffer_Address := Allocation_Result.Virtual_Address;
      Device.Ring_Buffer_Size := 4096;
      Device.Ring_Buffer_Offset_Read := 0;
      Device.Ring_Buffer_Offset_Write := 0;

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Allocate_Ring_Buffer");
         Result := Constraint_Exception;
   end Allocate_Ring_Buffer;

   ----------------------------------------------------------------------------
   --  Implementation Notes:
   --   - All interrupts are disabled during initialisation.
   ----------------------------------------------------------------------------
   procedure Initialise
     (Device : in out Device_T;
      Result : out Function_Result;
      Rate   : Baud_Rate := MAXIMUM_BAUD_RATE) is
   begin
      --  Disable all interrupts.
      Write_Unsigned_8 (Device.Virtual_Address + UART_Reg_Interrupt_Enable, 0);

      --  Set the baud rate.
      Set_Baud_Rate (Device, Rate);

      --  Configure the port with 8 bit word length.
      --  No parity bit, one stop bit.
      Write_Unsigned_8
        (Device.Virtual_Address + UART_Reg_Line_Control, 16#03#);

      --  Enable FIFO.
      Write_Unsigned_8
        (Device.Virtual_Address + UART_Reg_Interrupt_Ident_FIFO_Control,
         16#C7#);

      --  Result set by this call.
      Allocate_Ring_Buffer (Device, Result);

   end Initialise;

   ----------------------------------------------------------------------------
   --  Implementation Notes:
   --   - Does not determine whether the port has been initialised.
   ----------------------------------------------------------------------------
   procedure Put_Char (Device : Device_T; Data : Character) is
   begin
      Put_Byte (Device, Character'Pos (Data));
   end Put_Char;

   ----------------------------------------------------------------------------
   --  Implementation Notes:
   --   - Does not determine whether the port has been initialised.
   ----------------------------------------------------------------------------
   procedure Put_String (Device : Device_T; Data : String) is
   begin
      Print_Loop : for C of Data loop
         Put_Char (Device, C);
      end loop Print_Loop;
   end Put_String;

   procedure Put_String_Wide (Device : Device_T; Data : Wide_String) is
   begin
      Print_Loop : for C of Data loop
         Put_Char (Device, Convert_Wide_Char_To_ASCII (C));
      end loop Print_Loop;
   end Put_String_Wide;

   function Wait_For_Byte
     (Device : Device_T; Timeout : Positive := 1_000) return Unsigned_8 is
   begin
      for I in 1 .. Timeout loop
         if not Is_Rx_Empty (Device) then
            return Read_Byte (Device);
         end if;
      end loop;

      return 0;
   end Wait_For_Byte;

   ----------------------------------------------------------------------------
   --  Implementation Notes:
   --   - Does not determine whether the port has been initialised.
   ----------------------------------------------------------------------------
   procedure Set_Baud_Rate (Device : Device_T; Rate : Baud_Rate) is
      --  The baud rate divisor for this baud rate.
      Divisor           : Unsigned_16;
      --  The value to write into the divisor low register.
      Divisor_Low_Byte  : Unsigned_8;
      --  The value to write into the divisor high register.
      Divisor_High_Byte : Unsigned_8;
   begin
      Get_Divisor : begin
         Divisor := Unsigned_16 (MAXIMUM_BAUD_RATE / Rate);
      exception
         --  If an invalid value is generated, set the divisor to 1.
         when Constraint_Error =>
            Divisor := 1;
      end Get_Divisor;

      Get_Divisor_Value : begin
         Divisor_Low_Byte := Unsigned_8 (Divisor and 16#FF#);
         Divisor_High_Byte := Unsigned_8 (Shift_Right (Divisor, 8) and 16#FF#);
      exception
         --  In the case of any errors here, default to the
         --  highest baud rate setting.
         when Constraint_Error =>
            Divisor_Low_Byte := 16#01#;
            Divisor_High_Byte := 16#0#;
      end Get_Divisor_Value;

      --  Enable DLAB.
      Set_Divisor_Latch_State (Device, True);
      --  Set baud rate divisor low byte.
      Write_Unsigned_8 (Device.Virtual_Address, Divisor_Low_Byte);
      --  Set baud rate divisor high byte.
      Write_Unsigned_8 (Device.Virtual_Address + 1, Divisor_High_Byte);
      --  Disable DLAB.
      Set_Divisor_Latch_State (Device, False);
   end Set_Baud_Rate;

   ----------------------------------------------------------------------------
   --  Implementation Notes:
   --   - Does not determine whether the port has been initialised.
   ----------------------------------------------------------------------------
   procedure Set_Divisor_Latch_State (Device : Device_T; State : Boolean) is
      --  The existing line map status value.
      Line_Control_Status : Unsigned_8;
   begin
      --  Get the existing line control status, and modify accordingly
      --  to set the divisor latch state.
      Line_Control_Status :=
        Read_Unsigned_8 (Device.Virtual_Address + UART_Reg_Line_Control);

      case State is
         when True  =>
            Line_Control_Status := Line_Control_Status or 16#80#;

         when False =>
            Line_Control_Status := Line_Control_Status and (not 16#80#);
      end case;

      --  Write the DLAB state.
      Write_Unsigned_8
        (Device.Virtual_Address + UART_Reg_Line_Control, Line_Control_Status);
   exception
      when Constraint_Error =>
         null;
   end Set_Divisor_Latch_State;

   ----------------------------------------------------------------------------
   --  Implementation Notes:
   --   - Does not determine whether the port has been initialised.
   ----------------------------------------------------------------------------
   procedure Set_Interrupt_Generation
     (Device         : Device_T;
      Interrupt_Type : UART_Interrupt_Type;
      Status         : Boolean)
   is
      Interrupt_Status : Port_Interrupt_Status;
   begin
      --  Get the current status of this device's interrupts to
      --  preserve the current interrupt status.
      Interrupt_Status :=
        Byte_To_Port_Interrupt_Status
          (Read_Unsigned_8
             (Device.Virtual_Address + UART_Reg_Interrupt_Enable));

      Set_Interrupt_Status : begin
         --  Set the interrupt status var to the desired value.
         case Interrupt_Type is
            when Modem_Line_Status =>
               Interrupt_Status.EDSSI := Status;

            when Rx_Data_Available =>
               Interrupt_Status.ERBFI := Status;

            when Rx_Line_Status    =>
               Interrupt_Status.ELSI := Status;

            when Tx_Empty          =>
               Interrupt_Status.ETBEI := Status;
         end case;
      exception
         when Constraint_Error =>
            return;
      end Set_Interrupt_Status;

      --  Write to the Interrupt enable register.
      Write_Unsigned_8
        (Device.Virtual_Address + UART_Reg_Interrupt_Enable,
         Port_Interrupt_Status_To_Byte (Interrupt_Status));
   end Set_Interrupt_Generation;

   procedure Put_Bytes (Device : Device_T; Data : Byte_Array_T) is
   begin
      Print_Loop : for C of Data loop
         Put_Byte (Device, C);
      end loop Print_Loop;
   end Put_Bytes;

   ----------------------------------------------------------------------------
   --  Implementation Notes:
   --   - Does not determine whether the port has been initialised.
   ----------------------------------------------------------------------------
   procedure Put_Byte (Device : Device_T; Data : Unsigned_8) is
      Put_Byte_Timeout : constant := 1_000_000;
   begin
      for I in 1 .. Put_Byte_Timeout loop
         if Is_Tx_Empty (Device) then
            Write_Unsigned_8 (Device.Virtual_Address, Data);
            return;
         end if;
      end loop;

      Log_Error ("Timeout while waiting to put byte to UART.", Logging_Tags);
   end Put_Byte;

   procedure Read_Into_Ring_Buffer
     (Device      : in out Device_T;
      Ring_Buffer : in out Byte_Array_T;
      Bytes_Read  : out Integer;
      Result      : out Function_Result)
   is
      Buffer_Is_Full : Boolean := False;

      Incoming_Byte : Unsigned_8 := 0;
   begin
      Bytes_Read := 0;
      Result := Unset;

      --  Check whether the buffer is already full by checking whether
      --  advancing the write offset would cause it to equal the read offset.
      --  This sacrifices one byte of buffer space in the case that the buffer
      --  is full, but allows distinguishing between full and empty states.
      Buffer_Is_Full :=
        (Device.Ring_Buffer_Offset_Write + 1) mod Device.Ring_Buffer_Size
        = Device.Ring_Buffer_Offset_Read;

      while not Is_Rx_Empty (Device) loop
         --  Read incoming data even if the buffer is full to flush the FIFO.
         Incoming_Byte := Read_Byte (Device);

         --  If the driver buffer is full, incoming bytes will be dropped
         --  until space is available.
         if not Buffer_Is_Full then
            --  Store the incoming byte in the ring buffer.
            Ring_Buffer (Device.Ring_Buffer_Offset_Write) := Incoming_Byte;

            Device.Ring_Buffer_Offset_Write :=
              (Device.Ring_Buffer_Offset_Write + 1)
              mod Device.Ring_Buffer_Size;

            Bytes_Read := Bytes_Read + 1;

            Buffer_Is_Full :=
              (Device.Ring_Buffer_Offset_Write + 1) mod Device.Ring_Buffer_Size
              = Device.Ring_Buffer_Offset_Read;
         end if;
      end loop;

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Read_Into_Ring_Buffer");
         Result := Constraint_Exception;
   end Read_Into_Ring_Buffer;

   procedure Read_All_Incoming_Data
     (Device     : in out Device_T;
      Bytes_Read : out Integer;
      Result     : out Function_Result) is
   begin
      Bytes_Read := 0;
      Result := Unset;

      if Device.Ring_Buffer_Address = Null_Address then
         Log_Error ("Attempted to read from uninitialised UART ring buffer.");
         Result := Not_Initialised;
         return;
      end if;

      if Device.Ring_Buffer_Size = 0 then
         Log_Error ("Attempted to read from UART ring buffer with size 0.");
         Result := Not_Initialised;
         return;
      end if;

      declare
         Ring_Buffer : Byte_Array_T (0 .. Device.Ring_Buffer_Size - 1)
         with Import, Alignment => 1, Address => Device.Ring_Buffer_Address;
      begin
         Read_Into_Ring_Buffer (Device, Ring_Buffer, Bytes_Read, Result);
         if Is_Error (Result) then
            return;
         end if;
      end;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Read_All_Incoming_Data");
         Result := Constraint_Exception;
   end Read_All_Incoming_Data;
end Devices.UART;
