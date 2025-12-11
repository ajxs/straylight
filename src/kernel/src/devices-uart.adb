-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Utilities; use Utilities;

package body Devices.UART is
   procedure Acknowledge_Interrupt
     (Device_Address : Address; Result : out Function_Result) is
   begin
      Highest_Status_Interrupt : constant Unsigned_8 :=
        Read_Unsigned_8 (Device_Address + Interrupt_Ident_FIFO_Control)
        and 2#1111#;

      if Highest_Status_Interrupt = 0 then
         Log_Debug ("Spurious UART Interrupt.", Logging_Tags);
      elsif Highest_Status_Interrupt = 4 or else Highest_Status_Interrupt = 12
      then
         --  Handle 'Received Data Available' and
         --  'Character Timeout Indication' interrupts together.
         Read_Incoming_Data :
         declare
            Incoming_Character : Character := ASCII.NUL;
            Incoming_Message   : String (1 .. 128) := [others => ASCII.NUL];
            Incoming_Index     : Natural := 1;
         begin
            --  Received Data Available.
            while not Is_Rx_Empty (Device_Address) loop
               Incoming_Character := Read_Character (Device_Address);

               if Incoming_Index <= Incoming_Message'Length then
                  Incoming_Message (Incoming_Index) := Incoming_Character;
                  Incoming_Index := Incoming_Index + 1;
               end if;
            end loop;

            Log_Debug
              ("Received Data: '" & Incoming_Message & "'", Logging_Tags);
         end Read_Incoming_Data;
      else
         Log_Debug
           ("Unhandled UART Interrupt: " & Highest_Status_Interrupt'Image,
            Logging_Tags);
      end if;

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint error in Acknowledge_Interrupt");
         Result := Constraint_Exception;
   end Acknowledge_Interrupt;

   ----------------------------------------------------------------------------
   --  Implementation Notes:
   --   - All interrupts are disabled during initialisation.
   ----------------------------------------------------------------------------
   procedure Initialise
     (Device_Address : Address; Rate : Baud_Rate := MAXIMUM_BAUD_RATE) is
   begin
      --  Disable all interrupts.
      Write_Unsigned_8 (Device_Address + Interrupt_Enable, 0);

      --  Set the baud rate.
      Set_Baud_Rate (Device_Address, Rate);

      --  Configure the port with 8 bit word length.
      --  No parity bit, one stop bit.
      Write_Unsigned_8 (Device_Address + Line_Control, 16#03#);

      --  Enable FIFO.
      Write_Unsigned_8 (Device_Address + Interrupt_Ident_FIFO_Control, 16#C7#);
   end Initialise;

   ----------------------------------------------------------------------------
   --  Implementation Notes:
   --   - Does not determine whether the port has been initialised.
   ----------------------------------------------------------------------------
   procedure Put_Char (Device_Address : Address; Data : Character) is
   begin
      while Is_Tx_Empty (Device_Address) = False loop
         null;
      end loop;

      Write_Unsigned_8 (Device_Address, Character'Pos (Data));
   end Put_Char;

   ----------------------------------------------------------------------------
   --  Implementation Notes:
   --   - Does not determine whether the port has been initialised.
   ----------------------------------------------------------------------------
   procedure Put_String (Device_Address : Address; Data : String) is
   begin
      Print_Loop :
      for C of Data loop
         Put_Char (Device_Address, C);
      end loop Print_Loop;
   end Put_String;

   procedure Put_String_Wide (Device_Address : Address; Data : Wide_String) is
   begin
      Print_Loop :
      for C of Data loop
         Put_Char (Device_Address, Convert_Wide_Char_To_ASCII (C));
      end loop Print_Loop;
   end Put_String_Wide;

   function Read_Character (Device_Address : Address) return Character is
      Timeout_Counter : Natural := 0;
   begin
      while Is_Rx_Empty (Device_Address) loop
         Timeout_Counter := Timeout_Counter + 1;
         if Timeout_Counter > 1_000 then
            return Character'Val (0);
         end if;
      end loop;

      return Character'Val (Read_Unsigned_8 (Device_Address));
   exception
      when Constraint_Error =>
         return Character'Val (0);
   end Read_Character;

   ----------------------------------------------------------------------------
   --  Implementation Notes:
   --   - Does not determine whether the port has been initialised.
   ----------------------------------------------------------------------------
   procedure Set_Baud_Rate (Device_Address : Address; Rate : Baud_Rate) is
      --  The baud rate divisor for this baud rate.
      Divisor           : Unsigned_16;
      --  The value to write into the divisor low register.
      Divisor_Low_Byte  : Unsigned_8;
      --  The value to write into the divisor high register.
      Divisor_High_Byte : Unsigned_8;
   begin
      Get_Divisor :
      begin
         Divisor := Unsigned_16 (MAXIMUM_BAUD_RATE / Rate);
      exception
         --  If an invalid value is generated, set the divisor to 1.
         when Constraint_Error =>
            Divisor := 1;
      end Get_Divisor;

      Get_Divisor_Value :
      begin
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
      Set_Divisor_Latch_State (Device_Address, True);
      --  Set baud rate divisor low byte to 3 38400 baud.
      Write_Unsigned_8 (Device_Address, Divisor_Low_Byte);
      --  Set baud rate divisor high byte.
      Write_Unsigned_8 (Device_Address + 1, Divisor_High_Byte);
      --  Disable DLAB.
      Set_Divisor_Latch_State (Device_Address, False);
   end Set_Baud_Rate;

   ----------------------------------------------------------------------------
   --  Implementation Notes:
   --   - Does not determine whether the port has been initialised.
   ----------------------------------------------------------------------------
   procedure Set_Divisor_Latch_State
     (Device_Address : Address; State : Boolean)
   is
      --  The existing line map status value.
      Line_Control_Status : Unsigned_8;
   begin
      --  Get the existing line control status, and modify accordingly
      --  to set the divisor latch state.
      Line_Control_Status := Read_Unsigned_8 (Device_Address + Line_Control);

      case State is
         when True  =>
            Line_Control_Status := Line_Control_Status or 16#80#;

         when False =>
            Line_Control_Status := Line_Control_Status and (not 16#80#);
      end case;

      --  Write the DLAB state.
      Write_Unsigned_8 (Device_Address + Line_Control, Line_Control_Status);
   exception
      when Constraint_Error =>
         null;
   end Set_Divisor_Latch_State;

   ----------------------------------------------------------------------------
   --  Implementation Notes:
   --   - Does not determine whether the port has been initialised.
   ----------------------------------------------------------------------------
   procedure Set_Interrupt_Generation
     (Device_Address : Address;
      Interrupt_Type : UART_Interrupt_Type;
      Status         : Boolean)
   is
      Interrupt_Status : Port_Interrupt_Status;
   begin
      --  Get the current status of this device's interrupts to
      --  preserve the current interrupt status.
      Get_Interrupt_Status :
      begin
         Interrupt_Status :=
           Byte_To_Port_Interrupt_Status
             (Read_Unsigned_8 (Device_Address + Interrupt_Enable));
      end Get_Interrupt_Status;

      Set_Interrupt_Status :
      begin
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
        (Device_Address + Interrupt_Enable,
         Port_Interrupt_Status_To_Byte (Interrupt_Status));
   end Set_Interrupt_Generation;
end Devices.UART;
