-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Ada.Unchecked_Conversion;

with Function_Results; use Function_Results;
with MMIO;             use MMIO;

-------------------------------------------------------------------------------
--  Contains functionality for interfacing with the serial hardware.
-------------------------------------------------------------------------------

package Devices.UART is
   pragma Preelaborate;

   ----------------------------------------------------------------------------
   --  The maximum supported baud rate.
   ----------------------------------------------------------------------------
   MAXIMUM_BAUD_RATE : constant := 115200;

   ----------------------------------------------------------------------------
   --  UART Baud Rate
   ----------------------------------------------------------------------------
   subtype Baud_Rate is Natural range 50 .. MAXIMUM_BAUD_RATE;

   ----------------------------------------------------------------------------
   --  Serial Interrupt Type
   --  Defines the types of interrupts the serial port can generate.
   ----------------------------------------------------------------------------
   type UART_Interrupt_Type is
     (Modem_Line_Status, Rx_Data_Available, Rx_Line_Status, Tx_Empty);

   ----------------------------------------------------------------------------
   --  Initialises a particular serial port.
   --  This procedure will configure the baud rate, word length, parity
   --  and stop bits for a particular serial port.
   ----------------------------------------------------------------------------
   procedure Initialise
     (Device : in out Device_T;
      Result : out Function_Result;
      Rate   : Baud_Rate := MAXIMUM_BAUD_RATE);

   ----------------------------------------------------------------------------
   --  Enables or disables the generation of interrupts of a particular type.
   ----------------------------------------------------------------------------
   procedure Set_Interrupt_Generation
     (Device         : Device_T;
      Interrupt_Type : UART_Interrupt_Type;
      Status         : Boolean);

   procedure Acknowledge_Interrupt
     (Device : in out Device_T; Result : out Function_Result);

   ----------------------------------------------------------------------------
   --  Prints a string to the specified device.
   ----------------------------------------------------------------------------
   procedure Put_String (Device : Device_T; Data : String);

   ----------------------------------------------------------------------------
   --  Prints a wide string to the specified device.
   ----------------------------------------------------------------------------
   procedure Put_String_Wide (Device : Device_T; Data : Wide_String);

   ----------------------------------------------------------------------------
   --  Prints a character to a UART port.
   ----------------------------------------------------------------------------
   procedure Put_Char (Device : Device_T; Data : Character);

   type Byte_Array_T is array (Natural range <>) of Unsigned_8;

   procedure Put_Bytes (Device : Device_T; Data : Byte_Array_T);

   procedure Put_Byte (Device : Device_T; Data : Unsigned_8);
private
   --  Register offsets.
   UART_Reg_Rx_Buffer_Tx_Holding         : constant := 0;
   UART_Reg_Interrupt_Enable             : constant := 1;
   UART_Reg_Interrupt_Ident_FIFO_Control : constant := 2;
   UART_Reg_Line_Control                 : constant := 3;
   UART_Reg_Modem_Control                : constant := 4;
   UART_Reg_Line_Status                  : constant := 5;
   UART_Reg_Modem_Status                 : constant := 6;
   UART_Reg_Scratch                      : constant := 7;

   type IIR_Interrupt_Source is
     (UART_IRQ_Modem_Status,
      UART_IRQ_Tx_Holding_Empty,
      UART_IRQ_Rx_Data_Available,
      UART_IRQ_Rx_Line_Status,
      UART_IRQ_Char_Timeout)
   with Size => 3;
   for IIR_Interrupt_Source use
     (UART_IRQ_Modem_Status      => 0,
      UART_IRQ_Tx_Holding_Empty  => 1,
      UART_IRQ_Rx_Data_Available => 2,
      UART_IRQ_Rx_Line_Status    => 3,
      UART_IRQ_Char_Timeout      => 6);

   type Interrupt_Ident_Register is record
      No_Interrupt_Pending : Boolean;
      Interrupt_Source     : IIR_Interrupt_Source;
   end record
   with Size => 8, Convention => C, Volatile;
   for Interrupt_Ident_Register use
     record
       No_Interrupt_Pending at 0 range 0 .. 0;
       Interrupt_Source     at 0 range 1 .. 3;
     end record;

   function To_Interrupt_Ident_Register is new
     Ada.Unchecked_Conversion
       (Source => Unsigned_8,
        Target => Interrupt_Ident_Register);

   ----------------------------------------------------------------------------
   --  Port Interrupt status/enable register type.
   --  This type can be used for getting/setting the interrupt generation
   --  status of a particular interrupt type.
   --  For more information refer to page 17 of the PC16550D datasheet.
   ----------------------------------------------------------------------------
   type Port_Interrupt_Status is record
      ERBFI : Boolean;
      ETBEI : Boolean;
      ELSI  : Boolean;
      EDSSI : Boolean;
   end record
   with Size => 8, Convention => C, Volatile;
   for Port_Interrupt_Status use
     record
       ERBFI at 0 range 0 .. 0;
       ETBEI at 0 range 1 .. 1;
       ELSI  at 0 range 2 .. 2;
       EDSSI at 0 range 3 .. 3;
     end record;

   ----------------------------------------------------------------------------
   --  Unchecked conversion to read a port's interrupt status from an IO port.
   ----------------------------------------------------------------------------
   function Byte_To_Port_Interrupt_Status is new
     Ada.Unchecked_Conversion
       (Source => Unsigned_8,
        Target => Port_Interrupt_Status);

   ----------------------------------------------------------------------------
   --  Unchecked conversion to write a port's interrupt status to an IO port.
   ----------------------------------------------------------------------------
   function Port_Interrupt_Status_To_Byte is new
     Ada.Unchecked_Conversion
       (Source => Port_Interrupt_Status,
        Target => Unsigned_8);

   ----------------------------------------------------------------------------
   --  Tests whether a particular port's transmission buffer is ready to
   --  accept new data.
   --  This is used during the various transmission functions to ensure that
   --  an overflow exception is not generated.
   ----------------------------------------------------------------------------
   function Is_Tx_Empty (Device : Device_T) return Boolean
   is ((Read_Unsigned_8 (Device.Virtual_Address + UART_Reg_Line_Status)
        and 16#20#)
       /= 0)
   with Volatile_Function;

   function Is_Rx_Empty (Device : Device_T) return Boolean
   is ((Read_Unsigned_8 (Device.Virtual_Address + UART_Reg_Line_Status)
        and 16#01#)
       = 0)
   with Volatile_Function;

   ----------------------------------------------------------------------------
   --  Sets the divisor latch state for a particular UART peripheral.
   --  This will set the DLAB state for the selected serial peripheral.
   --  For more information regarding the use of this procedure refer to the
   --  16550 UART documentation.
   ----------------------------------------------------------------------------
   procedure Set_Divisor_Latch_State (Device : Device_T; State : Boolean);

   ----------------------------------------------------------------------------
   --  Sets the baud rate for a particular UART port.
   ----------------------------------------------------------------------------
   procedure Set_Baud_Rate (Device : Device_T; Rate : Baud_Rate);

   function Wait_For_Byte
     (Device : Device_T; Timeout : Positive := 1_000) return Unsigned_8;

   function Read_Byte (Device : Device_T) return Unsigned_8
   is (Read_Unsigned_8 (Device.Virtual_Address));

   function Read_Character (Device : Device_T) return Character
   is (Character'Val (Read_Byte (Device)));

   procedure Read_All_Incoming_Data
     (Device     : in out Device_T;
      Bytes_Read : out Integer;
      Result     : out Function_Result);

end Devices.UART;
