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
     (Device_Address : Address; Rate : Baud_Rate := MAXIMUM_BAUD_RATE);

   ----------------------------------------------------------------------------
   --  Enables or disables the generation of interrupts of a particular type.
   ----------------------------------------------------------------------------
   procedure Set_Interrupt_Generation
     (Device_Address : Address;
      Interrupt_Type : UART_Interrupt_Type;
      Status         : Boolean);

   procedure Acknowledge_Interrupt
     (Device_Address : Address; Result : out Function_Result);

   ----------------------------------------------------------------------------
   --  Prints a string to the specified device.
   ----------------------------------------------------------------------------
   procedure Put_String (Device_Address : Address; Data : String);

   ----------------------------------------------------------------------------
   --  Prints a wide string to the specified device.
   ----------------------------------------------------------------------------
   procedure Put_String_Wide (Device_Address : Address; Data : Wide_String);

   ----------------------------------------------------------------------------
   --  Prints a character to a UART port.
   ----------------------------------------------------------------------------
   procedure Put_Char (Device_Address : Address; Data : Character);
private
   --  Register offsets.
   Rx_Buffer_Tx_Holding         : constant := 0;
   Interrupt_Enable             : constant := 1;
   Interrupt_Ident_FIFO_Control : constant := 2;
   Line_Control                 : constant := 3;
   Modem_Control                : constant := 4;
   Line_Status                  : constant := 5;
   Modem_Status                 : constant := 6;
   Scratch                      : constant := 7;

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
   function Is_Tx_Empty (Device_Address : Address) return Boolean
   is ((Read_Unsigned_8 (Device_Address + Line_Status) and 16#20#) /= 0)
   with Volatile_Function;

   function Is_Rx_Empty (Device_Address : Address) return Boolean
   is ((Read_Unsigned_8 (Device_Address + Line_Status) and 16#01#) = 0)
   with Volatile_Function;

   ----------------------------------------------------------------------------
   --  Sets the divisor latch state for a particular UART peripheral.
   --  This will set the DLAB state for the selected serial peripheral.
   --  For more information regarding the use of this procedure refer to the
   --  16550 UART documentation.
   ----------------------------------------------------------------------------
   procedure Set_Divisor_Latch_State
     (Device_Address : Address; State : Boolean);

   ----------------------------------------------------------------------------
   --  Sets the baud rate for a particular UART port.
   ----------------------------------------------------------------------------
   procedure Set_Baud_Rate (Device_Address : Address; Rate : Baud_Rate);

   function Read_Character (Device_Address : Address) return Character;

end Devices.UART;
