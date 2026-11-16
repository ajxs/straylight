with GNAT.Source_Info;

with Logging;   use Logging;
with Utilities; use Utilities;

package body Devicetree is
   FDT_MAGIC_NUMBER : constant Unsigned_32 := 16#D00D_FEED#;

   Maximum_FDT_String_Length : constant := 256;

   subtype Devicetree_String_T is
     Fixed_Length_String_T (Maximum_FDT_String_Length);

   --  Forward declaration for this private function to avoid moving the
   --  function body.
   function Compare_Node_Name
     (Node_Name : Devicetree_String_T; Target_Name : String) return Boolean;

   Maximum_Memory_Regions : constant := 16;

   subtype Memory_Region_List_T is
     Physical_Memory_Region_Array_T (1 .. Maximum_Memory_Regions);

   Maximum_Devices : constant := 16;

   subtype FDT_Token_T is Unsigned_32;

   type FDT_Property_T is record
      Length      : Unsigned_32;
      Name_Offset : Unsigned_32;
   end record
   with Convention => C;

   type Reserved_Memory_Block_T is record
      Addr : Unsigned_64;
      Size : Unsigned_64;
   end record
   with Convention => C;

   --  The current address and size cells context is captured in a stack, so
   --  that we can properly handle nested nodes that override the
   --  #address-cells and #size-cells properties.
   type FDT_Cells_Context_T is record
      Address_Cells : Unsigned_32;
      Size_Cells    : Unsigned_32;
   end record;

   Maximum_Cells_Context_Depth : constant := 16;

   type FDT_Cells_Context_Array_T is
     array (1 .. Maximum_Cells_Context_Depth) of FDT_Cells_Context_T;

   type FDT_Cells_Context_Stack_T is record
      Contexts : FDT_Cells_Context_Array_T;
      Count    : Natural := 0;
   end record;

   type Devicetree_Property_T is record
      Name          : Devicetree_String_T;
      Length        : Unsigned_32 := 0;
      Value_Address : Address := Null_Address;
   end record;

   --  The state of the structure block parser, shared between the
   --  procedures that handle each token type.
   --  Some properties of the current node can only be acted on once all
   --  of its properties have been read, since the standard doesn't dictate
   --  their order. These properties are stored in the parser state until the
   --  node is fully parsed.
   type FDT_Parser_State_T is record
      Structure_Block_Address : Address;
      Structure_Block_Size    : Storage_Count;
      String_Table_Address    : Address;
      String_Table_Size       : Storage_Count;
      Curr_Offset             : Storage_Offset := 0;
      Cells_Context_Stack     : FDT_Cells_Context_Stack_T;
      --  This is the context the current node declares for its children.
      --  The node's own 'reg' uses its parent's context, which is on top
      --  of Cells_Context_Stack.
      --  Devicetree specification v0.4 section 2.3.5 states:
      --  'If missing, a client program should assume a default value of 2
      --  for #address-cells, and a value of 1 for #size-cells.'
      Current_Cells_Context   : FDT_Cells_Context_T :=
        (Address_Cells => 2, Size_Cells => 1);

      --  The name of the node currently being parsed.
      Current_Node_Name : Devicetree_String_T;

      --  Current_Node_Reg_Property's Value_Address is null if the node has
      --  no 'reg'.
      Current_Node_Reg_Property : Devicetree_Property_T;

      --  In future, this could be replaced by a 'Current_Node_Type' enum.
      Current_Node_Is_Memory : Boolean := False;

      --  The type of device the current node describes, determined by its
      --  'compatible' property.
      Current_Node_Device_Type : Devicetree_Device_Type_T :=
        Devicetree_Device_Type_None;

      --  The first cell of the current node's 'interrupts' property.
      Current_Node_Interrupt_Line : Unsigned_32 := 0;

      --  This variable is used to determine whether the parser is currently
      --  inside a 'reserved-memory' child node.
      --  When entering a reserved memory parent node, this is set to the
      --  current depth of the cells context stack.
      Reserved_Memory_Node_Depth : Natural := 0;

      Memory_Regions      : Memory_Region_List_T;
      Memory_Region_Count : Natural := 0;

      Reserved_Memory_Regions      : Memory_Region_List_T;
      Reserved_Memory_Region_Count : Natural := 0;

      Devices      : Devicetree_Device_Array_T (1 .. Maximum_Devices);
      Device_Count : Natural := 0;

      --  From '/cpus/timebase-frequency', or 0 if it hasn't been found.
      Timebase_Frequency : Unsigned_64 := 0;
   end record;

   procedure Push_Cells_Context
     (Context_Stack : in out FDT_Cells_Context_Stack_T;
      New_Context   : FDT_Cells_Context_T;
      Result        : out Function_Result) is
   begin
      if Context_Stack.Count >= Context_Stack.Contexts'Last then
         Log_Error
           ("Cells context stack overflow: " & Context_Stack.Count'Image);
         Result := Constraint_Exception;
         return;
      end if;

      pragma Debug (Debug_Devicetree, Log_Debug ("Pushing cells context"));

      Context_Stack.Count := Context_Stack.Count + 1;
      Context_Stack.Contexts (Context_Stack.Count) := New_Context;
      Result := Success;
   exception
      when Constraint_Error =>
         Log_Constraint_Error;
         Result := Constraint_Exception;
   end Push_Cells_Context;

   procedure Pop_Cells_Context
     (Context_Stack  : in out FDT_Cells_Context_Stack_T;
      Popped_Context : out FDT_Cells_Context_T;
      Result         : out Function_Result) is
   begin
      if Context_Stack.Count = 0 then
         Log_Error ("Cells context stack underflow");
         Result := Constraint_Exception;
         return;
      end if;

      pragma Debug (Debug_Devicetree, Log_Debug ("Popping cells context"));

      Popped_Context := Context_Stack.Contexts (Context_Stack.Count);
      Context_Stack.Count := Context_Stack.Count - 1;
      Result := Success;
   exception
      when Constraint_Error =>
         Log_Constraint_Error;
         Result := Constraint_Exception;
   end Pop_Cells_Context;

   procedure Peek_Cells_Context
     (Context_Stack : FDT_Cells_Context_Stack_T;
      Top_Context   : out FDT_Cells_Context_T;
      Result        : out Function_Result) is
   begin
      if Context_Stack.Count = 0 then
         Log_Error ("Cells context stack is empty");
         Result := Constraint_Exception;
         return;
      end if;

      Top_Context := Context_Stack.Contexts (Context_Stack.Count);
      Result := Success;
   exception
      when Constraint_Error =>
         Log_Constraint_Error;
         Result := Constraint_Exception;
   end Peek_Cells_Context;

   ----------------------------------------------------------------------------
   --  Combines up to 2 big-endian cells into one value. 0 cells gives 0.
   ----------------------------------------------------------------------------
   function Read_Cells_Value
     (Value_Address : Address; Cell_Count : Unsigned_32) return Unsigned_64
   is
      Value : Unsigned_64 := 0;
   begin
      --  Each cell is 4 bytes, this loop reads them in big-endian order,
      --  shifting the accumulated value 32 bits left for each cell.
      for I in 1 .. Cell_Count loop
         Read_Cell : declare
            Cell : constant Unsigned_32
            with
              Import,
              Address   => Value_Address + Storage_Offset ((I - 1) * 4),
              Alignment => 1;
         begin
            Value :=
              Shift_Left (Value, 32)
              or Unsigned_64 (Convert_BEU32_To_LEU32 (Cell));
         end Read_Cell;
      end loop;

      return Value;
   end Read_Cells_Value;

   procedure Get_Reg_Entry_Count
     (Property_Length : Unsigned_32;
      Cells_Context   : FDT_Cells_Context_T;
      Entry_Count     : out Natural;
      Result          : out Function_Result) is
   begin
      Entry_Size : constant Unsigned_32 :=
        (Cells_Context.Address_Cells + Cells_Context.Size_Cells) * 4;

      if Entry_Size = 0 then
         Entry_Count := 0;
         Result := Success;
         return;
      end if;

      Entry_Count := Natural (Property_Length / Entry_Size);
      Result := Success;
   exception
      when Constraint_Error =>
         Log_Constraint_Error;
         Entry_Count := 0;
         Result := Constraint_Exception;
   end Get_Reg_Entry_Count;

   procedure Read_Reg_Entry
     (Property_Address : Address;
      Property_Length  : Unsigned_32;
      Cells_Context    : FDT_Cells_Context_T;
      Entry_Index      : Natural;
      Reg_Base         : out Unsigned_64;
      Reg_Size         : out Unsigned_64;
      Result           : out Function_Result)
   is
      Total_Reg_Entry_Count : Natural;
   begin
      Reg_Base := 0;
      Reg_Size := 0;

      --  Values wider than 2 cells don't fit in the 64-bit outputs.
      if Cells_Context.Address_Cells > 2 or else Cells_Context.Size_Cells > 2
      then
         Log_Error ("Unsupported reg cell counts");
         Result := Invalid_Argument;
         return;
      end if;

      Get_Reg_Entry_Count
        (Property_Length, Cells_Context, Total_Reg_Entry_Count, Result);
      if Is_Error (Result) then
         return;
      end if;

      if Entry_Index >= Total_Reg_Entry_Count then
         Log_Error ("reg entry index out of range: " & Entry_Index'Image);
         Result := Invalid_Argument;
         return;
      end if;

      Address_Size : constant Storage_Offset :=
        Storage_Offset (Cells_Context.Address_Cells * 4);

      Entry_Size : constant Storage_Offset :=
        Address_Size + Storage_Offset (Cells_Context.Size_Cells * 4);

      Entry_Address : constant Address :=
        Property_Address + Storage_Offset (Entry_Index) * Entry_Size;

      Reg_Base :=
        Read_Cells_Value (Entry_Address, Cells_Context.Address_Cells);
      Reg_Size :=
        Read_Cells_Value
          (Entry_Address + Address_Size, Cells_Context.Size_Cells);

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Constraint_Error;
         Result := Constraint_Exception;
   end Read_Reg_Entry;

   ----------------------------------------------------------------------------
   --  Converts a base and size pair read from the devicetree into a physical
   --  memory region. The conversion is invalid if the region wraps around
   --  the address space, or if its size is 2**63 or more, which can't be
   --  represented by the native Storage_Count type.
   ----------------------------------------------------------------------------
   procedure Convert_To_Physical_Memory_Region
     (Base   : Unsigned_64;
      Size   : Unsigned_64;
      Region : out Physical_Memory_Region_T;
      Result : out Function_Result) is
   begin
      Region := (others => <>);

      if Size > Unsigned_64'Last - Base then
         Log_Error ("Devicetree region exceeds the address space");
         Result := Invalid_Argument;
         return;
      end if;

      if Size > Unsigned_64 (Storage_Count'Last) then
         Log_Error ("Devicetree region is too large");
         Result := Invalid_Argument;
         return;
      end if;

      Region :=
        (Base => Physical_Address_T (Unsigned_64_To_Address (Base)),
         Size => Storage_Count (Size));

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Constraint_Error;
         Result := Constraint_Exception;
   end Convert_To_Physical_Memory_Region;

   procedure Add_Memory_Region
     (Regions : in out Memory_Region_List_T;
      Count   : in out Natural;
      Region  : Physical_Memory_Region_T;
      Result  : out Function_Result) is
   begin
      Append_Physical_Memory_Region_To_Array (Regions, Count, Region, Result);
      if Is_Error (Result) then
         Log_Error ("Too many memory regions in devicetree");
         return;
      end if;

      Result := Success;
   end Add_Memory_Region;

   procedure Add_Discovered_Device
     (State  : in out FDT_Parser_State_T;
      Region : Physical_Memory_Region_T;
      Result : out Function_Result) is
   begin
      if Region.Size = 0 then
         Result := Success;
         return;
      end if;

      if State.Device_Count >= State.Devices'Last then
         Log_Error ("Too many devices in devicetree");
         Result := Region_Array_Exhausted;
         return;
      end if;

      State.Device_Count := State.Device_Count + 1;
      State.Devices (State.Device_Count) :=
        (Device_Type    => State.Current_Node_Device_Type,
         Base           => Region.Base,
         Size           => Region.Size,
         Interrupt_Line => State.Current_Node_Interrupt_Line);

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Constraint_Error;
         Result := Constraint_Exception;
   end Add_Discovered_Device;

   procedure Parse_Reserved_Memory_Regions
     (State                      : in out FDT_Parser_State_T;
      DTB_Address                : Address;
      Reserved_Memory_Map_Offset : Storage_Offset;
      Total_Header_Size          : Storage_Count;
      Result                     : out Function_Result)
   is
      Curr_Offset     : Storage_Offset := 0;
      Reserved_Region : Physical_Memory_Region_T;
   begin
      if Reserved_Memory_Map_Offset >= Total_Header_Size then
         Log_Error
           ("Reserved memory map offset exceeds total devicetree size");
         Result := Invalid_Argument;
         return;
      end if;

      Reserved_Mem_Map_Address : constant Address :=
        DTB_Address + Reserved_Memory_Map_Offset;

      Reserved_Memory_Map_Size : constant Storage_Count :=
        Total_Header_Size - Reserved_Memory_Map_Offset;

      pragma
        Debug
          (Debug_Devicetree, Log_Debug ("Parsing Reserved Memory Regions..."));

      loop
         if Curr_Offset + 16 > Reserved_Memory_Map_Size then
            Log_Error ("Reserved memory map exceeds total devicetree size.");
            Result := Unhandled_Exception;
            return;
         end if;

         Read_Block : declare
            Block : Reserved_Memory_Block_T
            with
              Import,
              Address   => Reserved_Mem_Map_Address + Curr_Offset,
              Alignment => 1;
         begin
            --  Section 5.3.2 of the Devicetree specification states that
            --  the reserved memory map is terminated by a block with both
            --  address and size set to zero.
            exit when Block.Size = 0 and then Block.Addr = 0;

            Convert_To_Physical_Memory_Region
              (Convert_BEU64_To_LEU64 (Block.Addr),
               Convert_BEU64_To_LEU64 (Block.Size),
               Reserved_Region,
               Result);
            if Is_Error (Result) then
               return;
            end if;

            Add_Memory_Region
              (State.Reserved_Memory_Regions,
               State.Reserved_Memory_Region_Count,
               Reserved_Region,
               Result);
            if Is_Error (Result) then
               return;
            end if;

         end Read_Block;

         --  Advance by the size of the reserved memory block structure.
         Curr_Offset := Curr_Offset + 16;
      end loop;

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Constraint_Error;
         Result := Constraint_Exception;
   end Parse_Reserved_Memory_Regions;

   procedure Read_Node_Name_String
     (Structure_Block_Address : Address;
      Structure_Block_Size    : Storage_Count;
      Curr_Offset             : in out Storage_Offset;
      Node_Name               : in out Devicetree_String_T;
      Result                  : out Function_Result) is
   begin
      --  Set an arbitrary maximum length for the string so that we can be
      --  sure that the loop reading the node name string terminates.
      --  Section 2.2.1 of the Devicetree specification specifies that a node
      --  name string should be between 1-31 characters in length, however
      --  in practice they can be longer. The 1-31 character limit also
      --  doesn't include the unit address suffix.
      Maximum_Possible_String_Length : constant Integer :=
        Integer'Min
          (Integer (Structure_Block_Size - Curr_Offset),
           Maximum_FDT_String_Length);

      if Maximum_Possible_String_Length <= 0 then
         Log_Error ("Unexpected end of structure block");
         Result := Unhandled_Exception;
         return;
      end if;

      Name_String : String (1 .. Maximum_Possible_String_Length)
      with
        Import,
        Address   => Structure_Block_Address + Curr_Offset,
        Alignment => 1;

      for I in Name_String'Range loop
         if Name_String (I) = ASCII.NUL then
            --  Increment the current read offset by the length of the node
            --  name string, taking into account the null terminator.
            Curr_Offset :=
              Curr_Offset + Storage_Offset (Node_Name.Byte_Length) + 1;

            Result := Success;
            return;
         end if;

         Node_Name.Value (I) := Name_String (I);
         Node_Name.Byte_Length := I;
      end loop;

      Log_Error ("Unterminated name string encountered");
      Result := Unhandled_Exception;
   exception
      when Constraint_Error =>
         Log_Constraint_Error;
         Result := Constraint_Exception;
   end Read_Node_Name_String;

   procedure Read_Property_Name_String
     (String_Table_Address : Address;
      String_Table_Size    : Storage_Count;
      String_Table_Offset  : Storage_Count;
      Property_Name        : out Devicetree_String_T;
      Result               : out Function_Result) is
   begin
      Property_Name.Byte_Length := 0;

      --  Set an arbitrary maximum length for the string so that we can be
      --  sure that the loop reading the property name string terminates.
      --  Section 2.2.4 of the Devicetree specification specifies that a
      --  property name should be between 1-31 characters in length, however
      --  in practice they can be longer.
      Maximum_Possible_String_Length : constant Integer :=
        Integer'Min
          (Integer (String_Table_Size - String_Table_Offset),
           Maximum_FDT_String_Length);

      if Maximum_Possible_String_Length <= 0 then
         Log_Error ("Unexpected end of string table");
         Result := Unhandled_Exception;
         return;
      end if;

      Property_Name_String : String (1 .. Maximum_Possible_String_Length)
      with
        Import,
        Convention => C,
        Address    => String_Table_Address + String_Table_Offset,
        Alignment  => 1;

      for I in Property_Name_String'Range loop
         if Property_Name_String (I) = ASCII.NUL then
            --  Note that this procedure does not update the current read
            --  offset into the structure block. Since we're already reading a
            --  property at this point, the caller will advance the read offset
            --  by the entire property size.
            Result := Success;
            return;
         end if;

         Property_Name.Value (I) := Property_Name_String (I);
         Property_Name.Byte_Length := I;
      end loop;

      Log_Error ("Unterminated property name string encountered");
      Result := Unhandled_Exception;
   exception
      when Constraint_Error =>
         Log_Constraint_Error;
         Result := Constraint_Exception;
   end Read_Property_Name_String;

   function Is_String_Value
     (Property_Name : Devicetree_String_T) return Boolean
   is
      --  For simplicity, assume properties with names ending in
      --  "compatible", "model", or "device_type" are string values.
      (Compare_Fixed_Length_String_With_String (Property_Name, "compatible")
       or else Compare_Fixed_Length_String_With_String (Property_Name, "model")
       or else
         Compare_Fixed_Length_String_With_String
           (Property_Name, "device_type"));

   procedure Print_Devicetree_Header (Header : FDT_Header_T) is
   begin
      Log_Debug
        ("Devicetree Header:"
         & ASCII.LF
         & "  Magic:            "
         & Convert_BEU32_To_LEU32 (Header.Magic)'Image
         & ASCII.LF
         & "  Totalsize:        "
         & Convert_BEU32_To_LEU32 (Header.Totalsize)'Image
         & ASCII.LF
         & "  Off_DT_Struct:    "
         & Convert_BEU32_To_LEU32 (Header.Off_DT_Struct)'Image
         & ASCII.LF
         & "  Off_DT_Strings:   "
         & Convert_BEU32_To_LEU32 (Header.Off_DT_Strings)'Image
         & ASCII.LF
         & "  Off_Mem_Rsvmap:   "
         & Convert_BEU32_To_LEU32 (Header.Off_Mem_Rsvmap)'Image
         & ASCII.LF
         & "  Version:          "
         & Convert_BEU32_To_LEU32 (Header.Version)'Image
         & ASCII.LF
         & "  Size_DT_Strings:  "
         & Convert_BEU32_To_LEU32 (Header.Size_DT_Strings)'Image
         & ASCII.LF
         & "  Size_DT_Struct:   "
         & Convert_BEU32_To_LEU32 (Header.Size_DT_Struct)'Image);

   exception
      when Constraint_Error =>
         Log_Constraint_Error;
   end Print_Devicetree_Header;

   ----------------------------------------------------------------------------
   --  Rounds an offset into the structure block up to the next 4-byte
   --  boundary, which is where the next token begins.
   ----------------------------------------------------------------------------
   procedure Align_Offset_To_Word
     (Offset : in out Storage_Offset; Result : out Function_Result) is
   begin
      Offset := (Offset + 3) / 4 * 4;
      Result := Success;
   exception
      when Constraint_Error =>
         Log_Constraint_Error;
         Result := Constraint_Exception;
   end Align_Offset_To_Word;

   procedure Parse_Property
     (State    : in out FDT_Parser_State_T;
      Property : out Devicetree_Property_T;
      Result   : out Function_Result)
   is
      Raw_Property : constant FDT_Property_T
      with
        Import,
        Address   => State.Structure_Block_Address + State.Curr_Offset,
        Alignment => 1;
   begin
      Property.Length := 0;
      Property.Value_Address := Null_Address;

      Read_Property_Name_String
        (State.String_Table_Address,
         State.String_Table_Size,
         Storage_Offset (Convert_BEU32_To_LEU32 (Raw_Property.Name_Offset)),
         Property.Name,
         Result);
      if Is_Error (Result) then
         return;
      end if;

      Property.Length := Convert_BEU32_To_LEU32 (Raw_Property.Length);
      Property.Value_Address :=
        State.Structure_Block_Address + State.Curr_Offset + 8;

      --  Advance the read offset by the size of the property structure
      --  (8 bytes) plus the length of the property value.
      State.Curr_Offset :=
        State.Curr_Offset + 8 + Storage_Offset (Property.Length);

      if State.Curr_Offset >= State.Structure_Block_Size then
         Log_Error ("Property exceeds structure block size.");
         Result := Unhandled_Exception;
         return;
      end if;

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Constraint_Error;
         Result := Constraint_Exception;
   end Parse_Property;

   procedure Get_Next_Token
     (State       : in out FDT_Parser_State_T;
      Token_Value : out Unsigned_32;
      Result      : out Function_Result)
   is
      Curr_Token : constant FDT_Token_T
      with
        Import,
        Address   => State.Structure_Block_Address + State.Curr_Offset,
        Alignment => 1;
   begin
      if State.Curr_Offset >= State.Structure_Block_Size then
         Log_Error ("End of structure block reached while reading token");
         Result := Unhandled_Exception;
         Token_Value := 0;
         return;
      end if;

      Token_Value := Convert_BEU32_To_LEU32 (Curr_Token);

      --  Advance the read offset by the size of the token (4 bytes).
      State.Curr_Offset := State.Curr_Offset + 4;
      Result := Success;
   exception
      when Constraint_Error =>
         Log_Constraint_Error;
         Result := Constraint_Exception;
   end Get_Next_Token;

   procedure Log_Node_Name
     (Node_Name : Devicetree_String_T;
      File      : String := GNAT.Source_Info.File;
      Line      : Positive := GNAT.Source_Info.Line) is
   begin
      Log_Debug
        ("  Node Name: '" & Node_Name.Value (1 .. Node_Name.Byte_Length) & "'",
         File,
         Line);
   exception
      when Constraint_Error =>
         Log_Constraint_Error;
   end Log_Node_Name;

   procedure Log_Cells_Context
     (Cells_Context : FDT_Cells_Context_T;
      File          : String := GNAT.Source_Info.File;
      Line          : Positive := GNAT.Source_Info.Line) is
   begin
      Log_Debug
        ("Node cells context: ("
         & "Address_Cells: "
         & Cells_Context.Address_Cells'Image
         & ", Size_Cells: "
         & Cells_Context.Size_Cells'Image
         & ")",
         File,
         Line);
   exception
      when Constraint_Error =>
         Log_Constraint_Error;
   end Log_Cells_Context;

   procedure Log_Property
     (Property : Devicetree_Property_T;
      File     : String := GNAT.Source_Info.File;
      Line     : Positive := GNAT.Source_Info.Line) is
   begin
      Log_Debug ("Property:", File, Line);
      Log_Debug
        ("  Name: '"
         & Property.Name.Value (1 .. Property.Name.Byte_Length)
         & "'",
         File,
         Line);

      if Property.Length = 0 then
         Log_Debug ("  (No Value)", File, Line);
      elsif Is_String_Value (Property.Name) then
         Read_Property_Value : declare
            Property_Value : String (1 .. Integer (Property.Length))
            with Import, Address => Property.Value_Address, Alignment => 1;
         begin
            Log_Debug ("  Value: '" & Property_Value & "'", File, Line);
         end Read_Property_Value;
      end if;
   exception
      when Constraint_Error =>
         Log_Constraint_Error;
   end Log_Property;

   ----------------------------------------------------------------------------
   --  Reads the first cell of a property, such as 'interrupts'. Any further
   --  cells are ignored.
   ----------------------------------------------------------------------------
   procedure Read_First_Cell_Property
     (Property   : Devicetree_Property_T;
      Cell_Value : out Unsigned_32;
      Result     : out Function_Result)
   is
      Property_Value : constant Unsigned_32
      with Import, Address => Property.Value_Address, Alignment => 1;
   begin
      Cell_Value := 0;

      if Property.Length < 4 then
         Log_Error ("Invalid cell property length");
         Result := Invalid_Argument;
         return;
      end if;

      Cell_Value := Convert_BEU32_To_LEU32 (Property_Value);
      Result := Success;
   end Read_First_Cell_Property;

   ----------------------------------------------------------------------------
   --  Reads the value of an #address-cells or #size-cells property.
   ----------------------------------------------------------------------------
   procedure Read_Cells_Property
     (Property    : Devicetree_Property_T;
      Cells_Value : out Unsigned_32;
      Result      : out Function_Result) is
   begin
      if Property.Length /= 4 then
         Cells_Value := 0;
         Log_Error ("Invalid cells property length");
         Result := Invalid_Argument;
         return;
      end if;

      Read_First_Cell_Property (Property, Cells_Value, Result);
   end Read_Cells_Property;

   ----------------------------------------------------------------------------
   --  Reads a property that holds one 32-bit or 64-bit number, such as
   --  'timebase-frequency'.
   ----------------------------------------------------------------------------
   procedure Read_Numeric_Property
     (Property : Devicetree_Property_T;
      Value    : out Unsigned_64;
      Result   : out Function_Result) is
   begin
      Value := 0;

      if Property.Length = 4 then
         Value := Read_Cells_Value (Property.Value_Address, 1);
      elsif Property.Length = 8 then
         Value := Read_Cells_Value (Property.Value_Address, 2);
      else
         Log_Error ("Invalid number property length");
         Result := Invalid_Argument;
         return;
      end if;

      Result := Success;
   end Read_Numeric_Property;

   function Compare_Property_Value_With_String
     (Property : Devicetree_Property_T; Value : String) return Boolean is
   begin
      --  The stored value includes the null terminator.
      if Property.Length /= Unsigned_32 (Value'Length) + 1 then
         return False;
      end if;

      Compare_Property_Value : declare
         Property_Value : String (1 .. Value'Length + 1)
         with Import, Address => Property.Value_Address, Alignment => 1;
      begin
         return
           Property_Value (1 .. Value'Length) = Value
           and then Property_Value (Property_Value'Last) = ASCII.NUL;
      end Compare_Property_Value;
   exception
      when Constraint_Error =>
         Log_Constraint_Error;
         return False;
   end Compare_Property_Value_With_String;

   function Is_Reserved_Memory_Child_Node
     (State : FDT_Parser_State_T) return Boolean is
   begin
      return
        State.Reserved_Memory_Node_Depth /= 0
        and then
          State.Cells_Context_Stack.Count
          = State.Reserved_Memory_Node_Depth + 1;
   exception
      when Constraint_Error =>
         Log_Constraint_Error;
         return False;
   end Is_Reserved_Memory_Child_Node;

   ----------------------------------------------------------------------------
   --  Decodes the current node's 'reg' property into base and size pairs.
   --  Entry_Count is 0 if the parent's cell sizes are too wide to decode.
   ----------------------------------------------------------------------------
   procedure Parse_Reg_Property
     (State       : FDT_Parser_State_T;
      Entries     : out Memory_Region_List_T;
      Entry_Count : out Natural;
      Result      : out Function_Result)
   is
      Parent_Cells_Context  : FDT_Cells_Context_T;
      Total_Reg_Entry_Count : Natural := 0;
      Reg_Base              : Unsigned_64 := 0;
      Reg_Size              : Unsigned_64 := 0;
   begin
      Entry_Count := 0;

      --  A node's 'reg' uses its parent's cells context, which was pushed on
      --  entry to this node.
      Peek_Cells_Context
        (State.Cells_Context_Stack, Parent_Cells_Context, Result);
      if Is_Error (Result) then
         return;
      end if;

      --  If the parent node's address-cells or size-cells exceed 2, we cannot
      --  parse the 'reg' property into 64-bit values.
      --  This might occur in PCI device nodes.
      if Parent_Cells_Context.Address_Cells > 2
        or else Parent_Cells_Context.Size_Cells > 2
      then
         Log_Error
           ("Skipping 'reg' with unsupported cell sizes in node '"
            & State.Current_Node_Name.Value
                (1 .. State.Current_Node_Name.Byte_Length)
            & "'");
         Result := Success;
         return;
      end if;

      Get_Reg_Entry_Count
        (State.Current_Node_Reg_Property.Length,
         Parent_Cells_Context,
         Total_Reg_Entry_Count,
         Result);
      if Is_Error (Result) then
         return;
      end if;

      if Total_Reg_Entry_Count > Entries'Length then
         Log_Error
           ("Too many 'reg' entries in node '"
            & State.Current_Node_Name.Value
                (1 .. State.Current_Node_Name.Byte_Length)
            & "'");
         Result := Region_Array_Exhausted;
         return;
      end if;

      for Entry_Index in 0 .. Total_Reg_Entry_Count - 1 loop
         Read_Reg_Entry
           (State.Current_Node_Reg_Property.Value_Address,
            State.Current_Node_Reg_Property.Length,
            Parent_Cells_Context,
            Entry_Index,
            Reg_Base,
            Reg_Size,
            Result);
         if Is_Error (Result) then
            return;
         end if;

         pragma
           Debug
             (Debug_Devicetree,
              Log_Debug
                ("  Reg"
                 & Entry_Index'Image
                 & ": Base="
                 & Reg_Base'Image
                 & ", Size="
                 & Reg_Size'Image));

         Convert_To_Physical_Memory_Region
           (Reg_Base, Reg_Size, Entries (Entries'First + Entry_Index), Result);
         if Is_Error (Result) then
            return;
         end if;
      end loop;

      Entry_Count := Total_Reg_Entry_Count;

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Constraint_Error;
         Result := Constraint_Exception;
   end Parse_Reg_Property;

   ----------------------------------------------------------------------------
   --  Checks whether any of the null-terminated strings in a 'stringlist'
   --  property, such as 'compatible', contains the given string.
   --  Bytes after the last null terminator are ignored.
   ----------------------------------------------------------------------------
   function Does_Stringlist_Property_Contain_String
     (Property : Devicetree_Property_T; Value : String) return Boolean
   is
      Start_Position : Positive := 1;
   begin
      Read_Stringlist : declare
         Stringlist : constant String (1 .. Integer (Property.Length))
         with Import, Address => Property.Value_Address, Alignment => 1;
      begin
         for I in Stringlist'Range loop
            if Stringlist (I) = ASCII.NUL then
               if Stringlist (Start_Position .. I - 1) = Value then
                  return True;
               end if;

               Start_Position := I + 1;
            end if;
         end loop;
      end Read_Stringlist;

      return False;
   exception
      when Constraint_Error =>
         Log_Constraint_Error;
         return False;
   end Does_Stringlist_Property_Contain_String;

   function Get_Device_Type_From_Compatible
     (Property : Devicetree_Property_T) return Devicetree_Device_Type_T is
   begin
      if Does_Stringlist_Property_Contain_String (Property, "riscv,plic0") then
         return Devicetree_Device_Type_PLIC;
      elsif Does_Stringlist_Property_Contain_String (Property, "ns16550a") then
         return Devicetree_Device_Type_UART_NS16550A;
      elsif Does_Stringlist_Property_Contain_String (Property, "virtio,mmio")
      then
         return Devicetree_Device_Type_Virtio_MMIO;
      end if;

      return Devicetree_Device_Type_None;
   end Get_Device_Type_From_Compatible;

   ----------------------------------------------------------------------------
   --  'Finalise' the node after all its properties have been read, since the
   --  standard does not dictate the order in which a node's properties appear.
   --  Section 5.4.2 of the Devicetree specification requires a node's
   --  properties to precede its child nodes, so this is called at the start
   --  of the first child node, or at the end of the node. As such, this
   --  procedure needs to be idempotent.
   ----------------------------------------------------------------------------
   procedure Finalise_Node_Properties
     (State : in out FDT_Parser_State_T; Result : out Function_Result)
   is
      Reg_Entries     : Memory_Region_List_T;
      Reg_Entry_Count : Natural := 0;
   begin
      --  Only decode 'reg' for the nodes the parser uses.
      if State.Current_Node_Reg_Property.Value_Address /= Null_Address
        and then
          (State.Current_Node_Is_Memory
           or else Is_Reserved_Memory_Child_Node (State)
           or else
             State.Current_Node_Device_Type /= Devicetree_Device_Type_None)
      then
         Parse_Reg_Property (State, Reg_Entries, Reg_Entry_Count, Result);
         if Is_Error (Result) then
            return;
         end if;
      end if;

      if State.Current_Node_Is_Memory then
         for Reg_Entry of Reg_Entries (1 .. Reg_Entry_Count) loop
            Add_Memory_Region
              (State.Memory_Regions,
               State.Memory_Region_Count,
               Reg_Entry,
               Result);
            if Is_Error (Result) then
               return;
            end if;
         end loop;

      elsif Is_Reserved_Memory_Child_Node (State) then
         for Reg_Entry of Reg_Entries (1 .. Reg_Entry_Count) loop
            Add_Memory_Region
              (State.Reserved_Memory_Regions,
               State.Reserved_Memory_Region_Count,
               Reg_Entry,
               Result);
            if Is_Error (Result) then
               return;
            end if;
         end loop;

      elsif State.Current_Node_Device_Type /= Devicetree_Device_Type_None
        and then Reg_Entry_Count > 0
      then
         --  Every recognised device has its registers in the first entry.
         Add_Discovered_Device (State, Reg_Entries (1), Result);
         if Is_Error (Result) then
            return;
         end if;
      end if;

      --  Clear the current node's state.
      State.Current_Node_Reg_Property.Value_Address := Null_Address;
      State.Current_Node_Is_Memory := False;
      State.Current_Node_Device_Type := Devicetree_Device_Type_None;
      State.Current_Node_Interrupt_Line := 0;

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Constraint_Error;
         Result := Constraint_Exception;
   end Finalise_Node_Properties;

   procedure Handle_Begin_Node
     (State : in out FDT_Parser_State_T; Result : out Function_Result) is
   begin
      pragma Debug (Debug_Devicetree, Log_Debug ("START STRUCTURE"));

      --  Finalise the current node's properties before starting a new node.
      --  This runs before the push, so the parent node's 'reg' is still
      --  decoded with its own parent's cells context.
      Finalise_Node_Properties (State, Result);
      if Is_Error (Result) then
         return;
      end if;

      State.Current_Node_Name.Byte_Length := 0;

      Push_Cells_Context
        (State.Cells_Context_Stack, State.Current_Cells_Context, Result);
      if Is_Error (Result) then
         return;
      end if;

      Read_Node_Name_String
        (State.Structure_Block_Address,
         State.Structure_Block_Size,
         State.Curr_Offset,
         State.Current_Node_Name,
         Result);
      if Is_Error (Result) then
         return;
      end if;

      if Compare_Node_Name (State.Current_Node_Name, "reserved-memory") then
         State.Reserved_Memory_Node_Depth := State.Cells_Context_Stack.Count;
      end if;

      Align_Offset_To_Word (State.Curr_Offset, Result);
      if Is_Error (Result) then
         return;
      end if;

      pragma Debug (Debug_Devicetree, Log_Node_Name (State.Current_Node_Name));
   end Handle_Begin_Node;

   procedure Handle_End_Node
     (State : in out FDT_Parser_State_T; Result : out Function_Result) is
   begin
      pragma
        Debug
          (Debug_Devicetree, Log_Cells_Context (State.Current_Cells_Context));

      --  If the node hasn't already been finalised before parsing its first
      --  child node, finalise it here.
      Finalise_Node_Properties (State, Result);
      if Is_Error (Result) then
         return;
      end if;

      if State.Cells_Context_Stack.Count = State.Reserved_Memory_Node_Depth
      then
         State.Reserved_Memory_Node_Depth := 0;
      end if;

      Pop_Cells_Context
        (State.Cells_Context_Stack, State.Current_Cells_Context, Result);
      if Is_Error (Result) then
         return;
      end if;

      pragma Debug (Debug_Devicetree, Log_Debug ("END STRUCTURE"));
   end Handle_End_Node;

   procedure Handle_Property
     (State : in out FDT_Parser_State_T; Result : out Function_Result)
   is
      Property : Devicetree_Property_T;
   begin
      Parse_Property (State, Property, Result);
      if Is_Error (Result) then
         return;
      end if;

      pragma Debug (Debug_Devicetree, Log_Property (Property));

      if Compare_Fixed_Length_String_With_String
           (Property.Name, "#address-cells")
      then
         Read_Cells_Property
           (Property, State.Current_Cells_Context.Address_Cells, Result);
      elsif Compare_Fixed_Length_String_With_String
              (Property.Name, "#size-cells")
      then
         Read_Cells_Property
           (Property, State.Current_Cells_Context.Size_Cells, Result);
      elsif Compare_Fixed_Length_String_With_String (Property.Name, "reg") then
         State.Current_Node_Reg_Property := Property;
      elsif Compare_Fixed_Length_String_With_String
              (Property.Name, "device_type")
      then
         State.Current_Node_Is_Memory :=
           Compare_Property_Value_With_String (Property, "memory");
      elsif Compare_Fixed_Length_String_With_String
              (Property.Name, "compatible")
      then
         State.Current_Node_Device_Type :=
           Get_Device_Type_From_Compatible (Property);
      elsif Compare_Fixed_Length_String_With_String
              (Property.Name, "interrupts")
      then
         Read_First_Cell_Property
           (Property, State.Current_Node_Interrupt_Line, Result);
      elsif Compare_Fixed_Length_String_With_String
              (Property.Name, "timebase-frequency")
        --  The root node is at depth 1, so '/cpus' is at depth 2.
        and then State.Cells_Context_Stack.Count = 2
        and then Compare_Node_Name (State.Current_Node_Name, "cpus")
      then
         Read_Numeric_Property (Property, State.Timebase_Frequency, Result);
      end if;

      if Is_Error (Result) then
         return;
      end if;

      Align_Offset_To_Word (State.Curr_Offset, Result);
   end Handle_Property;

   procedure Parse_Structure_Block
     (State : in out FDT_Parser_State_T; Result : out Function_Result)
   is
      FDT_BEGIN_NODE : constant := 1;
      FDT_END_NODE   : constant := 2;
      FDT_PROP       : constant := 3;
      FDT_NOP        : constant := 4;
      FDT_END        : constant := 9;

      Token_Value : Unsigned_32 := 0;
   begin
      loop
         Get_Next_Token (State, Token_Value, Result);
         if Is_Error (Result) then
            return;
         end if;

         case Token_Value is
            when FDT_BEGIN_NODE =>
               Handle_Begin_Node (State, Result);

            when FDT_END_NODE   =>
               Handle_End_Node (State, Result);

            when FDT_PROP       =>
               Handle_Property (State, Result);

            when FDT_NOP        =>
               pragma Debug (Debug_Devicetree, Log_Debug ("NOP"));

            when FDT_END        =>
               pragma Debug (Debug_Devicetree, Log_Debug ("END"));
               exit;

            when others         =>
               Log_Error ("Unknown token type: " & Token_Value'Image);
               Result := Invalid_Argument;
         end case;

         if Is_Error (Result) then
            return;
         end if;
      end loop;

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Constraint_Error;
         Result := Constraint_Exception;
   end Parse_Structure_Block;

   procedure Remove_Reserved_Memory_Regions_From_Memory_Regions
     (State : in out FDT_Parser_State_T; Result : out Function_Result) is
   begin
      --  Each pass removes one reserved region from every memory region, so
      --  later passes see the pieces that earlier ones left.
      for Reserved_Region of
        State.Reserved_Memory_Regions (1 .. State.Reserved_Memory_Region_Count)
      loop
         Remove_Physical_Memory_Region_From_Array
           (State.Memory_Regions,
            State.Memory_Region_Count,
            Reserved_Region,
            Result);
         if Is_Error (Result) then
            Log_Error ("Error removing a reserved region from memory");
            return;
         end if;
      end loop;

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Constraint_Error;
         Result := Constraint_Exception;
   end Remove_Reserved_Memory_Regions_From_Memory_Regions;

   procedure Copy_Parsed_Memory_Regions_To_Result
     (State               : FDT_Parser_State_T;
      Memory_Regions      : out Physical_Memory_Region_Array_T;
      Memory_Region_Count : out Natural;
      Result              : out Function_Result) is
   begin
      Memory_Region_Count := 0;

      if State.Memory_Region_Count > Memory_Regions'Length then
         Log_Error ("Too many memory regions for the output array");
         Result := Region_Array_Exhausted;
         return;
      end if;

      Memory_Regions
        (Memory_Regions'First
         .. Memory_Regions'First + State.Memory_Region_Count - 1) :=
        State.Memory_Regions (1 .. State.Memory_Region_Count);

      Memory_Region_Count := State.Memory_Region_Count;

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Constraint_Error;
         Memory_Region_Count := 0;
         Result := Constraint_Exception;
   end Copy_Parsed_Memory_Regions_To_Result;

   procedure Copy_Discovered_Devices_To_Result
     (State        : FDT_Parser_State_T;
      Devices      : out Devicetree_Device_Array_T;
      Device_Count : out Natural;
      Result       : out Function_Result) is
   begin
      Device_Count := 0;

      if State.Device_Count > Devices'Length then
         Log_Error ("Too many devices for the output array");
         Result := Region_Array_Exhausted;
         return;
      end if;

      Devices (Devices'First .. Devices'First + State.Device_Count - 1) :=
        State.Devices (1 .. State.Device_Count);

      Device_Count := State.Device_Count;

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Constraint_Error;
         Device_Count := 0;
         Result := Constraint_Exception;
   end Copy_Discovered_Devices_To_Result;

   procedure Parse_Devicetree
     (DTB_Address         : Address;
      Memory_Regions      : out Physical_Memory_Region_Array_T;
      Memory_Region_Count : out Natural;
      Devices             : out Devicetree_Device_Array_T;
      Device_Count        : out Natural;
      Timebase_Frequency  : out Unsigned_64;
      Result              : out Function_Result)
   is
      Header : constant FDT_Header_T
      with Import, Address => DTB_Address, Alignment => 1;

      State : FDT_Parser_State_T;
   begin
      Memory_Region_Count := 0;
      Device_Count := 0;
      Timebase_Frequency := 0;

      pragma Debug (Debug_Devicetree, Print_Devicetree_Header (Header));

      if Convert_BEU32_To_LEU32 (Header.Magic) /= FDT_MAGIC_NUMBER then
         Log_Error ("Invalid devicetree magic value.");
         Result := Unhandled_Exception;
         return;
      end if;

      State.String_Table_Address :=
        DTB_Address
        + Storage_Offset (Convert_BEU32_To_LEU32 (Header.Off_DT_Strings));

      State.String_Table_Size :=
        Storage_Count (Convert_BEU32_To_LEU32 (Header.Size_DT_Strings));

      State.Structure_Block_Address :=
        DTB_Address
        + Storage_Offset (Convert_BEU32_To_LEU32 (Header.Off_DT_Struct));

      State.Structure_Block_Size :=
        Storage_Count (Convert_BEU32_To_LEU32 (Header.Size_DT_Struct));

      Parse_Reserved_Memory_Regions
        (State,
         DTB_Address,
         Storage_Offset (Convert_BEU32_To_LEU32 (Header.Off_Mem_Rsvmap)),
         Storage_Count (Convert_BEU32_To_LEU32 (Header.Totalsize)),
         Result);
      if Is_Error (Result) then
         return;
      end if;

      Parse_Structure_Block (State, Result);
      if Is_Error (Result) then
         return;
      end if;

      Remove_Reserved_Memory_Regions_From_Memory_Regions (State, Result);
      if Is_Error (Result) then
         return;
      end if;

      Copy_Parsed_Memory_Regions_To_Result
        (State, Memory_Regions, Memory_Region_Count, Result);
      if Is_Error (Result) then
         return;
      end if;

      Copy_Discovered_Devices_To_Result (State, Devices, Device_Count, Result);
      if Is_Error (Result) then
         return;
      end if;

      Timebase_Frequency := State.Timebase_Frequency;

      Result := Success;
   end Parse_Devicetree;

   function Compare_Node_Name
     (Node_Name : Devicetree_String_T; Target_Name : String) return Boolean is
   begin
      --  Return early if the lengths don't match, taking into account the
      --  fact that a node name may include a unit address at the end, after
      --  an '@' character.
      if Target_Name'Length > Node_Name.Byte_Length then
         return False;
      end if;

      for I in Target_Name'Range loop
         if Node_Name.Value (I - Target_Name'First + 1) /= Target_Name (I) then
            return False;
         end if;
      end loop;

      --  Since we've compared every character up to Target_Name'Length, we can
      --  check the entire string is equal by comparing the lengths.
      if Node_Name.Byte_Length = Target_Name'Length then
         return True;
      end if;

      --  If the whole name doesn't match at this point, we can assume that
      --  the name is *at least* Target_Name'Length + 1 characters long.
      --  In this case, check if the next character is an '@' signifying a
      --  unit address suffix.
      return Node_Name.Value (Target_Name'Length + 1) = ASCII.At_Sign;
   exception
      when Constraint_Error =>
         Log_Constraint_Error;
         return False;
   end Compare_Node_Name;

end Devicetree;
