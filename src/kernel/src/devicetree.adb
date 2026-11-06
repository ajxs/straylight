with System.Storage_Elements; use System.Storage_Elements;

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

   --  A range of physical memory, as raw Devicetree values. The parser works
   --  in Unsigned_64, and only converts to Physical_Memory_Region_T when it
   --  returns the regions.
   type Memory_Region_T is record
      Base : Unsigned_64 := 0;
      Size : Unsigned_64 := 0;
   end record;

   Maximum_Memory_Regions : constant := 16;

   type Memory_Region_Array_T is
     array (1 .. Maximum_Memory_Regions) of Memory_Region_T;

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

      --  This variable is used to determine whether the parser is currently
      --  inside a 'reserved-memory' child node.
      --  When entering a reserved memory parent node, this is set to the
      --  current depth of the cells context stack.
      Reserved_Memory_Node_Depth : Natural := 0;

      Memory_Regions      : Memory_Region_Array_T;
      Memory_Region_Count : Natural := 0;

      Reserved_Memory_Regions      : Memory_Region_Array_T;
      Reserved_Memory_Region_Count : Natural := 0;
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

   procedure Add_Memory_Region
     (Regions : in out Memory_Region_Array_T;
      Count   : in out Natural;
      Base    : Unsigned_64;
      Size    : Unsigned_64;
      Result  : out Function_Result) is
   begin
      if Size = 0 then
         Result := Success;
         return;
      end if;

      --  Ensure that the memory region size doesn't overflow the address
      --  space, and wrap around.
      if Size > Unsigned_64'Last - Base then
         Log_Error ("Memory region exceeds the address space");
         Result := Invalid_Argument;
         return;
      end if;

      if Count >= Regions'Last then
         Log_Error ("Too many memory regions in devicetree");
         Result := Region_Array_Exhausted;
         return;
      end if;

      Count := Count + 1;
      Regions (Count) := (Base, Size);

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Constraint_Error;
         Result := Constraint_Exception;
   end Add_Memory_Region;

   procedure Parse_Reserved_Memory_Regions
     (State                      : in out FDT_Parser_State_T;
      DTB_Address                : Address;
      Reserved_Memory_Map_Offset : Storage_Offset;
      Total_Header_Size          : Storage_Count;
      Result                     : out Function_Result)
   is
      Curr_Offset : Storage_Offset := 0;
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

            Add_Memory_Region
              (State.Reserved_Memory_Regions,
               State.Reserved_Memory_Region_Count,
               Convert_BEU64_To_LEU64 (Block.Addr),
               Convert_BEU64_To_LEU64 (Block.Size),
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

   procedure Log_Node_Name (Node_Name : Devicetree_String_T) is
   begin
      Log_Debug
        ("  Node Name: '"
         & Node_Name.Value (1 .. Node_Name.Byte_Length)
         & "'");
   exception
      when Constraint_Error =>
         Log_Constraint_Error;
   end Log_Node_Name;

   procedure Log_Cells_Context (Cells_Context : FDT_Cells_Context_T) is
   begin
      Log_Debug
        ("Node cells context: ("
         & "Address_Cells: "
         & Cells_Context.Address_Cells'Image
         & ", Size_Cells: "
         & Cells_Context.Size_Cells'Image
         & ")");
   exception
      when Constraint_Error =>
         Log_Constraint_Error;
   end Log_Cells_Context;

   procedure Log_Property (Property : Devicetree_Property_T) is
   begin
      Log_Debug ("Property:");
      Log_Debug
        ("  Name: '"
         & Property.Name.Value (1 .. Property.Name.Byte_Length)
         & "'");

      if Property.Length = 0 then
         Log_Debug ("  (No Value)");
      elsif Is_String_Value (Property.Name) then
         Read_Property_Value : declare
            Property_Value : String (1 .. Integer (Property.Length))
            with Import, Address => Property.Value_Address, Alignment => 1;
         begin
            Log_Debug ("  Value: '" & Property_Value & "'");
         end Read_Property_Value;
      end if;
   exception
      when Constraint_Error =>
         Log_Constraint_Error;
   end Log_Property;

   ----------------------------------------------------------------------------
   --  Reads the value of an #address-cells or #size-cells property.
   ----------------------------------------------------------------------------
   procedure Read_Cells_Property
     (Property    : Devicetree_Property_T;
      Cells_Value : out Unsigned_32;
      Result      : out Function_Result)
   is
      Property_Value : constant Unsigned_32
      with Import, Address => Property.Value_Address, Alignment => 1;
   begin
      Cells_Value := 0;

      if Property.Length /= 4 then
         Log_Error ("Invalid cells property length");
         Result := Invalid_Argument;
         return;
      end if;

      Cells_Value := Convert_BEU32_To_LEU32 (Property_Value);
      Result := Success;
   end Read_Cells_Property;

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

   procedure Parse_Reg_Property
     (State : in out FDT_Parser_State_T; Result : out Function_Result)
   is
      Parent_Cells_Context  : FDT_Cells_Context_T;
      Total_Reg_Entry_Count : Natural := 0;
      Reg_Base              : Unsigned_64 := 0;
      Reg_Size              : Unsigned_64 := 0;
   begin
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

         if State.Current_Node_Is_Memory then
            Add_Memory_Region
              (State.Memory_Regions,
               State.Memory_Region_Count,
               Reg_Base,
               Reg_Size,
               Result);
            if Is_Error (Result) then
               return;
            end if;

         elsif Is_Reserved_Memory_Child_Node (State) then
            Add_Memory_Region
              (State.Reserved_Memory_Regions,
               State.Reserved_Memory_Region_Count,
               Reg_Base,
               Reg_Size,
               Result);
            if Is_Error (Result) then
               return;
            end if;
         end if;

      end loop;

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Constraint_Error;
         Result := Constraint_Exception;
   end Parse_Reg_Property;

   ----------------------------------------------------------------------------
   --  'Finalise' the node after all its properties have been read, since the
   --  standard does not dictate the order in which a node's properties appear.
   --  Section 5.4.2 of the Devicetree specification requires a node's
   --  properties to precede its child nodes, so this is called at the start
   --  of the first child node, or at the end of the node. As such, this
   --  procedure needs to be idempotent.
   ----------------------------------------------------------------------------
   procedure Finalise_Node_Properties
     (State : in out FDT_Parser_State_T; Result : out Function_Result) is
   begin
      if State.Current_Node_Reg_Property.Value_Address /= Null_Address then
         Parse_Reg_Property (State, Result);
         if Is_Error (Result) then
            return;
         end if;
      end if;

      --  Clear the current node's state.
      State.Current_Node_Reg_Property.Value_Address := Null_Address;
      State.Current_Node_Is_Memory := False;

      Result := Success;
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

   ----------------------------------------------------------------------------
   --  Takes a memory region and a reserved region, and creates a new set of
   --  regions representing the parts of the memory region not covered by the
   --  reserved region.
   --  This works because Add_Memory_Region ignores empty regions, so any part
   --  of the memory region that is completely covered by the reserved region
   --  won't be added to the new set of regions.
   ----------------------------------------------------------------------------
   procedure Create_New_Regions_With_Reserved_Regions_Removed
     (Regions         : in out Memory_Region_Array_T;
      Count           : in out Natural;
      Memory_Region   : Memory_Region_T;
      Reserved_Region : Memory_Region_T;
      Result          : out Function_Result)
   is
      --  Add_Memory_Region rejects regions that wrap around the address
      --  space, and Remove_Region_From_Memory_Regions checks the region it
      --  removes, so neither end can overflow.
      Memory_End : constant Unsigned_64 :=
        Memory_Region.Base + Memory_Region.Size;

      Reserved_End : constant Unsigned_64 :=
        Reserved_Region.Base + Reserved_Region.Size;

      --  The reserved region's ends, clamped to the memory region.
      Lower_End : constant Unsigned_64 :=
        Unsigned_64'Min
          (Unsigned_64'Max (Reserved_Region.Base, Memory_Region.Base),
           Memory_End);

      Upper_Base : constant Unsigned_64 :=
        Unsigned_64'Min
          (Unsigned_64'Max (Reserved_End, Memory_Region.Base), Memory_End);
   begin
      Add_Memory_Region
        (Regions,
         Count,
         Memory_Region.Base,
         Lower_End - Memory_Region.Base,
         Result);
      if Is_Error (Result) then
         return;
      end if;

      Add_Memory_Region
        (Regions, Count, Upper_Base, Memory_End - Upper_Base, Result);
   end Create_New_Regions_With_Reserved_Regions_Removed;

   procedure Remove_Region_From_Memory_Regions
     (Regions : in out Memory_Region_Array_T;
      Count   : in out Natural;
      Region  : Memory_Region_T;
      Result  : out Function_Result)
   is
      --  This array holds the memory regions that remain after removing the
      --  region.
      Remaining_Regions      : Memory_Region_Array_T;
      Remaining_Region_Count : Natural := 0;
   begin
      if Region.Size > Unsigned_64'Last - Region.Base then
         Log_Error ("Removed memory region exceeds the address space");
         Result := Invalid_Argument;
         return;
      end if;

      --  A split adds a region, so the remainder is built in a separate
      --  array rather than edited in place.
      for Memory_Region of Regions (1 .. Count) loop
         Create_New_Regions_With_Reserved_Regions_Removed
           (Remaining_Regions,
            Remaining_Region_Count,
            Memory_Region,
            Region,
            Result);
         if Is_Error (Result) then
            return;
         end if;
      end loop;

      Regions := Remaining_Regions;
      Count := Remaining_Region_Count;

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Constraint_Error;
         Result := Constraint_Exception;
   end Remove_Region_From_Memory_Regions;

   procedure Remove_Reserved_Memory_Regions_From_Memory_Regions
     (State : in out FDT_Parser_State_T; Result : out Function_Result)
   is
      --  The kernel image's physical address range. It starts with the boot
      --  section, which Boot.Free_Boot_Memory frees once it's no longer
      --  needed.
      Kernel_Start : constant Natural
      with Import, External_Name => "__boot_start_physical";

      Kernel_End : constant Natural
      with Import, External_Name => "__kernel_end_physical";

      Kernel_Start_Phys_Addr_As_U64 : constant Unsigned_64 :=
        Address_To_Unsigned_64 (Kernel_Start'Address);

      Kernel_End_Phys_Addr_As_U64 : constant Unsigned_64 :=
        Address_To_Unsigned_64 (Kernel_End'Address);
   begin
      --  Each pass removes one reserved region from every memory region, so
      --  later passes see the pieces that earlier ones left.
      for Reserved_Region of
        State.Reserved_Memory_Regions (1 .. State.Reserved_Memory_Region_Count)
      loop
         Remove_Region_From_Memory_Regions
           (State.Memory_Regions,
            State.Memory_Region_Count,
            Reserved_Region,
            Result);
         if Is_Error (Result) then
            return;
         end if;
      end loop;

      Size : constant Unsigned_64 :=
        Kernel_End_Phys_Addr_As_U64 - Kernel_Start_Phys_Addr_As_U64;

      pragma
        Debug
          (Debug_Devicetree,
           Log_Debug
             ("Removing kernel image from memory regions: Base = "
              & Kernel_Start_Phys_Addr_As_U64'Image
              & ", Size = "
              & Size'Image));

      --  Remove the kernel image from the available memory regions.
      Remove_Region_From_Memory_Regions
        (State.Memory_Regions,
         State.Memory_Region_Count,
         (Base => Kernel_Start_Phys_Addr_As_U64, Size => Size),
         Result);
      if Is_Error (Result) then
         return;
      end if;

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

      for I in 1 .. State.Memory_Region_Count loop
         --  Storage_Count can't hold sizes of 2**63 or more.
         if State.Memory_Regions (I).Size > Unsigned_64 (Storage_Count'Last)
         then
            Log_Error ("Memory region is too large");
            Result := Invalid_Argument;
            return;
         end if;

         Memory_Regions (Memory_Regions'First + I - 1) :=
           (Base =>
              Physical_Address_T
                (Unsigned_64_To_Address (State.Memory_Regions (I).Base)),
            Size => Storage_Count (State.Memory_Regions (I).Size));
      end loop;

      Memory_Region_Count := State.Memory_Region_Count;

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Constraint_Error;
         Memory_Region_Count := 0;
         Result := Constraint_Exception;
   end Copy_Parsed_Memory_Regions_To_Result;

   procedure Parse_Devicetree
     (DTB_Address         : Address;
      Memory_Regions      : out Physical_Memory_Region_Array_T;
      Memory_Region_Count : out Natural;
      Result              : out Function_Result)
   is
      Header : constant FDT_Header_T
      with Import, Address => DTB_Address, Alignment => 1;

      State : FDT_Parser_State_T;
   begin
      Memory_Region_Count := 0;

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
