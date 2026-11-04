with Logging; use Logging;
with Memory;  use Memory;

package body Devicetree is
   --  The current address and size cells context is captured in a stack, so
   --  that we can properly handle nested nodes that override the
   --  #address-cells and #size-cells properties.
   --  Procedures are contained in this package for pushing and popping the
   --  context as the devicetree is traversed.
   type FDT_Cells_Context_T is record
      Address_Cells : Unsigned_32;
      Size_Cells    : Unsigned_32;
   end record;

   type FDT_Cells_Context_Stack_T is
     array (Natural range <>) of FDT_Cells_Context_T;

   procedure Push_Cells_Context
     (Context_Stack     : in out FDT_Cells_Context_Stack_T;
      Context_Stack_Ptr : in out Natural;
      New_Context       : FDT_Cells_Context_T;
      Result            : out Function_Result) is
   begin
      if Context_Stack_Ptr > Context_Stack'Last then
         Log_Error
           ("Cells context stack overflow: " & Context_Stack_Ptr'Image);
         Result := Constraint_Exception;
         return;
      end if;

      pragma Debug (Debug_Devicetree, Log_Debug ("Pushing cells context"));

      Context_Stack (Context_Stack_Ptr) := New_Context;
      Context_Stack_Ptr := Context_Stack_Ptr + 1;
      Result := Success;
   exception
      when Constraint_Error =>
         Log_Constraint_Error;
         Result := Constraint_Exception;
   end Push_Cells_Context;

   procedure Pop_Cells_Context
     (Context_Stack     : FDT_Cells_Context_Stack_T;
      Context_Stack_Ptr : in out Natural;
      Popped_Context    : out FDT_Cells_Context_T;
      Result            : out Function_Result) is
   begin
      if Context_Stack_Ptr <= Context_Stack'First then
         Log_Error ("Cells context stack underflow");
         Result := Constraint_Exception;
         return;
      end if;

      pragma Debug (Debug_Devicetree, Log_Debug ("Popping cells context"));

      Context_Stack_Ptr := Context_Stack_Ptr - 1;
      Popped_Context := Context_Stack (Context_Stack_Ptr);
      Result := Success;
   exception
      when Constraint_Error =>
         Log_Constraint_Error;
         Result := Constraint_Exception;
   end Pop_Cells_Context;

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

   procedure Parse_Reserved_Memory_Regions
     (Reserved_Mem_Map_Address : Address;
      Total_Size               : Storage_Count;
      Result                   : out Function_Result)
   is
      Curr_Offset : Storage_Offset := 0;
   begin
      pragma
        Debug
          (Debug_Devicetree, Log_Debug ("Parsing Reserved Memory Regions..."));

      loop
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

            pragma
              Debug
                (Debug_Devicetree,
                 Log_Debug
                   ("Addr="
                    & Convert_BEU64_To_LEU64 (Block.Addr)'Image
                    & ", Size= "
                    & Convert_BEU64_To_LEU64 (Block.Size)'Image));

         end Read_Block;

         --  Advance by the size of the reserved memory block structure.
         Curr_Offset := Curr_Offset + 16;
         if Curr_Offset >= Total_Size then
            Log_Error ("Reserved memory map exceeds total devicetree size.");
            Result := Unhandled_Exception;
            return;
         end if;
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

   procedure Parse_Devicetree
     (DTB_Address : Address; Result : out Function_Result)
   is
      Header : constant FDT_Header_T
      with Import, Address => DTB_Address, Alignment => 1;
   begin
      pragma Debug (Debug_Devicetree, Print_Devicetree_Header (Header));

      if Convert_BEU32_To_LEU32 (Header.Magic) /= FDT_MAGIC_NUMBER then
         Log_Error ("Invalid devicetree magic value.");
         Result := Unhandled_Exception;
         return;
      end if;

      String_Table_Address : constant Virtual_Address_T :=
        DTB_Address
        + Storage_Offset (Convert_BEU32_To_LEU32 (Header.Off_DT_Strings));

      Reserved_Mem_Map_Address : constant Virtual_Address_T :=
        DTB_Address
        + Storage_Offset (Convert_BEU32_To_LEU32 (Header.Off_Mem_Rsvmap));

      Structure_Block_Address : constant Virtual_Address_T :=
        DTB_Address
        + Storage_Offset (Convert_BEU32_To_LEU32 (Header.Off_DT_Struct));

      Parse_Reserved_Memory_Regions
        (Reserved_Mem_Map_Address,
         Storage_Count (Convert_BEU32_To_LEU32 (Header.Totalsize)),
         Result);
      if Is_Error (Result) then
         return;
      end if;

      Parse_Structure_Block
        (Structure_Block_Address,
         Storage_Count (Convert_BEU32_To_LEU32 (Header.Size_DT_Struct)),
         String_Table_Address,
         Storage_Count (Convert_BEU32_To_LEU32 (Header.Size_DT_Strings)),
         Result);
      if Is_Error (Result) then
         return;
      end if;

      Result := Success;
   end Parse_Devicetree;

   procedure Parse_Property
     (Structure_Block_Address : Address;
      Structure_Block_Size    : Storage_Count;
      String_Table_Address    : Address;
      String_Table_Size       : Storage_Count;
      Property_Name           : out Devicetree_String_T;
      Property_Length         : out Unsigned_32;
      Property_Address        : out Address;
      Curr_Offset             : in out Storage_Offset;
      Result                  : out Function_Result)
   is
      Property : FDT_Property_T
      with
        Import,
        Address   => Structure_Block_Address + Curr_Offset,
        Alignment => 1;
   begin
      Property_Length := 0;
      Property_Address := Null_Address;

      Read_Property_Name_String
        (String_Table_Address,
         String_Table_Size,
         Storage_Offset (Convert_BEU32_To_LEU32 (Property.Name_Offset)),
         Property_Name,
         Result);
      if Is_Error (Result) then
         return;
      end if;

      Property_Length := Convert_BEU32_To_LEU32 (Property.Length);
      Property_Address := Structure_Block_Address + Curr_Offset + 8;

      --  Advance the read offset by the size of the property structure
      --  (8 bytes) plus the length of the property value.
      Curr_Offset := Curr_Offset + 8 + Storage_Offset (Property_Length);
      if Curr_Offset >= Structure_Block_Size then
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
     (Structure_Block_Address : Address;
      Structure_Block_Size    : Storage_Count;
      Curr_Offset             : in out Storage_Offset;
      Token_Value             : out Unsigned_32;
      Result                  : out Function_Result)
   is
      Curr_Token : constant FDT_Token_T
      with
        Import,
        Address   => Structure_Block_Address + Curr_Offset,
        Alignment => 1;
   begin
      if Curr_Offset >= Structure_Block_Size then
         Log_Error ("End of structure block reached while reading token");
         Result := Unhandled_Exception;
         Token_Value := 0;
         return;
      end if;

      Token_Value := Convert_BEU32_To_LEU32 (Curr_Token);

      --  Advance the read offset by the size of the token (4 bytes).
      Curr_Offset := Curr_Offset + 4;
      Result := Success;
   exception
      when Constraint_Error =>
         Log_Constraint_Error;
         Result := Constraint_Exception;
   end Get_Next_Token;

   procedure Parse_Structure_Block
     (Structure_Block_Address : Address;
      Structure_Block_Size    : Storage_Count;
      String_Table_Address    : Address;
      String_Table_Size       : Storage_Count;
      Result                  : out Function_Result)
   is
      Curr_Offset : Storage_Offset := 0;

      Node_Name : Devicetree_String_T;

      Property_Name    : Devicetree_String_T;
      Property_Length  : Unsigned_32;
      Property_Address : Address;

      FDT_BEGIN_NODE : constant := 1;
      FDT_END_NODE   : constant := 2;
      FDT_PROP       : constant := 3;
      FDT_NOP        : constant := 4;
      FDT_END        : constant := 9;

      Cells_Context_Stack     : FDT_Cells_Context_Stack_T (1 .. 16);
      Cells_Context_Stack_Ptr : Natural := Cells_Context_Stack'First;

      --  Devicetree specification v0.4 section 2.3.5 states:
      --  'If missing, a client program should assume a default value of 2 for
      --  #address-cells, and a value of 1 for #size-cells.'
      --  This is the context the current node declares for its children.
      --  The node's own 'reg' uses its parent's context, which is on top
      --  of Cells_Context_Stack.
      Current_Cells_Context : FDT_Cells_Context_T :=
        (Address_Cells => 2, Size_Cells => 1);

      Token_Value : Unsigned_32 := 0;
   begin
      loop
         Get_Next_Token
           (Structure_Block_Address,
            Structure_Block_Size,
            Curr_Offset,
            Token_Value,
            Result);
         if Is_Error (Result) then
            return;
         end if;

         if Token_Value = FDT_BEGIN_NODE then
            pragma Debug (Debug_Devicetree, Log_Debug ("START STRUCTURE"));
            Node_Name.Byte_Length := 0;

            Push_Cells_Context
              (Cells_Context_Stack,
               Cells_Context_Stack_Ptr,
               Current_Cells_Context,
               Result);
            if Is_Error (Result) then
               return;
            end if;

            Read_Node_Name_String
              (Structure_Block_Address,
               Structure_Block_Size,
               Curr_Offset,
               Node_Name,
               Result);
            if Is_Error (Result) then
               return;
            end if;

            --  Align offset to the next word boundary.
            while Curr_Offset mod 4 /= 0 loop
               Curr_Offset := Curr_Offset + 1;
            end loop;

            pragma
              Debug
                (Debug_Devicetree,
                 Log_Debug
                   ("  Node Name: '"
                    & Node_Name.Value (1 .. Node_Name.Byte_Length)
                    & "'"));

         elsif Token_Value = FDT_END_NODE then
            pragma
              Debug
                (Debug_Devicetree,
                 Log_Debug
                   ("Node cells context: ("
                    & "Address_Cells: "
                    & Current_Cells_Context.Address_Cells'Image
                    & ", Size_Cells: "
                    & Current_Cells_Context.Size_Cells'Image
                    & ")"));

            Pop_Cells_Context
              (Cells_Context_Stack,
               Cells_Context_Stack_Ptr,
               Current_Cells_Context,
               Result);
            if Is_Error (Result) then
               return;
            end if;

            pragma Debug (Debug_Devicetree, Log_Debug ("END STRUCTURE"));
         elsif Token_Value = FDT_PROP then
            Parse_Property
              (Structure_Block_Address,
               Structure_Block_Size,
               String_Table_Address,
               String_Table_Size,
               Property_Name,
               Property_Length,
               Property_Address,
               Curr_Offset,
               Result);
            if Is_Error (Result) then
               return;
            end if;

            pragma Debug (Debug_Devicetree, Log_Debug ("Property:"));

            pragma
              Debug
                (Debug_Devicetree,
                 Log_Debug
                   ("  Name: '"
                    & Property_Name.Value (1 .. Property_Name.Byte_Length)
                    & "'"));

            if Property_Length = 0 then
               pragma Debug (Debug_Devicetree, Log_Debug ("  (No Value)"));
            elsif Is_String_Value (Property_Name) then
               Read_Property_Value : declare
                  Prop_Value : String (1 .. Integer (Property_Length))
                  with Import, Address => Property_Address, Alignment => 1;
               begin
                  pragma
                    Debug
                      (Debug_Devicetree,
                       Log_Debug ("  Value: '" & Prop_Value & "'"));
               end Read_Property_Value;
            end if;

            if Compare_Fixed_Length_String_With_String
                 (Property_Name, "#size-cells")
              or else
                Compare_Fixed_Length_String_With_String
                  (Property_Name, "#address-cells")
            then
               Read_Cells_Property_Value : declare
                  Prop_Value : constant Unsigned_32
                  with Import, Address => Property_Address, Alignment => 1;
               begin
                  if Property_Length /= 4 then
                     Log_Error ("Invalid cells property length");
                     Result := Invalid_Argument;
                     return;
                  end if;

                  Cells_Value : constant Unsigned_32 :=
                    Convert_BEU32_To_LEU32 (Prop_Value);

                  if Compare_Fixed_Length_String_With_String
                       (Property_Name, "#size-cells")
                  then
                     Current_Cells_Context.Size_Cells := Cells_Value;
                  elsif Compare_Fixed_Length_String_With_String
                          (Property_Name, "#address-cells")
                  then
                     Current_Cells_Context.Address_Cells := Cells_Value;
                  end if;
               end Read_Cells_Property_Value;

            elsif Compare_Fixed_Length_String_With_String
                    (Property_Name, "reg")
            then
               if Cells_Context_Stack_Ptr <= Cells_Context_Stack'First then
                  Log_Error ("No parent cells context for 'reg' property");
                  Result := Invalid_Argument;
                  return;
               end if;

               Read_Reg_Property : declare
                  --  A node's 'reg' uses its parent's cells context, which
                  --  was pushed on entry to this node.
                  Parent_Cells_Context : constant FDT_Cells_Context_T :=
                    Cells_Context_Stack (Cells_Context_Stack_Ptr - 1);

                  Reg_Base : Unsigned_64 := 0;
                  Reg_Size : Unsigned_64 := 0;

                  Total_Reg_Entry_Count : Natural := 0;

                  --  If the parent node's address-cells or size-cells exceed
                  --  2, we cannot parse the 'reg' property into 64-bit values.
                  --  This might occur in PCI device nodes.
                  Cells_Context_Is_Parseable : constant Boolean :=
                    Parent_Cells_Context.Address_Cells <= 2
                    and then Parent_Cells_Context.Size_Cells <= 2;
               begin
                  if Cells_Context_Is_Parseable then
                     Get_Reg_Entry_Count
                       (Property_Length,
                        Parent_Cells_Context,
                        Total_Reg_Entry_Count,
                        Result);
                     if Is_Error (Result) then
                        return;
                     end if;

                     for Entry_Index in 0 .. Total_Reg_Entry_Count - 1 loop
                        Read_Reg_Entry
                          (Property_Address,
                           Property_Length,
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
                     end loop;
                  else
                     Log_Error
                       ("Skipping 'reg' with unsupported cell sizes in node '"
                        & Node_Name.Value (1 .. Node_Name.Byte_Length)
                        & "'");
                  end if;
               end Read_Reg_Property;
            end if;

            --  Align offset to the next word boundary.
            while Curr_Offset mod 4 /= 0 loop
               Curr_Offset := Curr_Offset + 1;
            end loop;
         elsif Token_Value = FDT_NOP then
            pragma Debug (Debug_Devicetree, Log_Debug ("NOP"));
         elsif Token_Value = FDT_END then
            pragma Debug (Debug_Devicetree, Log_Debug ("END"));
            exit;
         else
            Log_Error ("Unknown token type: " & Token_Value'Image);
            Result := Invalid_Argument;
            return;
         end if;
      end loop;

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Constraint_Error;
         Result := Constraint_Exception;
   end Parse_Structure_Block;

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
