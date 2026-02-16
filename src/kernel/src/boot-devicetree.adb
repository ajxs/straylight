with Utilities; use Utilities;

package body Boot.Devicetree is
   procedure Parse_Devicetree
     (DTB_Address : Address; Result : out Function_Result)
   is
      Header : constant FDT_Header_T
      with Import, Address => DTB_Address, Alignment => 1;
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
         & Convert_BEU32_To_LEU32 (Header.Size_DT_Struct)'Image,
         Devicetree_Logging_Tags);

      if Convert_BEU32_To_LEU32 (Header.Magic) /= FDT_MAGIC_NUMBER then
         Log_Debug
           ("Invalid devicetree magic value.", Devicetree_Logging_Tags);
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

      Read_Reserved_Memory_Regions : declare
         Curr_Addr : Address := Reserved_Mem_Map_Address;
      begin

         Log_Debug ("Reserved Memory Regions", Devicetree_Logging_Tags);

         while True loop
            Read_Block : declare
               Block : Reserved_Memory_Block_T
               with Import, Address => Curr_Addr, Alignment => 1;
            begin
               exit when Block.Size = 0 and then Block.Addr = 0;

               Log_Debug
                 ("  Block Addr: "
                  & Convert_BEU64_To_LEU64 (Block.Addr)'Image
                  & ASCII.LF
                  & "  Block Size: "
                  & Convert_BEU64_To_LEU64 (Block.Size)'Image
                  & ASCII.LF
                  & "------------------------",
                  Devicetree_Logging_Tags);

            end Read_Block;

            Curr_Addr := Curr_Addr + 16;
         end loop;
      end Read_Reserved_Memory_Regions;

      Parse_Structure_Block
        (Structure_Block_Address, String_Table_Address, Result);
      if Is_Error (Result) then
         return;
      end if;

      Result := Success;
   exception
      when others =>
         Log_Error
           ("Constraint_Error: Parse_Devicetree", Devicetree_Logging_Tags);

         Result := Constraint_Exception;
   end Parse_Devicetree;

   procedure Parse_Property
     (Structure_Block_Address : Address;
      String_Table_Address    : Address;
      Property_Name           : out Devicetree_String_T;
      Property_Name_Length    : out Natural;
      Property_Length         : out Unsigned_32;
      Property_Address        : out Address;
      Curr_Offset             : in out Storage_Offset)
   is
      Property : FDT_Property_T
      with
        Import,
        Address   => Structure_Block_Address + Curr_Offset,
        Alignment => 1;
   begin
      Read_Property_Name_String
        (String_Table_Address,
         Storage_Offset (Convert_BEU32_To_LEU32 (Property.Name_Offset)),
         Property_Name,
         Property_Name_Length);

      Property_Length := Convert_BEU32_To_LEU32 (Property.Length);
      Property_Address := Structure_Block_Address + Curr_Offset + 8;

      Curr_Offset := Curr_Offset + 8 + Storage_Offset (Property_Length);
   exception
      when others =>
         Log_Error ("Constraint_Error: Parse_Property");
   end Parse_Property;

   procedure Parse_Structure_Block
     (Structure_Block_Address : Address;
      String_Table_Address    : Address;
      Result                  : out Function_Result)
   is
      Curr_Offset : Storage_Offset := 0;

      Node_Name        : Devicetree_String_T;
      Node_Name_Length : Natural := 0;

      Property_Name        : Devicetree_String_T;
      Property_Name_Length : Natural := 0;
      Property_Length      : Unsigned_32;
      Property_Address     : Address;

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
      Current_Cells_Context : FDT_Cells_Context_T :=
        (Address_Cells => 2, Size_Cells => 1);

   begin
      while True loop
         Read_Structure : declare
            Curr_Token : FDT_Token_T
            with
              Import,
              Address   => Structure_Block_Address + Curr_Offset,
              Alignment => 1;

            Token_Value : constant Unsigned_32 :=
              Convert_BEU32_To_LEU32 (Curr_Token);
         begin
            if Token_Value = FDT_BEGIN_NODE then
               Log_Debug ("START STRUCTURE", Devicetree_Logging_Tags);
               Curr_Offset := Curr_Offset + 4;
               Node_Name_Length := 0;

               Push_Cells_Context
                 (Cells_Context_Stack,
                  Cells_Context_Stack_Ptr,
                  Current_Cells_Context,
                  Result);
               if Is_Error (Result) then
                  return;
               end if;

               Read_Name : declare
                  Name_String : String (1 .. Maximum_String_Length)
                  with
                    Import,
                    Address   => Structure_Block_Address + Curr_Offset,
                    Alignment => 1;
               begin
                  for I in Name_String'Range loop
                     exit when Name_String (I) = ASCII.NUL;
                     Node_Name (I) := Name_String (I);
                     Node_Name_Length := I;
                  end loop;

                  Curr_Offset :=
                    Curr_Offset + Storage_Offset (Node_Name_Length) + 1;
               end Read_Name;

               while Curr_Offset mod 4 /= 0 loop
                  Curr_Offset := Curr_Offset + 1;
               end loop;

               Log_Debug
                 ("  Node Name: '" & Node_Name (1 .. Node_Name_Length) & "'",
                  Devicetree_Logging_Tags);

            elsif Token_Value = FDT_END_NODE then
               Log_Debug
                 ("Node cells context: ("
                  & "Address_Cells: "
                  & Current_Cells_Context.Address_Cells'Image
                  & ", Size_Cells: "
                  & Current_Cells_Context.Size_Cells'Image
                  & ")",
                  Devicetree_Logging_Tags);

               Pop_Cells_Context
                 (Cells_Context_Stack,
                  Cells_Context_Stack_Ptr,
                  Current_Cells_Context,
                  Result);
               if Is_Error (Result) then
                  return;
               end if;

               Log_Debug ("END STRUCTURE" & ASCII.LF, Devicetree_Logging_Tags);

               Curr_Offset := Curr_Offset + 4;
            elsif Token_Value = FDT_PROP then
               Curr_Offset := Curr_Offset + 4;

               Parse_Property
                 (Structure_Block_Address,
                  String_Table_Address,
                  Property_Name,
                  Property_Name_Length,
                  Property_Length,
                  Property_Address,
                  Curr_Offset);

               Log_Debug ("Property:", Devicetree_Logging_Tags);

               Log_Debug
                 ("  Name: '"
                  & Property_Name (1 .. Property_Name_Length)
                  & "'",
                  Devicetree_Logging_Tags);

               if Property_Length = 0 then
                  Log_Debug ("  (No Value)", Devicetree_Logging_Tags);
               elsif Is_String_Value (Property_Name, Property_Name_Length) then
                  Read_Property_Value : declare
                     Prop_Value : String (1 .. Integer (Property_Length))
                     with Import, Address => Property_Address, Alignment => 1;
                  begin
                     Log_Debug
                       ("  Value: '" & Prop_Value & "'",
                        Devicetree_Logging_Tags);
                  end Read_Property_Value;
               end if;

               if Compare_Property_Name
                    (Property_Name, Property_Name_Length, "#size-cells")
                 or else
                   Compare_Property_Name
                     (Property_Name, Property_Name_Length, "#address-cells")
               then
                  Read_Cells_Property_Value : declare
                     Prop_Value : Unsigned_32
                     with Import, Address => Property_Address, Alignment => 1;
                  begin

                     Cells_Value : constant Unsigned_32 :=
                       Convert_BEU32_To_LEU32 (Prop_Value);

                     if Compare_Property_Name
                          (Property_Name, Property_Name_Length, "#size-cells")
                     then
                        Current_Cells_Context.Size_Cells := Cells_Value;
                     elsif Compare_Property_Name
                             (Property_Name,
                              Property_Name_Length,
                              "#address-cells")
                     then
                        Current_Cells_Context.Address_Cells := Cells_Value;
                     end if;

                  end Read_Cells_Property_Value;
               end if;

               while Curr_Offset mod 4 /= 0 loop
                  Curr_Offset := Curr_Offset + 1;
               end loop;
            elsif Token_Value = FDT_NOP then
               Log_Debug ("NOP", Devicetree_Logging_Tags);
               Curr_Offset := Curr_Offset + 4;
            elsif Token_Value = FDT_END then
               Log_Debug ("END", Devicetree_Logging_Tags);
               exit;
            else
               Log_Debug ("ABORT", Devicetree_Logging_Tags);
               exit;
            end if;

         end Read_Structure;
      end loop;

      Result := Success;
   exception
      when others =>
         Log_Error ("Constraint_Error: Parse_Structure_Block");
         Result := Constraint_Exception;
   end Parse_Structure_Block;

   procedure Read_Property_Name_String
     (String_Table_Address : Address;
      String_Table_Offset  : Storage_Offset;
      Property_Name        : out Devicetree_String_T;
      Property_Name_Length : out Natural)
   is
      S : array (0 .. Maximum_String_Length - 1) of Character
      with
        Import,
        Convention => C,
        Address    => String_Table_Address + String_Table_Offset,
        Alignment  => 1;
   begin
      Property_Name_Length := 0;

      for I in S'Range loop
         exit when S (I) = ASCII.NUL;
         Property_Name (I + 1) := S (I);
         Property_Name_Length := I + 1;
      end loop;

   end Read_Property_Name_String;

   function Compare_Property_Name
     (Property_Name        : Devicetree_String_T;
      Property_Name_Length : Natural;
      Target_Name          : String) return Boolean is
   begin
      if Property_Name_Length /= Target_Name'Length then
         return False;
      end if;

      for I in Target_Name'Range loop
         if Property_Name (I) /= Target_Name (I) then
            return False;
         end if;
      end loop;

      return True;
   exception
      when others =>
         Log_Error ("Constraint_Error: Compare_Property_Name");
         return False;
   end Compare_Property_Name;

   function Is_String_Value
     (Property_Name : Devicetree_String_T; Property_Name_Length : Natural)
      return Boolean is
   begin
      --  For simplicity, assume properties with names ending in
      --  "compatible", "model", or "device_type" are string values.
      if Compare_Property_Name
           (Property_Name, Property_Name_Length, "compatible")
        or else
          Compare_Property_Name (Property_Name, Property_Name_Length, "model")
        or else
          Compare_Property_Name
            (Property_Name, Property_Name_Length, "device_type")
      then
         return True;
      end if;

      return False;
   end Is_String_Value;

   function Get_Devicetree_Size (DTB_Address : Address) return Storage_Offset
   is
      Header : FDT_Header_T
      with Import, Address => DTB_Address, Alignment => 1;
   begin
      return Storage_Offset (Header.Totalsize);
   end Get_Devicetree_Size;

   procedure Push_Cells_Context
     (Context_Stack     : in out FDT_Cells_Context_Stack_T;
      Context_Stack_Ptr : in out Natural;
      New_Context       : FDT_Cells_Context_T;
      Result            : out Function_Result) is
   begin
      if Context_Stack_Ptr >= Context_Stack'Last then
         Log_Error
           ("Cells context stack overflow: " & Context_Stack_Ptr'Image);
         Result := Constraint_Exception;
         return;
      end if;

      Log_Debug ("Pushing cells context", Devicetree_Logging_Tags);

      Context_Stack (Context_Stack_Ptr) := New_Context;
      Context_Stack_Ptr := Context_Stack_Ptr + 1;
      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Push_Cells_Context");
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

      Log_Debug ("Popping cells context", Devicetree_Logging_Tags);

      Context_Stack_Ptr := Context_Stack_Ptr - 1;
      Popped_Context := Context_Stack (Context_Stack_Ptr);
      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Pop_Cells_Context");
         Result := Constraint_Exception;
   end Pop_Cells_Context;

   function Compare_Node_Name
     (Node_Name        : Devicetree_String_T;
      Node_Name_Length : Natural;
      Target_Name      : String) return Boolean is
   begin
      if Target_Name'Length > Node_Name_Length then
         return False;
      end if;

      for I in Target_Name'Range loop
         if Node_Name (I) /= Target_Name (I) then
            return False;
         elsif Node_Name (I) = ASCII.At_Sign then
            --  If we've reached the '@' character in the node name, without
            --  finding any mismatches, consider it a match.
            return True;
         end if;
      end loop;

      return True;
   exception
      when others =>
         Log_Error ("Constraint_Error: Compare_Node_Name");
         return False;
   end Compare_Node_Name;

end Boot.Devicetree;
