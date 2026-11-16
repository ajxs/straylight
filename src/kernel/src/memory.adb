package body Memory is
   use Function_Results;

   function Does_Region_Wrap (Region : Physical_Memory_Region_T) return Boolean
   is (To_Integer (Address (Region.Base))
       > Integer_Address'Last - Integer_Address (Region.Size));

   --  'Min and 'Max only work on scalar types, and System.Address, which
   --  Physical_Address_T is derived from, is a private type.
   function Minimum
     (Left, Right : Physical_Address_T) return Physical_Address_T
   is (if Left < Right then Left else Right);

   function Maximum
     (Left, Right : Physical_Address_T) return Physical_Address_T
   is (if Left > Right then Left else Right);

   procedure Append_Physical_Memory_Region_To_Array
     (Regions : in out Physical_Memory_Region_Array_T;
      Count   : in out Natural;
      Region  : Physical_Memory_Region_T;
      Result  : out Function_Result) is
   begin
      if Region.Size = 0 then
         Result := Success;
         return;
      end if;

      if Count >= Regions'Length then
         Result := Region_Array_Exhausted;
         return;
      end if;

      Regions (Regions'First + Count) := Region;
      Count := Count + 1;

      Result := Success;
   exception
      when Constraint_Error =>
         Result := Constraint_Exception;
   end Append_Physical_Memory_Region_To_Array;

   procedure Remove_Physical_Memory_Region_From_Array
     (Regions        : in out Physical_Memory_Region_Array_T;
      Count          : in out Natural;
      Removed_Region : Physical_Memory_Region_T;
      Result         : out Function_Result) is
   begin
      --  Removing an empty region would otherwise split the region that
      --  contains it in two.
      if Removed_Region.Size = 0 then
         Result := Success;
         return;
      end if;

      if Does_Region_Wrap (Removed_Region) then
         Result := Invalid_Argument;
         return;
      end if;

      --  A split adds an entry, so what remains is built in a separate array
      --  rather than edited in place. This is declared after 'begin', so that
      --  the handler below covers its elaboration.
      Remaining_Regions : Physical_Memory_Region_Array_T (Regions'Range);
      Remaining_Region_Count : Natural := 0;

      for Region of Regions (Regions'First .. Regions'First + Count - 1) loop
         if Does_Region_Wrap (Region) then
            Result := Invalid_Argument;
            return;
         end if;

         Region_End : constant Physical_Address_T := Region.Base + Region.Size;

         Removed_End : constant Physical_Address_T :=
           Removed_Region.Base + Removed_Region.Size;

         --  The removed region's ends, clamped to this region.
         Lower_End : constant Physical_Address_T :=
           Minimum (Maximum (Removed_Region.Base, Region.Base), Region_End);

         Upper_Base : constant Physical_Address_T :=
           Minimum (Maximum (Removed_End, Region.Base), Region_End);

         --  Keep the parts below and above the removed region. Either part
         --  can be empty, and Append_Physical_Memory_Region_To_Array skips
         --  empty regions.
         Append_Physical_Memory_Region_To_Array
           (Remaining_Regions,
            Remaining_Region_Count,
            (Base => Region.Base, Size => Lower_End - Region.Base),
            Result);
         if Is_Error (Result) then
            return;
         end if;

         Append_Physical_Memory_Region_To_Array
           (Remaining_Regions,
            Remaining_Region_Count,
            (Base => Upper_Base, Size => Region_End - Upper_Base),
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
         Result := Constraint_Exception;
   end Remove_Physical_Memory_Region_From_Array;

   function Get_Dword_Word_Low (Dword : Unsigned_64) return Unsigned_32 is
   begin
      return Unsigned_32 (Dword and 16#FFFF_FFFF#);
   exception
      when Constraint_Error =>
         return 0;
   end Get_Dword_Word_Low;

   function Get_Dword_Word_High (Dword : Unsigned_64) return Unsigned_32 is
   begin
      return Unsigned_32 (Shift_Right (Dword, 32));
   exception
      when Constraint_Error =>
         return 0;
   end Get_Dword_Word_High;

   function Is_Address_Range_Within_Region
     (Address_Range_Start  : Address;
      Address_Range_Length : Storage_Count;
      Region_Start         : Address;
      Region_Length        : Storage_Count) return Boolean is
   begin
      return
        Address_Range_Length <= Region_Length
        and then Address_Range_Start >= Region_Start
        and then
          Address_Range_Start - Region_Start
          <= Region_Length - Address_Range_Length;
   exception
      when Constraint_Error =>
         return False;
   end Is_Address_Range_Within_Region;

   function Align_Memory_Size_Up
     (Size : Alignable_Size_T; Boundary : Alignment_T) return Storage_Count
   is
      --  Without this pragma, GNAT warns that the two 'mod Boundary'
      --  operations below may raise Constraint_Error on a zero divisor.
      --  Boundary can't be zero because the constrained Alignment_T subtype
      --  starts at 1, and that range is checked at the call site.
      --  Because of this, the generated code is identical with or without
      --  the division check being suppressed. Overflow and range checks are
      --  unaffected.
      --  If Alignment_T ever included 0, this pragma would remove a real
      --  check, and a zero Boundary would give a wrong result.
      pragma Suppress (Division_Check);
   begin
      return Size + (Boundary - Size mod Boundary) mod Boundary;
   end Align_Memory_Size_Up;

end Memory;
