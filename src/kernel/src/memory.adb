package body Memory is
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
