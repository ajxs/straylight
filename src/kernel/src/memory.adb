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

end Memory;
