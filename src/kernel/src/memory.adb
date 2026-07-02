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

end Memory;
