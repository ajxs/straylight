package body Straylight.Strings is
   function Get_String_Length (String_Address : System.Address) return Integer
   is
      External_String : String (1 .. 256)
      with Import, Alignment => 1, Address => String_Address;

      Current_Length : Integer := 0;
   begin
      for Index in External_String'Range loop
         if External_String (Index) = ASCII.NUL then
            return Current_Length;
         end if;

         Current_Length := Current_Length + 1;
      end loop;

      return Current_Length;
   exception
      when Constraint_Error =>
         return 0;
   end Get_String_Length;
end Straylight.Strings;
