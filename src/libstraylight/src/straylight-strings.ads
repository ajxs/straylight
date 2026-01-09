package Straylight.Strings is
   pragma Preelaborate;

   function Get_String_Length (String_Address : System.Address) return Integer
   with Import, Convention => C, External_Name => "strlen";
end Straylight.Strings;
