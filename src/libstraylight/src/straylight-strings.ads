package Straylight.Strings is
   pragma Preelaborate;

   Default_Maximum_String_Length : constant := 128;

   function Get_String_Length
     (String_Address : System.Address;
      Maximum_Length : Integer := Default_Maximum_String_Length)
      return Integer;
end Straylight.Strings;
