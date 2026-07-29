package body Straylight.Graphics is
   procedure Fill_Framebuffer
     (Framebuffer_Address : System.Address;
      Framebuffer_Width   : Integer;
      Framebuffer_Height  : Integer;
      Colour              : Unsigned_32) is
   begin
      Pixel_Data :
        array (0 .. (Framebuffer_Width * Framebuffer_Height) - 1)
        of Unsigned_32
      with Import, Alignment => 1, Address => Framebuffer_Address;

      for I in Pixel_Data'Range loop
         Pixel_Data (I) := Colour;
      end loop;
   exception
      when Constraint_Error =>
         null;
   end Fill_Framebuffer;

   procedure Update_Framebuffer
     (Framebuffer_Address : System.Address;
      X, Y, Width, Height : Unsigned_64;
      Result              : out Function_Result) is
   begin
      Syscall_Result : constant Syscall_Result_T :=
        Do_Syscall
          (Syscall_Update_Framebuffer,
           Address_To_Unsigned_64 (Framebuffer_Address),
           X,
           Y,
           Width,
           Height);

      if Syscall_Result = Syscall_Result_Success then
         Result := Function_Result_Success;
      else
         Result := Function_Result_Failure;
      end if;
   end Update_Framebuffer;

end Straylight.Graphics;
