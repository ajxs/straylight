package body Straylight.Graphics is
   procedure Update_Framebuffer
     (Framebuffer_Address : System.Address; Result : out Function_Result) is
   begin
      Syscall_Result : constant Syscall_Result_T :=
        Do_Syscall
          (Syscall_Update_Framebuffer,
           Address_To_Unsigned_64 (Framebuffer_Address));

      if Syscall_Result = Syscall_Result_Success then
         Result := Function_Result_Success;
      else
         Result := Function_Result_Failure;
      end if;
   end Update_Framebuffer;

end Straylight.Graphics;
