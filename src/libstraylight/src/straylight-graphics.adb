with System.Machine_Code;

package body Straylight.Graphics is
   procedure Update_Framebuffer
     (Framebuffer_Address : System.Address; Result : out Syscall_Result_T) is
   begin
      System.Machine_Code.Asm
        (Template =>
           "mv a0, %1"
           & ASCII.LF
           & "mv a1, %2"
           & ASCII.LF
           & "ecall"
           & ASCII.LF
           & "mv %0, a0",
         Outputs  => [Unsigned_64'Asm_Output ("=r", Result)],
         Inputs   =>
           [Unsigned_64'Asm_Input ("r", Syscall_Update_Framebuffer),
            Address'Asm_Input ("r", Framebuffer_Address)],
         Clobber  => "a0,a1",
         Volatile => True);
   end Update_Framebuffer;

   function Update_Framebuffer_C
     (Framebuffer_Address : System.Address) return Syscall_Result_T
   is
      Result : Syscall_Result_T;
   begin
      Update_Framebuffer (Framebuffer_Address, Result);
      return Result;
   end Update_Framebuffer_C;

end Straylight.Graphics;
