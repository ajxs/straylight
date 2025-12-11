package Straylight.Graphics is
   pragma Preelaborate;

   procedure Update_Framebuffer
     (Framebuffer_Address : System.Address; Result : out Syscall_Result_T);

   procedure Fill_Framebuffer
     (Framebuffer_Address : System.Address;
      Framebuffer_Width   : Integer;
      Framebuffer_Height  : Integer;
      Colour              : Unsigned_32)
   with
     Import,
     Convention    => Assembler,
     External_Name => "straylight_graphics_fill_framebuffer";

   function Make_Colour
     (Red   : Unsigned_8;
      Green : Unsigned_8;
      Blue  : Unsigned_8;
      Alpha : Unsigned_8) return Unsigned_32
   with
     Import,
     Convention    => Assembler,
     External_Name => "straylight_graphics_make_colour";

private
   function Update_Framebuffer_C
     (Framebuffer_Address : System.Address) return Syscall_Result_T
   with
     Export,
     Convention    => C,
     External_Name => "straylight_graphics_update_framebuffer";

end Straylight.Graphics;
