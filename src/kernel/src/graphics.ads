with Interfaces; use Interfaces;

with Addresses; use Addresses;

package Graphics is
   pragma Preelaborate;

   type Colour_T is new Unsigned_32;

   function Make_Colour
     (Red   : Unsigned_8;
      Green : Unsigned_8;
      Blue  : Unsigned_8;
      Alpha : Unsigned_8 := 255) return Colour_T
   with
     Import,
     Convention    => Assembler,
     External_Name => "graphics_make_colour";

   procedure Fill_Framebuffer
     (Framebuffer_Base_Address : Virtual_Address_T;
      Framebuffer_Width        : Integer;
      Framebuffer_Height       : Integer;
      Colour                   : Colour_T)
   with
     Import,
     Convention    => Assembler,
     External_Name => "graphics_fill_framebuffer";

end Graphics;
