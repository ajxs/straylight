with Interfaces; use Interfaces;

with Addresses;        use Addresses;
with Function_Results; use Function_Results;
with Logging;          use Logging;

package Graphics is
   pragma Preelaborate;

   subtype Colour_T is Unsigned_32;

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

   type Bitmap_Pixel_Format_T is
     (Bitmap_Pixel_Format_RGB, Bitmap_Pixel_Format_RGBA);

   procedure Transfer_Image_Data
     (Source_Pixel_Data_Addr : Virtual_Address_T;
      Source_Buffer_Width    : Natural;
      Source_Buffer_Height   : Natural;
      Dest_Pixel_Data_Addr   : Virtual_Address_T;
      Dest_Buffer_Width      : Natural;
      Dest_Buffer_Height     : Natural;
      Destination_X          : Natural;
      Destination_Y          : Natural;
      Transparency_Colour    : Colour_T;
      Result                 : out Function_Result);

   procedure Parse_Bitmap
     (Bitmap_Data_Buffer_Address : Virtual_Address_T;
      Pixel_Data_Address         : out Virtual_Address_T;
      Pixel_Data_Length          : out Natural;
      Pixel_Data_Format          : out Bitmap_Pixel_Format_T;
      Width                      : out Integer;
      Height                     : out Integer;
      Result                     : out Function_Result);

   procedure Read_Bitmap_Pixel_Data_Into_Buffer
     (Destination_Buffer_Addr   : Virtual_Address_T;
      Bitmap_Pixel_Data_Address : Virtual_Address_T;
      Bitmap_Pixel_Data_Length  : Natural;
      Bitmap_Pixel_Data_Format  : Bitmap_Pixel_Format_T;
      Bitmap_Width              : Integer;
      Bitmap_Height             : Integer;
      Result                    : out Function_Result);

private
   Logging_Tags : constant Log_Tags := [Log_Tag_Graphics];

   type Bitmap_Header_T is record
      BfType      : Unsigned_16;
      BfSize      : Unsigned_32;
      BfReserved1 : Unsigned_16;
      BfReserved2 : Unsigned_16;
      BfOffBits   : Unsigned_32;
   end record
   with Convention => C, Pack;

   --  Matches the BITMAPINFOHEADER specification.
   type Bitmap_DIB_Header_T is record
      BiSize          : Unsigned_32;
      BiWidth         : Integer;
      BiHeight        : Integer;
      BiPlanes        : Unsigned_16;
      BiBitCount      : Unsigned_16;
      BiCompression   : Unsigned_32;
      BiSizeImage     : Unsigned_32;
      BiXPelsPerMeter : Integer;
      BiYPelsPerMeter : Integer;
      BiClrUsed       : Unsigned_32;
      BiClrImportant  : Unsigned_32;
   end record
   with Convention => C, Pack;
end Graphics;
