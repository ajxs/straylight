with System;                  use System;
with System.Storage_Elements; use System.Storage_Elements;

package body Graphics is
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
      Result                 : out Function_Result)
   is
      Rows_To_Copy    : Integer := 0;
      Columns_To_Copy : Integer := 0;
   begin
      if Source_Buffer_Width > Dest_Buffer_Width
        or else Source_Buffer_Height > Dest_Buffer_Height
      then
         Log_Error ("Source image exceeds destination image dimensions");
         Result := Invalid_Argument;
         return;
      end if;

      if Destination_X >= Dest_Buffer_Width
        or else Destination_Y >= Dest_Buffer_Height
      then
         Log_Error
           ("Source image position exceeds destination image dimensions");
         Result := Invalid_Argument;
         return;
      end if;

      --  Truncate the data to be copied if it exceeds the destination
      --  buffer boundaries.
      Columns_To_Copy := Source_Buffer_Width;
      if (Destination_X + Source_Buffer_Width) > Dest_Buffer_Width then
         Columns_To_Copy := Dest_Buffer_Width - Destination_X;
      end if;

      Rows_To_Copy := Source_Buffer_Height;
      if (Destination_Y + Source_Buffer_Height) > Dest_Buffer_Height then
         Rows_To_Copy := Dest_Buffer_Height - Destination_Y;
      end if;

      Copy_Pixel_Data :
      declare
         Source_Pixel_Data :
           array (0 .. (Source_Buffer_Width * Source_Buffer_Height) - 1)
           of Colour_T
         with Import, Alignment => 1, Address => Source_Pixel_Data_Addr;

         Dest_Pixel_Data :
           array (0 .. (Dest_Buffer_Width * Dest_Buffer_Height) - 1)
           of Colour_T
         with Import, Alignment => 1, Address => Dest_Pixel_Data_Addr;

         Source_Pixel_Colour : Colour_T;
         Dest_Buffer_Index   : Natural := 0;
      begin
         for Y_Index in 0 .. Rows_To_Copy - 1 loop
            for X_Index in 0 .. Columns_To_Copy - 1 loop
               Dest_Buffer_Index :=
                 ((Destination_Y + Y_Index) * Dest_Buffer_Width)
                 + (Destination_X + X_Index);

               Source_Pixel_Colour :=
                 Source_Pixel_Data ((Y_Index * Source_Buffer_Width) + X_Index);

               if Transparency_Colour /= 0
                 and then Source_Pixel_Colour /= Transparency_Colour
               then
                  Dest_Pixel_Data (Dest_Buffer_Index) := Source_Pixel_Colour;
               end if;

            end loop;
         end loop;
      end Copy_Pixel_Data;

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Transfer_Image_Data");
         Result := Constraint_Exception;
   end Transfer_Image_Data;

   procedure Read_Bitmap_Pixel_Data_Into_Buffer
     (Destination_Buffer_Addr   : Virtual_Address_T;
      Bitmap_Pixel_Data_Address : Virtual_Address_T;
      Bitmap_Pixel_Data_Length  : Natural;
      Bitmap_Pixel_Data_Format  : Bitmap_Pixel_Format_T;
      Bitmap_Width              : Integer;
      Bitmap_Height             : Integer;
      Result                    : out Function_Result)
   is
      Bitmap_Pixel_Data : array (0 .. Bitmap_Pixel_Data_Length) of Unsigned_8
      with Import, Alignment => 1, Address => Bitmap_Pixel_Data_Address;

      Bitmap_Data_Offset : Natural := 0;
      Pixel_Data_Offset  : Natural := 0;
      Pixels_Read        : Natural := 0;

      X_Index : Integer := 0;
      Y_Index : Integer := 0;

      Colour : Colour_T;
   begin
      --  @TODO: Take into account negative image height.
      declare
         Pixel_Data_Buffer :
           array (0 .. Bitmap_Width * Bitmap_Height - 1) of Colour_T
         with Import, Alignment => 1, Address => Destination_Buffer_Addr;
      begin
         loop
            Y_Index := Bitmap_Height - (Pixels_Read / Bitmap_Width) - 1;
            X_Index := Pixels_Read mod Bitmap_Width;

            Pixel_Data_Offset := Y_Index * Bitmap_Width + X_Index;

            case Bitmap_Pixel_Data_Format is
               when Bitmap_Pixel_Format_RGB  =>
                  Colour :=
                    Make_Colour
                      (Red   => Bitmap_Pixel_Data (Bitmap_Data_Offset + 2),
                       Green => Bitmap_Pixel_Data (Bitmap_Data_Offset + 1),
                       Blue  => Bitmap_Pixel_Data (Bitmap_Data_Offset + 0),
                       Alpha => 255);

                  Bitmap_Data_Offset := Bitmap_Data_Offset + 3;

               when Bitmap_Pixel_Format_RGBA =>
                  Colour :=
                    Make_Colour
                      (Red   => Bitmap_Pixel_Data (Bitmap_Data_Offset + 2),
                       Green => Bitmap_Pixel_Data (Bitmap_Data_Offset + 1),
                       Blue  => Bitmap_Pixel_Data (Bitmap_Data_Offset + 0),
                       Alpha => Bitmap_Pixel_Data (Bitmap_Data_Offset + 3));

                  Bitmap_Data_Offset := Bitmap_Data_Offset + 4;
            end case;

            Pixel_Data_Buffer (Pixel_Data_Offset) := Colour;
            Pixels_Read := Pixels_Read + 1;

            exit when Pixels_Read = (Bitmap_Width * Bitmap_Height);
         end loop;
      end;

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Read_Bitmap_Pixel_Data_Into_Buffer");
         Result := Constraint_Exception;
   end Read_Bitmap_Pixel_Data_Into_Buffer;

   procedure Parse_Bitmap
     (Bitmap_Data_Buffer_Address : Virtual_Address_T;
      Pixel_Data_Address         : out Virtual_Address_T;
      Pixel_Data_Length          : out Natural;
      Pixel_Data_Format          : out Bitmap_Pixel_Format_T;
      Width                      : out Integer;
      Height                     : out Integer;
      Result                     : out Function_Result)
   is
      Bitmap_Header : constant Bitmap_Header_T
      with Import, Alignment => 1, Address => Bitmap_Data_Buffer_Address;

      DIB_Header : constant Bitmap_DIB_Header_T
      with
        Import,
        Alignment => 1,
        Address   => Bitmap_Data_Buffer_Address + Storage_Offset (14);
   begin
      Pixel_Data_Length := 0;
      Pixel_Data_Address := Null_Address;
      Width := 0;
      Height := 0;
      Pixel_Data_Format := Bitmap_Pixel_Format_RGB;

      if Bitmap_Header.BfType /= 16#4D42# then
         Result := Not_Supported;
         return;
      end if;

      Log_Debug
        ("Bitmap: "
         & ASCII.LF
         & "  BfType:    "
         & Bitmap_Header.BfType'Image
         & ASCII.LF
         & "  BfSize:    "
         & Bitmap_Header.BfSize'Image
         & ASCII.LF
         & "  BfOffBits: "
         & Bitmap_Header.BfOffBits'Image,
         Logging_Tags);

      Pixel_Data_Address :=
        Bitmap_Data_Buffer_Address + Storage_Offset (Bitmap_Header.BfOffBits);

      Pixel_Data_Length :=
        Positive (Bitmap_Header.BfSize - Bitmap_Header.BfOffBits);
      Pixel_Data_Format := Bitmap_Pixel_Format_RGB;

      Log_Debug
        ("DIB Header: "
         & ASCII.LF
         & "  BiSize:        "
         & DIB_Header.BiSize'Image
         & ASCII.LF
         & "  BiWidth:       "
         & DIB_Header.BiWidth'Image
         & ASCII.LF
         & "  BiHeight:      "
         & DIB_Header.BiHeight'Image
         & ASCII.LF
         & "  BiPlanes:      "
         & DIB_Header.BiPlanes'Image
         & ASCII.LF
         & "  BiBitCount:    "
         & DIB_Header.BiBitCount'Image
         & ASCII.LF
         & "  BiCompression: "
         & DIB_Header.BiCompression'Image,
         Logging_Tags);

      Width := DIB_Header.BiWidth;
      Height := DIB_Header.BiHeight;

      case DIB_Header.BiCompression is
         when 0      =>
            Pixel_Data_Format := Bitmap_Pixel_Format_RGB;

         when others =>
            Log_Error
              ("Unsupported bitmap compression: "
               & DIB_Header.BiCompression'Image);
            Result := Not_Supported;
            return;
      end case;

      Result := Success;
   exception
      when others =>
         Log_Error ("Constraint_Error: Parse_Bitmap");
         Result := Constraint_Exception;
   end Parse_Bitmap;

end Graphics;
