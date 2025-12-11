with System;     use System;
with Interfaces; use Interfaces;
with Straylight; use Straylight;

procedure Print_Random_Words is
   Result         : Syscall_Result_T;
   File_Handle_Id : File_Handle_Id_T;

   Bytes_To_Read  : constant Unsigned_64 := 8;
   Bytes_Read     : Unsigned_64 := 0;
   Buffer_Address : Address := Null_Address;

   Number_Of_Words : constant Unsigned_64 := 10;
begin
   Straylight.Print_To_Serial
     ("Printing " & Number_Of_Words'Image & " words." & ASCII.LF);

   Straylight.Allocate_Memory (64, Buffer_Address, Result);
   if Result /= Syscall_Result_Success then
      Straylight.Log_Error ("Failed to allocate memory");
      return;
   end if;

   Straylight.Open_File
     ("/Devices/Disk_B/seven_letter_words.txt",
      File_Open_Mode_Read,
      File_Handle_Id,
      Result);
   if Result /= Syscall_Result_Success then
      Straylight.Log_Error ("Failed to open file");
      return;
   end if;

   loop
      Straylight.Seek_File (File_Handle_Id, 0, Result);
      if Result /= Syscall_Result_Success then
         Straylight.Log_Error ("Failed to seek file");
         return;
      end if;

      for I in 1 .. Number_Of_Words loop
         Straylight.Read_File
           (File_Handle_Id, Buffer_Address, Bytes_To_Read, Bytes_Read, Result);
         if Result /= Syscall_Result_Success
           or else Bytes_Read /= Bytes_To_Read
         then
            Straylight.Log_Error ("Failed to read file");
            return;
         end if;

         declare
            Buffer_String : constant String (1 .. Integer (Bytes_To_Read))
            with
              Import,
              Convention => C,
              Alignment  => 1,
              Address    => Buffer_Address;
         begin
            Straylight.Print_To_Serial
              ("Word "
               & I'Image
               & ": "
               & Buffer_String (1 .. Integer (Bytes_To_Read)));
         end;
      end loop;
   end loop;
exception
   when others =>
      Straylight.Log_Error ("Exception occurred in Print_Random_Words");
end Print_Random_Words;
