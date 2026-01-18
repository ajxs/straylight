-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

package body Filesystems.Root is
   procedure Add_Filesystem_Node_To_Root_Filesystem
     (Filesystem      : Filesystem_Access;
      Filename        : Filesystem_Path_T;
      Parent_Index    : Filesystem_Node_Index_T;
      New_Node_Index  : out Filesystem_Node_Index_T;
      Result          : out Function_Result;
      Node_Type       : Filesystem_Node_Type_T := Filesystem_Node_Type_File;
      Node_Filesystem : Filesystem_Access := null)
   is
      New_Entry_Index : Positive := 1;
   begin
      if not Is_Valid_Filesystem_Pointer (Filesystem)
        or else Filesystem.all.Filesystem_Type /= Filesystem_Type_Root
      then
         Log_Error ("Invalid root filesystem");
         New_Node_Index := 0;
         Result := Invalid_Argument;
         return;
      end if;

      if Filename'Length = 0
        or else Filename'Length > Maximum_Root_Filesystem_Filename_Length
      then
         Log_Error ("Invalid filename length");
         New_Node_Index := 0;
         Result := Invalid_Argument;
         return;
      end if;

      declare
         Root_Filesystem : Root_Filesystem_T
         with
           Import,
           Convention => Ada,
           Alignment  => 1,
           Address    => Filesystem.all.Device.all.Virtual_Address;
      begin
         Find_Free_Root_Filesystem_Node_Index
           (Root_Filesystem, New_Entry_Index, Result);
         if Is_Error (Result) then
            return;
         end if;

         Root_Filesystem.Nodes (New_Entry_Index) :=
           (Entry_Used           => True,
            Index                => Filesystem_Node_Index_T (New_Entry_Index),
            Parent_Index         => Parent_Index,
            Filename             => [others => Character'Val (0)],
            Filename_Byte_Length => 0,
            Node_Type            => Node_Type,
            Filesystem           => Node_Filesystem);

         Root_Filesystem.Nodes (New_Entry_Index).Filename
           (1 .. Filename'Length) :=
           Filename (Filename'Range);
         Root_Filesystem.Nodes (New_Entry_Index).Filename_Byte_Length :=
           Filename'Length;

         New_Node_Index := Root_Filesystem.Nodes (New_Entry_Index).Index;
      end;

      Log_Debug
        ("Added filesystem node to root filesystem with index: "
         & New_Entry_Index'Image,
         Logging_Tags_FS_Root);

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error
           ("Constraint_Error: Add_Filesystem_Node_To_Root_Filesystem");
         Result := Constraint_Exception;
   end Add_Filesystem_Node_To_Root_Filesystem;

   procedure Find_Free_Root_Filesystem_Node_Index
     (Root_Filesystem : Root_Filesystem_T;
      Free_Index      : out Positive;
      Result          : out Function_Result) is
   begin
      for Index in Root_Filesystem.Nodes'Range loop
         if not Root_Filesystem.Nodes (Index).Entry_Used then
            Free_Index := Index;
            Result := Success;
            return;
         end if;
      end loop;

      Free_Index := 1;
      Result := No_Free_Entries;
   end Find_Free_Root_Filesystem_Node_Index;

   procedure Find_File
     (Filesystem        : Filesystem_Access;
      Filename          : Filesystem_Path_T;
      Parent_Node_Index : Unsigned_64;
      Filesystem_Node   : out Filesystem_Node_Access;
      Result            : out Function_Result) is
   begin
      declare
         Root_Filesystem : Root_Filesystem_T
         with
           Import,
           Convention => Ada,
           Alignment  => 1,
           Address    => Filesystem.all.Device.all.Virtual_Address;
      begin
         for Current_Node of Root_Filesystem.Nodes loop
            if Current_Node.Entry_Used then
               Log_Debug
                 ("Parsed entry with filename: '"
                  & Current_Node.Filename
                      (1 .. Current_Node.Filename_Byte_Length)
                  & "'",
                  Logging_Tags_FS_Root);

               if Does_Root_FS_Node_Name_Match_Path_Name
                    (Current_Node.Filename,
                     Current_Node.Filename_Byte_Length,
                     Filename)
                 and then Current_Node.Parent_Index = Parent_Node_Index
               then
                  Create_Filesystem_Node_Cache_Entry
                    (Filesystem,
                     Filename,
                     Filesystem_Node,
                     Result,
                     Current_Node.Index,
                     Current_Node.Parent_Index,
                     0,
                     0,
                     Current_Node.Node_Type,
                     Current_Node.Filesystem);
                  if Is_Error (Result) then
                     Filesystem_Node := null;
                     return;
                  end if;

                  Result := Success;
                  return;
               end if;
            end if;
         end loop;
      end;

      Filesystem_Node := null;
      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Find_File");
         Filesystem_Node := null;
         Result := Constraint_Exception;
   end Find_File;

   procedure Initialise_Root_Filesystem
     (Filesystem : Filesystem_Access; Result : out Function_Result) is
   begin
      if not Is_Valid_Filesystem_Pointer (Filesystem)
        or else Filesystem.all.Filesystem_Type /= Filesystem_Type_Root
      then
         Log_Error ("Invalid root filesystem");
         Result := Invalid_Argument;
         return;
      end if;

      Log_Debug ("Initialising root filesystem...", Logging_Tags_FS_Root);

      declare
         Root_Filesystem : Root_Filesystem_T
         with
           Import,
           Convention => Ada,
           Alignment  => 1,
           Address    => Filesystem.all.Device.all.Virtual_Address;
      begin
         for Index in Root_Filesystem.Nodes'Range loop
            Root_Filesystem.Nodes (Index).Entry_Used := False;
         end loop;
      end;

      Log_Debug ("Initialised root filesystem.", Logging_Tags_FS_Root);
      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Initialise_Root_Filesystem");
         Result := Constraint_Exception;
   end Initialise_Root_Filesystem;

   function Does_Root_FS_Node_Name_Match_Path_Name
     (Node_Name             : Filesystem_Path_T;
      Node_Name_Byte_Length : Integer;
      Path                  : Filesystem_Path_T) return Boolean is
   begin
      if Node_Name_Byte_Length /= Path'Length then
         return False;
      end if;

      for Index in 1 .. Node_Name_Byte_Length loop
         if Node_Name (Index) /= Path (Index) then
            return False;
         end if;
      end loop;

      return True;
   exception
      when Constraint_Error =>
         Log_Error
           ("Constraint_Error: Does_Root_FS_Node_Name_Match_Path_Name");
         return False;
   end Does_Root_FS_Node_Name_Match_Path_Name;

end Filesystems.Root;
