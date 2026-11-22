-------------------------------------------------------------------------------
--  Copyright (c) 2026, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Logging; use Logging;

package body Filesystems.Paths is
   ----------------------------------------------------------------------------
   --  Adds each component of Source to the end of Absolute_Path, with a
   --  separator before each one. "." is skipped, and ".." removes the last
   --  component. Returns Path_Too_Long if a component doesn't fit.
   ----------------------------------------------------------------------------
   procedure Append_Path_Components
     (Source        : Filesystem_Path_T;
      Absolute_Path : in out Fixed_Length_String_T;
      Result        : out Function_Result)
   is
      Start_Index : Integer := Source'First;
      End_Index   : Integer := Source'First;
      Length      : Integer := 0;
   begin
      loop
         --  Get the next path component from the source path.
         Get_Next_Path_Component (Source, Start_Index, End_Index, Length);
         exit when Length = 0;

         Component : constant Filesystem_Path_T :=
           Source (Start_Index .. End_Index - 1);

         if Component = "." then
            --  Skip the current component as it represents the current
            --  directory.
            null;
         elsif Component = ".." then
            --  Remove the last component by cutting the path back to just
            --  before its last separator.
            --  At the root directory the path is empty, so the loop doesn't
            --  run, and ".." stays at the root.
            for I in reverse
              1 .. Absolute_Path.Byte_Length
              when Absolute_Path.Value (I) = Filesystem_Node_Separator
            loop
               Absolute_Path.Byte_Length := I - 1;
               exit;
            end loop;
         else
            --  Add the new component to the absolute path.
            --  First check that the new component fits in the absolute path.
            New_Length : constant Natural :=
              Absolute_Path.Byte_Length + 1 + Component'Length;

            if New_Length > Absolute_Path.Max_Length then
               Log_Error ("Path is too long");
               Result := Path_Too_Long;
               return;
            end if;

            --  Append a path separator character, then the new path component.
            Absolute_Path.Value (Absolute_Path.Byte_Length + 1) :=
              Filesystem_Node_Separator;

            Absolute_Path.Value
              (Absolute_Path.Byte_Length + 2 .. New_Length) :=
              Component;
            Absolute_Path.Byte_Length := New_Length;
         end if;

         Start_Index := End_Index;
      end loop;

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Constraint_Error;
         Result := Constraint_Exception;
   end Append_Path_Components;

   procedure Resolve_Path
     (Working_Directory : Filesystem_Path_T;
      Path              : Filesystem_Path_T;
      Absolute_Path     : out Fixed_Length_String_T;
      Result            : out Function_Result) is
   begin
      Absolute_Path.Byte_Length := 0;

      if Path'Length = 0 then
         Log_Error ("Path is empty");
         Result := Invalid_Filename;
         return;
      end if;

      if Path (Path'First) /= Filesystem_Node_Separator then
         Append_Path_Components (Working_Directory, Absolute_Path, Result);
         if Is_Error (Result) then
            return;
         end if;
      end if;

      Append_Path_Components (Path, Absolute_Path, Result);
      if Is_Error (Result) then
         return;
      end if;

      --  A path with no components left is the root directory.
      if Absolute_Path.Byte_Length = 0 then
         Absolute_Path.Value (1) := Filesystem_Node_Separator;
         Absolute_Path.Byte_Length := 1;
      end if;

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Constraint_Error;
         Absolute_Path.Byte_Length := 0;
         Result := Constraint_Exception;
   end Resolve_Path;

end Filesystems.Paths;
