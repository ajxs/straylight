-------------------------------------------------------------------------------
--  Copyright (c) 2026, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
--  Handles the text of filesystem paths. Nothing here looks a path up in a
--  filesystem.
-------------------------------------------------------------------------------

private package Filesystems.Paths
  with Preelaborate
is
   ----------------------------------------------------------------------------
   --  Finds the next component of a path, skipping any separators before it.
   --  On return, the component is Path (Start_Index .. End_Index - 1), and
   --  Next_Token_Byte_Length is 0 when there are no more components.
   ----------------------------------------------------------------------------
   procedure Get_Next_Path_Component
     (Path                   : Filesystem_Path_T;
      Start_Index            : in out Integer;
      End_Index              : out Integer;
      Next_Token_Byte_Length : out Integer);

   ----------------------------------------------------------------------------
   --  Splits a path into its parent directory, Path (Path'First ..
   --  Parent_Last), and its last component's name, Path (Name_First ..
   --  Path'Last). The parent of a name directly under the root directory,
   --  such as "/name", is "/".
   --  Returns Invalid_Filename if the path has no separator, and so no parent
   --  directory.
   ----------------------------------------------------------------------------
   procedure Split_Path
     (Path        : Filesystem_Path_T;
      Parent_Last : out Natural;
      Name_First  : out Natural;
      Result      : out Function_Result);

   ----------------------------------------------------------------------------
   --  Transforms the specified path string into an absolute path with
   --  no "." or ".." components, and no repeated or trailing separators.
   --  A relative path is joined onto the specified Working_Directory, which
   --  must be absolute.
   --  ".." removes the component before it, and stays at the root directory.
   --  This is exact because there are no symbolic links.
   --  Returns Invalid_Filename for an empty path, and Path_Too_Long if the
   --  result doesn't fit in Absolute_Path.
   --  The result is checked as it's built, so a path that only fits after an
   --  included path traversal character like ".." is also considered too long.
   ----------------------------------------------------------------------------
   procedure Resolve_Path
     (Working_Directory : Filesystem_Path_T;
      Path              : Filesystem_Path_T;
      Absolute_Path     : out Fixed_Length_String_T;
      Result            : out Function_Result);

end Filesystems.Paths;
