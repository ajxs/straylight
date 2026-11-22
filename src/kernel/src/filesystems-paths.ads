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
