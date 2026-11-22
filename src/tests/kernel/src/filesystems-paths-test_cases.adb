-------------------------------------------------------------------------------
--  Copyright (c) 2026, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with AUnit.Assertions; use AUnit.Assertions;

package body Filesystems.Paths.Test_Cases is
   --  The buffer length used for resolved paths in these tests.
   Test_Path_Length : constant := 256;

   --  Resolves Path against Working_Directory, and checks that it succeeds
   --  with the expected absolute path.
   procedure Assert_Resolves_To
     (Working_Directory : String; Path : String; Expected : String)
   is
      Resolved : Fixed_Length_String_T (Test_Path_Length);
      Result   : Function_Result := Unset;
   begin
      Resolve_Path (Working_Directory, Path, Resolved, Result);
      Assert
        (Result = Success,
         "'" & Path & "' in '" & Working_Directory & "' resolves");

      Actual : constant String := Resolved.Value (1 .. Resolved.Byte_Length);
      Assert
        (Actual = Expected,
         "'"
         & Path
         & "' in '"
         & Working_Directory
         & "' resolves to '"
         & Expected
         & "', not '"
         & Actual
         & "'");
   end Assert_Resolves_To;

   --  Resolves Path against Working_Directory, and checks that it fails with
   --  the expected result.
   procedure Assert_Fails_With
     (Working_Directory : String;
      Path              : String;
      Expected          : Function_Result;
      Max_Length        : Positive := Test_Path_Length)
   is
      Resolved : Fixed_Length_String_T (Max_Length);
      Result   : Function_Result := Unset;
   begin
      Resolve_Path (Working_Directory, Path, Resolved, Result);
      Assert
        (Result = Expected,
         "'"
         & Path
         & "' fails with"
         & Expected'Image
         & ", not"
         & Result'Image);
   end Assert_Fails_With;

   overriding
   procedure Run_Test (T : in out Test_Resolve_Absolute_Path) is
      pragma Unreferenced (T);
   begin
      Assert_Resolves_To ("/", "/a/b", "/a/b");
      Assert_Resolves_To ("/", "/", "/");
      Assert_Resolves_To ("/", "//", "/");
      Assert_Resolves_To ("/", "///a", "/a");
      Assert_Resolves_To ("/", "/a//b", "/a/b");
      Assert_Resolves_To ("/", "/a/b/", "/a/b");
      --  An absolute path ignores the working directory.
      Assert_Resolves_To ("/Volumes/0", "/Devices", "/Devices");
   end Run_Test;

   overriding
   procedure Run_Test (T : in out Test_Resolve_Dot_Components) is
      pragma Unreferenced (T);
   begin
      Assert_Resolves_To ("/", "/a/./b", "/a/b");
      Assert_Resolves_To ("/", "/.", "/");
      Assert_Resolves_To ("/", "/a/.", "/a");
      Assert_Resolves_To ("/x", ".", "/x");
      Assert_Resolves_To ("/x", "./y", "/x/y");
      --  Only a whole component of "." or ".." is special.
      Assert_Resolves_To ("/", "/.a/..b/...", "/.a/..b/...");
   end Run_Test;

   overriding
   procedure Run_Test (T : in out Test_Resolve_Dot_Dot_Components) is
      pragma Unreferenced (T);
   begin
      Assert_Resolves_To ("/", "/a/b/..", "/a");
      Assert_Resolves_To ("/", "/a/../b", "/b");
      --  ".." stays at the root directory.
      Assert_Resolves_To ("/", "/..", "/");
      Assert_Resolves_To ("/", "/a/../..", "/");
      Assert_Resolves_To ("/", "/../a", "/a");
      --  ".." out of a volume's root directory lands in /Volumes.
      Assert_Resolves_To ("/Volumes/0", "..", "/Volumes");
      Assert_Resolves_To
        ("/Volumes/0", "../1/words.txt", "/Volumes/1/words.txt");
      Assert_Resolves_To ("/Volumes/0", "../../..", "/");
   end Run_Test;

   overriding
   procedure Run_Test (T : in out Test_Resolve_Relative_Path) is
      pragma Unreferenced (T);
   begin
      Assert_Resolves_To
        ("/Volumes/0",
         "Programs/simple_shell.elf",
         "/Volumes/0/Programs/simple_shell.elf");
      Assert_Resolves_To ("/", "Devices", "/Devices");
      Assert_Resolves_To ("/Volumes/0/", "Programs/", "/Volumes/0/Programs");
      --  An empty working directory is treated as the root directory.
      Assert_Resolves_To ("", "a", "/a");
   end Run_Test;

   overriding
   procedure Run_Test (T : in out Test_Resolve_Invalid_Path) is
      pragma Unreferenced (T);
      Longest_Name  : constant String (1 .. Test_Path_Length - 1) :=
        [others => 'a'];
      Too_Long_Name : constant String (1 .. Test_Path_Length) :=
        [others => 'a'];
   begin
      Assert_Fails_With ("/", "", Invalid_Filename);

      --  The longest path that fits is exactly the buffer's length.
      Assert_Resolves_To ("/", "/" & Longest_Name, "/" & Longest_Name);
      Assert_Fails_With ("/", "/" & Too_Long_Name, Path_Too_Long);

      --  Each part fits, but the joined path doesn't.
      Assert_Fails_With
        ("/" & Longest_Name (1 .. 200),
         Longest_Name (1 .. 100),
         Path_Too_Long);

      --  The limit is the length of the caller's buffer.
      Assert_Fails_With ("/", "/abcd", Path_Too_Long, Max_Length => 4);
   end Run_Test;

   overriding
   procedure Run_Test (T : in out Test_Split_Path) is
      pragma Unreferenced (T);

      --  Splits Path, and checks the parent directory and name it gives.
      procedure Assert_Splits_To
        (Path : String; Expected_Parent : String; Expected_Name : String)
      is
         Parent_Last : Natural := 0;
         Name_First  : Natural := 0;
         Result      : Function_Result := Unset;
      begin
         Split_Path (Path, Parent_Last, Name_First, Result);
         Assert (Result = Success, "'" & Path & "' splits");

         Actual_Parent : constant String := Path (Path'First .. Parent_Last);
         Actual_Name : constant String := Path (Name_First .. Path'Last);
         Assert
           (Actual_Parent = Expected_Parent
            and then Actual_Name = Expected_Name,
            "'"
            & Path
            & "' splits into '"
            & Expected_Parent
            & "' and '"
            & Expected_Name
            & "', not '"
            & Actual_Parent
            & "' and '"
            & Actual_Name
            & "'");
      end Assert_Splits_To;

      --  The indexes are relative to the path's own bounds.
      Offset_Path : constant String (5 .. 10) := "/a/bcd";

      Parent_Last : Natural := 0;
      Name_First  : Natural := 0;
      Result      : Function_Result := Unset;
   begin
      Assert_Splits_To ("/a/b", "/a", "b");
      Assert_Splits_To ("/a/b/c.txt", "/a/b", "c.txt");
      --  A name directly under the root directory has "/" as its parent.
      Assert_Splits_To ("/name", "/", "name");
      Assert_Splits_To (Offset_Path, "/a", "bcd");

      Split_Path ("name", Parent_Last, Name_First, Result);
      Assert
        (Result = Invalid_Filename,
         "A path with no separator fails with Invalid_Filename");
   end Run_Test;

end Filesystems.Paths.Test_Cases;
