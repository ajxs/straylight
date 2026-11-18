-------------------------------------------------------------------------------
--  Copyright (c) 2026, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Boot.Filesystem_Setup;
with Filesystems;      use Filesystems;
with Function_Results; use Function_Results;
with Loader;
with Logging;          use Logging;
with Processes.Scheduler;
with Utilities;

package body Boot.Init is
   procedure Initialise_Init_Process is
      Result : Function_Result := Unset;
   begin
      Create_New_Process (Init_Process, Result);
      if Is_Error (Result) then
         --  Error already printed.
         Panic;
      end if;

      --  Set the Init process' entry point.
      --  Instead of 'returning' to the userland process initialisation
      --  procedure like a regular process being initialised would, we instead
      --  jump straight to the kernel-level init process without 'returning'
      --  into userspace.
      Init_Process.all.Kernel_Context (ra) :=
        Address_To_Unsigned_64 (Start_Init_Process'Address);

      Add_Process_To_Process_Queue (Init_Process, Result);
      if Is_Error (Result) then
         Panic ("Error adding init process");
      end if;
   exception
      when Constraint_Error =>
         Panic_Constraint_Error;
   end Initialise_Init_Process;

   ----------------------------------------------------------------------------
   --  Searches the mounted volumes, in /Volumes entry order, for one that has
   --  a /Programs directory, which holds the startup programs.
   --  Returns the path of that directory if found, panics if not.
   ----------------------------------------------------------------------------
   procedure Find_Startup_Programs_Directory
     (Programs_Directory : out Utilities.Fixed_Length_String_T)
   is
      Read_Only_Flags : constant File_Open_Flags_T :=
        (Access_Mode    => Read_Only,
         Creation_Flags => (others => False),
         Status_Flags   => (others => False));

      Volumes_Directory_Handle  : Process_File_Handle_Access := null;
      Programs_Directory_Handle : Process_File_Handle_Access := null;
      Volume_Name               : Filesystem_Node_Name_T :=
        Null_Filesystem_Node_Name;
      Volume_Type               : Filesystem_Node_Type_T :=
        Filesystem_Node_Type_None;
      Result                    : Function_Result := Unset;
   begin
      Programs_Directory.Byte_Length := 0;

      Open_File
        (Init_Process.all,
         "/Volumes",
         Read_Only_Flags,
         Volumes_Directory_Handle,
         Result);
      if Is_Error (Result) or else Result = File_Not_Found then
         Panic ("Unable to open /Volumes");
      end if;

      loop
         Read_Next_Directory_Entry
           (Volumes_Directory_Handle, Volume_Name, Volume_Type, Result);
         if Is_Error (Result) then
            Panic;
         end if;

         exit when Result = End_Of_Directory;

         Path : constant Filesystem_Path_T :=
           "/Volumes/"
           & Volume_Name.Value (1 .. Volume_Name.Byte_Length)
           & "/Programs";

         pragma
           Debug
             (Debug_Boot,
              Log_Debug ("Checking for 'Programs' directory: path=" & Path));

         Open_File
           (Init_Process.all,
            Path,
            Read_Only_Flags,
            Programs_Directory_Handle,
            Result);
         if Is_Error (Result) then
            Panic;
         end if;

         if Result /= File_Not_Found then
            Close_File (Programs_Directory_Handle, Result);
            if Is_Error (Result) then
               Panic;
            end if;

            Programs_Directory.Value (1 .. Path'Length) := Path;
            Programs_Directory.Byte_Length := Path'Length;
            exit;
         end if;
      end loop;

      Close_File (Volumes_Directory_Handle, Result);
      if Is_Error (Result) then
         Panic;
      end if;

      if Programs_Directory.Byte_Length = 0 then
         Panic ("Unable to find volume with startup programs.");
      end if;
   exception
      when Constraint_Error =>
         Panic_Constraint_Error;
   end Find_Startup_Programs_Directory;

   procedure Load_Startup_Programs (Programs_Directory : Filesystem_Path_T) is
      Result : Function_Result := Unset;
   begin
      pragma
        Debug
          (Debug_Boot,
           Log_Debug ("Loading startup programs: path=" & Programs_Directory));

      Loader.Load_New_Process_From_Filesystem
        (Init_Process.all,
         Programs_Directory & "/print_fractal_pattern.elf",
         Result);
      if Is_Error (Result) then
         Panic;
      end if;

      Loader.Load_New_Process_From_Filesystem
        (Init_Process.all,
         Programs_Directory & "/print_fractal_pattern_2.elf",
         Result);
      if Is_Error (Result) then
         Panic;
      end if;

      Loader.Load_New_Process_From_Filesystem
        (Init_Process.all, Programs_Directory & "/simple_shell.elf", Result);
      if Is_Error (Result) then
         Panic;
      end if;
   exception
      when Constraint_Error =>
         Panic_Constraint_Error;
   end Load_Startup_Programs;

   procedure Start_Init_Process is
      --  Long enough for /Volumes/<name>/Programs, since root filesystem
      --  names are much shorter than a filesystem node name.
      Programs_Directory :
        Utilities.Fixed_Length_String_T (Filesystem_Node_Name_Max_Byte_Length);
   begin
      --  Note: There's no need to release the lock of any previous process
      --  when starting the init process, as it's impossible by design for any
      --  other process to be running at this point in time.
      pragma Debug (Debug_Boot, Log_Debug ("Starting init process..."));

      --  The graphics system is initialised here because the Virtio driver
      --  requires a process context to operate in.
      Initialise_Graphics;

      Filesystem_Setup.Mount_Disk_Filesystems;

      Find_Startup_Programs_Directory (Programs_Directory);

      Load_Startup_Programs
        (Programs_Directory.Value (1 .. Programs_Directory.Byte_Length));

      --  Wait for all the harts to start before freeing the boot memory.
      --  Since their boot stacks are in the boot memory region that would be
      --  freed here.
      Wait_For_All_Harts_To_Start;
      Free_Boot_Memory;

      --  Exit the init process via the scheduler.
      Processes.Scheduler.Run (Process_Stopped);
      Panic ("Init_Process still running after exit.");
   exception
      when Constraint_Error =>
         Panic_Constraint_Error;
   end Start_Init_Process;

end Boot.Init;
