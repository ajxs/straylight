-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Devices.Ramdisk;
with Devices.Virtio.Block;
with Logging;             use Logging;
with Processes.Scheduler; use Processes.Scheduler;
with RISCV;

package body Filesystems.Block_Cache is
   procedure Find_And_Claim_Available_Block_Cache_Entry
     (Filesystem   : Filesystem_Access;
      Block_Number : Block_Index_T;
      Cache_Index  : out Positive;
      Result       : out Function_Result)
   is
      Best_Eviction_Candidate_Index : Natural := 0;
      Lowest_Access_Time            : Unsigned_64 := Unsigned_64'Last;
   begin
      --  Search for an unused entry.
      for I in System_Block_Cache.Entries'Range loop
         if not System_Block_Cache.Entries (I).Entry_Used then
            Cache_Index := I;

            goto Claim_Entry;
         end if;
      end loop;

      --  If no unused entry was found, search for an entry that can be
      --  evicted. The available cache entry with the lowest last access time
      --  and a reference count of zero will be chosen.
      for I in System_Block_Cache.Entries'Range loop
         if System_Block_Cache.Entries (I).Reference_Count = 0 then
            if System_Block_Cache.Entries (I).Last_Access_Time
              < Lowest_Access_Time
            then
               Lowest_Access_Time :=
                 System_Block_Cache.Entries (I).Last_Access_Time;
               Best_Eviction_Candidate_Index := I;
            end if;
         end if;
      end loop;

      if Best_Eviction_Candidate_Index /= 0 then
         pragma
           Debug
             (Debug_Filesystems_Block_Cache,
              Log_Debug
                ("Invalidating block cache entry at index: "
                 & Best_Eviction_Candidate_Index'Image));

         Cache_Index := Best_Eviction_Candidate_Index;
         goto Claim_Entry;
      end if;

      Cache_Index := System_Block_Cache.Entries'First;
      Result := Not_Found;
      return;

      <<Claim_Entry>>
      System_Block_Cache.Entries (Cache_Index).Entry_Used := True;
      System_Block_Cache.Entries (Cache_Index).Filesystem := Filesystem;
      System_Block_Cache.Entries (Cache_Index).Block_Number := Block_Number;
      System_Block_Cache.Entries (Cache_Index).Data_Is_Valid := False;
      System_Block_Cache.Entries (Cache_Index).Last_Access_Time :=
        RISCV.Get_System_Time;

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Constraint_Error;
         Result := Constraint_Exception;
   end Find_And_Claim_Available_Block_Cache_Entry;

   procedure Find_Existing_Block_In_Cache
     (Cache        : in out Block_Cache_T;
      Filesystem   : Filesystem_Access;
      Block_Number : Block_Index_T;
      Cache_Index  : out Positive;
      Result       : out Function_Result) is
   begin
      for I in Cache.Entries'Range loop
         if Cache.Entries (I).Entry_Used
           and then Cache.Entries (I).Filesystem = Filesystem
           and then Cache.Entries (I).Block_Number = Block_Number
         then
            Cache_Index := I;
            Result := Success;
            return;
         end if;
      end loop;

      Result := Not_Found;
   end Find_Existing_Block_In_Cache;

   pragma
     Warnings
       (Off, "pragma Restrictions (No_Exception_Propagation) in effect");
   procedure Get_Block_Cache_Entry_Data_Address
     (Cache                 : Block_Cache_T;
      Cache_Index           : Positive;
      Data_Address_Virtual  : out Virtual_Address_T;
      Data_Address_Physical : out Physical_Address_T;
      Result                : out Function_Result) is
   begin
      if Cache_Index > Cache.Entries'Last then
         Result := Invalid_Argument;
         return;
      end if;

      Cache_Entry_Offset : constant Storage_Offset :=
        Storage_Offset (Block_Size * (Cache_Index - 1));

      Data_Address_Virtual := Cache.Data_Address_Virtual + Cache_Entry_Offset;
      Data_Address_Physical :=
        Cache.Data_Address_Physical + Cache_Entry_Offset;

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Constraint_Error;
         Result := Constraint_Exception;
   end Get_Block_Cache_Entry_Data_Address;
   pragma
     Warnings (On, "pragma Restrictions (No_Exception_Propagation) in effect");

   procedure Read_Block_From_Filesystem
     (Filesystem           : Filesystem_Access;
      Reading_Process      : in out Process_Control_Block_T;
      Block_Number         : Block_Index_T;
      Data_Virtual_Address : out Virtual_Address_T;
      Result               : out Function_Result)
   is
      Cache_Index : Positive := 1;

      Cache_Entry_Addr_Physical : Physical_Address_T := Null_Physical_Address;
   begin
      if not Is_Valid_Filesystem_Pointer (Filesystem) then
         Log_Error ("Read_Block_From_Filesystem: Unsupported filesystem");
         Result := Invalid_Argument;
         return;
      end if;

      pragma
        Debug
          (Debug_Filesystems_Block_Cache,
           Log_Debug ("Read_Block_From_Filesystem: " & Block_Number'Image));

      --  Acquire the block cache spinlock prior to searching the cache.
      --  We need to ensure the block we're looking for isn't being modified
      --  by another process while we're searching.
      Acquire_Spinlock (System_Block_Cache.Spinlock);

      --  The search and the claim need to happen under one hold of the cache
      --  spinlock. Otherwise two processes could claim separate entries for
      --  the same block.
      Find_Or_Claim_Entry_Loop : loop
         Find_Existing_Block_In_Cache
           (System_Block_Cache, Filesystem, Block_Number, Cache_Index, Result);
         exit Find_Or_Claim_Entry_Loop when Result = Success;

         Find_And_Claim_Available_Block_Cache_Entry
           (Filesystem, Block_Number, Cache_Index, Result);
         exit Find_Or_Claim_Entry_Loop when Result /= Not_Found;

         pragma
           Debug
             (Debug_Filesystems_Block_Cache,
              Log_Debug
                ("No available block cache entries. "
                 & "Waiting for an entry to become available..."));

         --  If we couldn't find an unused or invalidatable entry, we need to
         --  wait for an entry to become available.
         --  To do this, we release the block cache spinlock, and yield the CPU
         --  to allow other processes to run, which may release cache entries.
         Release_Spinlock (System_Block_Cache.Spinlock);
         Run;
         Acquire_Spinlock (System_Block_Cache.Spinlock);
      end loop Find_Or_Claim_Entry_Loop;

      if Is_Error (Result) then
         Release_Spinlock (System_Block_Cache.Spinlock);
         return;
      end if;

      --  Incrementing the reference count ensures that the entry won't be
      --  claimed for another block while the current process is waiting
      --  to acquire the sleeplock.
      System_Block_Cache.Entries (Cache_Index).Reference_Count :=
        System_Block_Cache.Entries (Cache_Index).Reference_Count + 1;

      --  If this block is currently being used by another process, the
      --  current process will 'sleep' here until the other process is done
      --  with it.
      Acquire_Sleeplock
        (System_Block_Cache.Entries (Cache_Index).Sleeplock,
         System_Block_Cache.Spinlock,
         Reading_Process);

      Release_Spinlock (System_Block_Cache.Spinlock);

      if not System_Block_Cache.Entries (Cache_Index).Data_Is_Valid then
         Read_Write_Block_Cache_Entry_Data
           (System_Block_Cache,
            Filesystem,
            Reading_Process,
            Block_Number,
            Cache_Index,
            Write  => False,
            Result => Result);

         --  In the case that reading the block from the filesystem
         --  failed, release the cache entry we allocated.
         if Is_Error (Result) then
            --  Save the original error result.
            Error_Result : constant Function_Result := Result;

            --  Specifically invalidate the invalid cache entry, so that the
            --  invalid entry isn't picked up by another process.
            Release_Block (Filesystem, Block_Number, Result, True);

            Result := Error_Result;

            return;
         end if;

         System_Block_Cache.Entries (Cache_Index).Data_Is_Valid := True;
      end if;

      Get_Block_Cache_Entry_Data_Address
        (System_Block_Cache,
         Cache_Index,
         Data_Virtual_Address,
         Cache_Entry_Addr_Physical,
         Result);
      if Is_Error (Result) then
         return;
      end if;

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Constraint_Error;
         Result := Constraint_Exception;
   end Read_Block_From_Filesystem;

   procedure Read_Write_Block_Cache_Entry_Data
     (Cache        : in out Block_Cache_T;
      Filesystem   : Filesystem_Access;
      Process      : in out Process_Control_Block_T;
      Block_Number : Block_Index_T;
      Cache_Index  : Positive;
      Write        : Boolean;
      Result       : out Function_Result)
   is
      Data_Addr_Virtual  : Virtual_Address_T := Null_Address;
      Data_Addr_Physical : Physical_Address_T := Null_Physical_Address;

      --  Currently all of the supported storage devices have fixed 512-byte
      --  sectors, so we can hardcode that here. (VirtIO specifies 512-byte
      --  sectors, and the ramdisk is implemented with 512-byte sectors.)
      Sector_Size : constant := 512;
   begin
      pragma
        Debug
          (Debug_Filesystems_Block_Cache,
           Log_Debug
             ((if Write
               then "Writing block from cache: "
               else "Reading block into cache: ")
              & Block_Number'Image));

      Get_Block_Cache_Entry_Data_Address
        (Cache, Cache_Index, Data_Addr_Virtual, Data_Addr_Physical, Result);
      if Is_Error (Result) then
         return;
      end if;

      Start_Sector : constant Sector_Index_T :=
        (Block_Number * Block_Size) / Sector_Size;

      Sectors_Per_Block : constant Natural :=
        Get_Sectors_Per_Block (Sector_Size);

      case Filesystem.all.Device.all.Device_Bus is
         when Device_Bus_Virtio_MMIO   =>
            if Write then
               Devices.Virtio.Block.Write_Sectors
                 (Process,
                  Filesystem.all.Device.all,
                  Data_Addr_Physical,
                  Start_Sector,
                  Sectors_Per_Block,
                  Result);
            else
               Devices.Virtio.Block.Read_Sectors
                 (Process,
                  Filesystem.all.Device.all,
                  Data_Addr_Physical,
                  Start_Sector,
                  Sectors_Per_Block,
                  Result);
            end if;

         when Device_Bus_Memory_Mapped =>
            if Write then
               Devices.Ramdisk.Write_Sectors
                 (Filesystem.all.Device.all,
                  Start_Sector,
                  Sectors_Per_Block,
                  Data_Addr_Virtual,
                  Result);
            else
               Devices.Ramdisk.Read_Sectors
                 (Filesystem.all.Device.all,
                  Start_Sector,
                  Sectors_Per_Block,
                  Data_Addr_Virtual,
                  Result);
            end if;

         when others                   =>
            Log_Error ("Unsupported device bus");
            Result := Not_Supported;

      end case;
   exception
      when Constraint_Error =>
         Log_Constraint_Error;
         Result := Constraint_Exception;
   end Read_Write_Block_Cache_Entry_Data;

   procedure Release_Block_Unlocked
     (Filesystem             : Filesystem_Access;
      Block_Number           : Block_Index_T;
      Result                 : out Function_Result;
      Invalidate_Cache_Entry : Boolean)
   is
      Cache_Index : Positive := 1;
   begin
      if not Is_Valid_Filesystem_Pointer (Filesystem) then
         Log_Error ("Release_Block: Unsupported filesystem");
         Result := Invalid_Argument;
         return;
      end if;

      pragma
        Debug
          (Debug_Filesystems_Block_Cache,
           Log_Debug ("Releasing block: " & Block_Number'Image));

      Find_Existing_Block_In_Cache
        (System_Block_Cache, Filesystem, Block_Number, Cache_Index, Result);
      if Is_Error (Result) then
         return;
      end if;

      pragma
        Debug
          (Debug_Filesystems_Block_Cache,
           Log_Debug ("Found block to release in cache."));

      --  Under some circumstances, such as when reading data into the cache
      --  entry fails, we may want to invalidate the cache entry before
      --  releasing it, to ensure it's not used by another process in an
      --  invalid state.
      System_Block_Cache.Entries (Cache_Index).Reference_Count :=
        System_Block_Cache.Entries (Cache_Index).Reference_Count - 1;

      System_Block_Cache.Entries (Cache_Index).Last_Access_Time :=
        RISCV.Get_System_Time;

      if Invalidate_Cache_Entry then
         System_Block_Cache.Entries (Cache_Index).Data_Is_Valid := False;

         --  Any processes still waiting for this entry will retry the read.
         if System_Block_Cache.Entries (Cache_Index).Reference_Count = 0 then
            System_Block_Cache.Entries (Cache_Index).Entry_Used := False;
            System_Block_Cache.Entries (Cache_Index).Filesystem := null;
            System_Block_Cache.Entries (Cache_Index).Block_Number := 0;
         end if;
      end if;

      Release_Sleeplock (System_Block_Cache.Entries (Cache_Index).Sleeplock);

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Constraint_Error;
         Result := Constraint_Exception;
   end Release_Block_Unlocked;

   procedure Release_Block
     (Filesystem             : Filesystem_Access;
      Block_Number           : Block_Index_T;
      Result                 : out Function_Result;
      Invalidate_Cache_Entry : Boolean := False) is
   begin
      Acquire_Spinlock (System_Block_Cache.Spinlock);
      Release_Block_Unlocked
        (Filesystem, Block_Number, Result, Invalidate_Cache_Entry);
      Release_Spinlock (System_Block_Cache.Spinlock);
   end Release_Block;

   procedure Write_Block_To_Filesystem
     (Filesystem      : Filesystem_Access;
      Writing_Process : in out Process_Control_Block_T;
      Block_Number    : Block_Index_T;
      Result          : out Function_Result)
   is
      Cache_Index : Positive := 1;
   begin
      if not Is_Valid_Filesystem_Pointer (Filesystem) then
         Log_Error ("Write_Block_To_Filesystem: Unsupported filesystem");
         Result := Invalid_Argument;
         return;
      end if;

      pragma
        Debug
          (Debug_Filesystems_Block_Cache,
           Log_Debug ("Write_Block_To_Filesystem: " & Block_Number'Image));

      --  Acquire the block cache spinlock prior to searching the cache.
      --  We need to ensure the block we're looking for isn't being modified
      --  by another process while we're searching.
      Acquire_Spinlock (System_Block_Cache.Spinlock);

      Find_Existing_Block_In_Cache
        (System_Block_Cache, Filesystem, Block_Number, Cache_Index, Result);

      Release_Spinlock (System_Block_Cache.Spinlock);

      if Is_Error (Result) then
         return;
      end if;

      if not Is_Sleeplock_Held_By_Process
               (System_Block_Cache.Entries (Cache_Index).Sleeplock,
                Writing_Process.Process_Id)
      then
         Log_Error ("Write_Block_To_Filesystem: Block sleeplock not held.");
         Result := Invalid_Argument;
         return;
      end if;

      Read_Write_Block_Cache_Entry_Data
        (System_Block_Cache,
         Filesystem,
         Writing_Process,
         Block_Number,
         Cache_Index,
         Write  => True,
         Result => Result);
      if Is_Error (Result) then
         return;
      end if;

   exception
      when Constraint_Error =>
         Log_Constraint_Error;
         Result := Constraint_Exception;
   end Write_Block_To_Filesystem;

   procedure Write_Block_To_Filesystem_And_Release
     (Filesystem      : Filesystem_Access;
      Writing_Process : in out Process_Control_Block_T;
      Block_Number    : Block_Index_T;
      Result          : out Function_Result)
   is
      Write_Result, Release_Result : Function_Result := Unset;
   begin
      Write_Block_To_Filesystem
        (Filesystem, Writing_Process, Block_Number, Write_Result);

      Release_Block (Filesystem, Block_Number, Release_Result);

      Result :=
        (if Is_Error (Write_Result) then Write_Result else Release_Result);
   end Write_Block_To_Filesystem_And_Release;

end Filesystems.Block_Cache;
