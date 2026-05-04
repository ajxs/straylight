-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Devices.Ramdisk;
with Devices.Virtio.Block;
with Processes.Scheduler; use Processes.Scheduler;
with RISCV;

package body Filesystems.Block_Cache is
   function Can_Block_Cache_Entry_Be_Invalidated
     (Cache        : Block_Cache_T;
      Cache_Index  : Positive;
      Current_Time : Unsigned_64) return Boolean is
   begin
      return
        (Cache.Entries (Cache_Index).Entry_Used
         and then
           not Is_Sleeplock_Locked (Cache.Entries (Cache_Index).Sleeplock)
         and then
           (Current_Time - Cache.Entries (Cache_Index).Last_Access)
           > Cache_Entry_Age_Threshold);
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Can_Block_Cache_Entry_Be_Invalidated");
         return False;
   end Can_Block_Cache_Entry_Be_Invalidated;

   procedure Find_And_Claim_Available_Block_Cache_Entry
     (Cache_Index : out Positive; Result : out Function_Result) is
   begin
      Claim_Entry_Loop : loop
         --  Search for an unused entry.
         for I in System_Block_Cache.Entries'Range loop
            if not System_Block_Cache.Entries (I).Entry_Used then
               Cache_Index := I;

               exit Claim_Entry_Loop;
            end if;
         end loop;

         --  Search for an entry that can be invalidated.
         for I in System_Block_Cache.Entries'Range loop
            if Can_Block_Cache_Entry_Be_Invalidated
                 (System_Block_Cache, I, RISCV.Get_System_Time)
            then
               Log_Debug
                 ("Invalidating block cache entry at index: " & I'Image,
                  Logging_Tags_Block_Cache);

               Cache_Index := I;

               exit Claim_Entry_Loop;
            end if;
         end loop;

         Log_Debug
           ("No available block cache entries. "
            & "Waiting for an entry to become available...",
            Logging_Tags_Block_Cache);

         --  If we couldn't find an unused or invalidatable entry, we need to
         --  wait for an entry to become available.
         --  To do this, we release the block cache spinlock, and yield the CPU
         --  to allow other processes to run, which may release cache entries.
         Release_Spinlock (System_Block_Cache.Spinlock);
         Run;
         Acquire_Spinlock (System_Block_Cache.Spinlock);

      end loop Claim_Entry_Loop;

      System_Block_Cache.Entries (Cache_Index).Entry_Used := True;
      System_Block_Cache.Entries (Cache_Index).Filesystem := null;
      System_Block_Cache.Entries (Cache_Index).Block_Number := 0;
      System_Block_Cache.Entries (Cache_Index).Last_Access :=
        RISCV.Get_System_Time;

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error
           ("Constraint_Error: Find_And_Claim_Available_Block_Cache_Entry");
         Result := Constraint_Exception;
   end Find_And_Claim_Available_Block_Cache_Entry;

   function Is_Matching_Used_Cache_Entry
     (Cache        : Block_Cache_T;
      Cache_Index  : Positive;
      Filesystem   : Filesystem_Access;
      Block_Number : Block_Index_T) return Boolean is
   begin
      return
        (Cache.Entries (Cache_Index).Entry_Used
         and then Cache.Entries (Cache_Index).Filesystem = Filesystem
         and then Cache.Entries (Cache_Index).Block_Number = Block_Number);
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Is_Matching_Used_Cache_Entry");
         return False;
   end Is_Matching_Used_Cache_Entry;

   procedure Find_Existing_Block_In_Cache
     (Cache        : in out Block_Cache_T;
      Filesystem   : Filesystem_Access;
      Block_Number : Block_Index_T;
      Cache_Index  : out Positive;
      Result       : out Function_Result) is
   begin
      for I in Cache.Entries'Range loop
         if Is_Matching_Used_Cache_Entry (Cache, I, Filesystem, Block_Number)
         then
            Cache_Index := I;
            Result := Success;
            return;
         end if;
      end loop;

      Result := Cache_Entry_Not_Found;
   end Find_Existing_Block_In_Cache;

   pragma
     Warnings
       (Off, "pragma Restrictions (No_Exception_Propagation) in effect");
   procedure Get_Block_Cache_Entry_Data_Address
     (Cache                 : Block_Cache_T;
      Cache_Index           : Positive;
      Data_Address_Virtual  : out Virtual_Address_T;
      Data_Address_Physical : out Physical_Address_T;
      Result                : out Function_Result)
   is
      Cache_Entry_Offset : Storage_Offset := 0;
   begin
      if Cache_Index > Cache.Entries'Last then
         Result := Invalid_Argument;
         return;
      end if;

      Cache_Entry_Offset := Storage_Offset (Block_Size * (Cache_Index - 1));

      Data_Address_Virtual := Cache.Data_Address_Virtual + Cache_Entry_Offset;
      Data_Address_Physical :=
        Cache.Data_Address_Physical + Cache_Entry_Offset;

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Get_Block_Cache_Entry_Data_Address");
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

      Read_Block_Retry_Count     : Natural := 0;
      Read_Block_Retry_Threshold : constant Natural := 3;

      Cache_Entry_Addr_Physical : Physical_Address_T := Null_Physical_Address;
   begin
      if not Is_Valid_Filesystem_Pointer (Filesystem) then
         Log_Error ("Read_Block_From_Filesystem: Unsupported filesystem");
         Result := Invalid_Argument;
         return;
      end if;

      Log_Debug
        ("Read_Block_From_Filesystem: " & Block_Number'Image,
         Logging_Tags_Block_Cache);

      --  We read from/into the cache in a loop, so that if reading into a
      --  cache entry fails, we can release the cache lock, and immediately
      --  loop back to retry with another cache entry.
      Read_From_Cache_Loop : loop
         --  Acquire the block cache spinlock prior to searching the cache.
         --  We need to ensure the block we're looking for isn't being modified
         --  by another process while we're searching.
         Acquire_Spinlock (System_Block_Cache.Spinlock);

         Find_Existing_Block_In_Cache
           (System_Block_Cache, Filesystem, Block_Number, Cache_Index, Result);

         --  Two possible results: Success / Cache_Entry_Not_Found.
         if Result = Success then
            Log_Debug
              ("Found existing block in cache.", Logging_Tags_Block_Cache);

            --  Acquire the cache entry's sleeplock.
            --  This means that if this block is currently being used by
            --  another process, the current process will 'sleep' here until
            --  the other process is done with it.
            --  If free, the process will acquire the lock and continue.
            Acquire_Sleeplock
              (System_Block_Cache.Entries (Cache_Index).Sleeplock,
               System_Block_Cache.Spinlock,
               Reading_Process.Process_Id);

            --  Test whether the cache entry we previously found is no longer
            --  valid. This could happen if another process released the cache
            --  entry after we found it, but before we acquired the sleeplock.
            --  During that time, it could have been reused for a different
            --  block.
            if not Is_Matching_Used_Cache_Entry
                     (System_Block_Cache,
                      Cache_Index,
                      Filesystem,
                      Block_Number)
            then
               Log_Error
                 ("Cache entry found but no longer valid. Retrying...",
                  Logging_Tags_Block_Cache);

               Release_Sleeplock
                 (System_Block_Cache.Entries (Cache_Index).Sleeplock);
               Release_Spinlock (System_Block_Cache.Spinlock);
            else
               Release_Spinlock (System_Block_Cache.Spinlock);
               exit Read_From_Cache_Loop;
            end if;
         elsif Result = Cache_Entry_Not_Found then
            Log_Debug
              ("Block not found in cache; Reading from filesystem...",
               Logging_Tags_Block_Cache);

            --  If the block isn't already in the cache, allocate a new cache
            --  entry, then read the data from the filesystem into that entry.
            Find_And_Claim_Available_Block_Cache_Entry (Cache_Index, Result);
            if Is_Error (Result) then
               Release_Spinlock (System_Block_Cache.Spinlock);
               return;
            end if;

            Log_Debug
              ("Allocating block cache entry at index: " & Cache_Index'Image,
               Logging_Tags_Block_Cache);

            --  Claim the cache entry prior to acquiring the sleeplock, to
            --  ensure there's absolutely no window for another process to
            --  claim the same  cache entry.
            System_Block_Cache.Entries (Cache_Index).Filesystem := Filesystem;
            System_Block_Cache.Entries (Cache_Index).Block_Number :=
              Block_Number;

            --  Acquire the cache entry's sleeplock.
            --  In this case, it's unlikely the newly allocated block will be
            --  in use by another process.
            --  This needs to happen here, under the cache lock, before reading
            --  the block from the filesystem into the cache entry, to ensure
            --  that no other process can claim the same cache entry.
            Acquire_Sleeplock
              (System_Block_Cache.Entries (Cache_Index).Sleeplock,
               System_Block_Cache.Spinlock,
               Reading_Process.Process_Id);

            Release_Spinlock (System_Block_Cache.Spinlock);

            Read_Block_From_Filesystem_Into_Cache_Entry
              (System_Block_Cache,
               Filesystem,
               Reading_Process,
               Block_Number,
               Cache_Index,
               Result);
            if Is_Error (Result) then
               --  In the case that reading the block from the filesystem
               --  failed, release the cache entry we allocated.

               --  Save the original error result.
               Error_Result : constant Function_Result := Result;

               --  Specifically invalidate the invalid cache entry, so that the
               --  invalid entry isn't picked up by another process.
               Release_Block (Filesystem, Block_Number, Result, True);

               Result := Error_Result;

               return;
            end if;

            exit Read_From_Cache_Loop;
         end if;

         Read_Block_Retry_Count := Read_Block_Retry_Count + 1;
         if Read_Block_Retry_Count > Read_Block_Retry_Threshold then
            Log_Error
              ("Exceeded retry threshold trying to read block.",
               Logging_Tags_Block_Cache);
            Result := Unhandled_Exception;
            return;
         end if;
      end loop Read_From_Cache_Loop;

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
         Log_Error ("Constraint_Error: Read_Block_From_Filesystem");
         Result := Constraint_Exception;
   end Read_Block_From_Filesystem;

   procedure Read_Sector_From_Filesystem
     (Filesystem           : Filesystem_Access;
      Reading_Process      : in out Process_Control_Block_T;
      Sector_Number        : Sector_Index_T;
      Sector_Size          : Natural;
      Data_Virtual_Address : out Virtual_Address_T;
      Result               : out Function_Result)
   is
      Cache_Entry_Addr_Virtual : Virtual_Address_T := Null_Address;
   begin
      Read_Block_From_Filesystem
        (Filesystem,
         Reading_Process,
         Sector_To_Block (Sector_Number, Sector_Size),
         Cache_Entry_Addr_Virtual,
         Result);
      if Is_Error (Result) then
         return;
      end if;

      Data_Virtual_Address :=
        Cache_Entry_Addr_Virtual
        + Get_Sector_Offset_Within_Block (Sector_Number, Sector_Size);
      Result := Success;
   end Read_Sector_From_Filesystem;

   procedure Read_Block_From_Filesystem_Into_Cache_Entry
     (Cache           : in out Block_Cache_T;
      Filesystem      : Filesystem_Access;
      Reading_Process : in out Process_Control_Block_T;
      Block_Number    : Block_Index_T;
      Cache_Index     : Positive;
      Result          : out Function_Result)
   is
      Current_Sector             : Sector_Index_T := 0;
      Current_Read_Addr_Virtual  : Virtual_Address_T := Null_Address;
      Current_Read_Addr_Physical : Physical_Address_T := Null_Physical_Address;

      --  @TODO: This currently assumes 512-byte sectors on all devices.
      Sector_Size : constant := 512;
   begin
      Get_Block_Cache_Entry_Data_Address
        (Cache,
         Cache_Index,
         Current_Read_Addr_Virtual,
         Current_Read_Addr_Physical,
         Result);
      if Is_Error (Result) then
         return;
      end if;

      Sectors_Per_Block : constant Natural :=
        Get_Sectors_Per_Block (Sector_Size);

      Current_Sector := (Block_Number * Block_Size) / Sector_Size;

      for I in 0 .. Sectors_Per_Block - 1 loop
         Log_Debug
           ("Reading sector into block cache: " & Current_Sector'Image,
            Logging_Tags_Block_Cache);
         case Filesystem.all.Device.all.Device_Bus is
            when Device_Bus_Virtio_MMIO   =>
               Devices.Virtio.Block.Read_Sector
                 (Reading_Process,
                  Filesystem.all.Device.all,
                  Current_Read_Addr_Physical,
                  Current_Sector,
                  Result);

            when Device_Bus_Memory_Mapped =>
               Devices.Ramdisk.Read_Sector
                 (Filesystem.all.Device.all,
                  Current_Sector,
                  Current_Read_Addr_Virtual,
                  Result);
         end case;

         if Is_Error (Result) then
            return;
         end if;

         Current_Sector := Current_Sector + 1;

         Current_Read_Addr_Virtual :=
           Current_Read_Addr_Virtual + Storage_Offset (Sector_Size);
         Current_Read_Addr_Physical :=
           Current_Read_Addr_Physical + Storage_Offset (Sector_Size);
      end loop;

      Log_Debug
        ("Added new entry in block cache for block number: "
         & Block_Number'Image,
         Logging_Tags_Block_Cache);

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error
           ("Constraint_Error: Read_Block_From_Filesystem_Into_Cache_Entry");
         Result := Constraint_Exception;
   end Read_Block_From_Filesystem_Into_Cache_Entry;

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

      Log_Debug
        ("Releasing block: " & Block_Number'Image, Logging_Tags_Block_Cache);

      Find_Existing_Block_In_Cache
        (System_Block_Cache, Filesystem, Block_Number, Cache_Index, Result);
      if Is_Error (Result) then
         return;
      elsif Result = Cache_Entry_Not_Found then
         Log_Error ("Block to release not found in cache.");
         Result := Invalid_Argument;
         return;
      end if;

      Log_Debug ("Found block to release in cache.", Logging_Tags_Block_Cache);

      --  Under some circumstances, such as when reading data into the cache
      --  entry fails, we may want to invalidate the cache entry before
      --  releasing it, to ensure it's not used by another process in an
      --  invalid state.
      if Invalidate_Cache_Entry then
         System_Block_Cache.Entries (Cache_Index).Entry_Used := False;
         System_Block_Cache.Entries (Cache_Index).Filesystem := null;
         System_Block_Cache.Entries (Cache_Index).Block_Number := 0;
      end if;

      Release_Sleeplock (System_Block_Cache.Entries (Cache_Index).Sleeplock);

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Release_Block_Unlocked");
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

   procedure Release_Sector
     (Filesystem    : Filesystem_Access;
      Sector_Number : Sector_Index_T;
      Sector_Size   : Natural;
      Result        : out Function_Result) is
   begin
      Release_Block
        (Filesystem, Sector_To_Block (Sector_Number, Sector_Size), Result);
   end Release_Sector;

   procedure Write_Block_From_Cache_Entry_To_Filesystem
     (Cache           : in out Block_Cache_T;
      Filesystem      : Filesystem_Access;
      Writing_Process : in out Process_Control_Block_T;
      Block_Number    : Block_Index_T;
      Cache_Index     : Positive;
      Result          : out Function_Result)
   is
      Current_Sector : Sector_Index_T := 0;

      Curr_Write_Addr_Virtual  : Virtual_Address_T := Null_Address;
      Curr_Write_Addr_Physical : Physical_Address_T := Null_Physical_Address;

      --  @TODO: This currently assumes 512-byte sectors on all devices.
      Sector_Size : constant := 512;
   begin
      Get_Block_Cache_Entry_Data_Address
        (Cache,
         Cache_Index,
         Curr_Write_Addr_Virtual,
         Curr_Write_Addr_Physical,
         Result);
      if Is_Error (Result) then
         return;
      end if;

      Sectors_Per_Block : constant Natural :=
        Get_Sectors_Per_Block (Sector_Size);

      Current_Sector := (Block_Number * Block_Size) / Sector_Size;

      for I in 0 .. Sectors_Per_Block - 1 loop
         Log_Debug
           ("Writing sector from block cache: " & Current_Sector'Image,
            Logging_Tags_Block_Cache);
         case Filesystem.all.Device.all.Device_Bus is
            when Device_Bus_Virtio_MMIO   =>
               Devices.Virtio.Block.Write_Sector
                 (Writing_Process,
                  Filesystem.all.Device.all,
                  Curr_Write_Addr_Physical,
                  Current_Sector,
                  Result);

            when Device_Bus_Memory_Mapped =>
               Devices.Ramdisk.Write_Sector
                 (Filesystem.all.Device.all,
                  Current_Sector,
                  Curr_Write_Addr_Virtual,
                  Result);
         end case;

         if Is_Error (Result) then
            return;
         end if;

         Current_Sector := Current_Sector + 1;

         Curr_Write_Addr_Virtual :=
           Curr_Write_Addr_Virtual + Storage_Offset (Sector_Size);
         Curr_Write_Addr_Physical :=
           Curr_Write_Addr_Physical + Storage_Offset (Sector_Size);
      end loop;

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error
           ("Constraint_Error: Write_Block_From_Cache_Entry_To_Filesystem");
         Result := Constraint_Exception;
   end Write_Block_From_Cache_Entry_To_Filesystem;

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

      Log_Debug
        ("Write_Block_To_Filesystem: " & Block_Number'Image,
         Logging_Tags_Block_Cache);

      --  Acquire the block cache spinlock prior to searching the cache.
      --  We need to ensure the block we're looking for isn't being modified
      --  by another process while we're searching.
      Acquire_Spinlock (System_Block_Cache.Spinlock);

      Find_Existing_Block_In_Cache
        (System_Block_Cache, Filesystem, Block_Number, Cache_Index, Result);

      Release_Spinlock (System_Block_Cache.Spinlock);

      if Is_Error (Result) then
         return;
      elsif Result = Cache_Entry_Not_Found then
         Log_Error
           ("Block to write not found in cache.", Logging_Tags_Block_Cache);
         Result := Invalid_Argument;
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

      Write_Block_From_Cache_Entry_To_Filesystem
        (System_Block_Cache,
         Filesystem,
         Writing_Process,
         Block_Number,
         Cache_Index,
         Result);
      if Is_Error (Result) then
         return;
      end if;

   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Write_Block_To_Filesystem");
         Result := Constraint_Exception;
   end Write_Block_To_Filesystem;

   procedure Write_Sector_To_Filesystem
     (Filesystem      : Filesystem_Access;
      Writing_Process : in out Process_Control_Block_T;
      Sector_Number   : Sector_Index_T;
      Sector_Size     : Natural;
      Result          : out Function_Result) is
   begin
      Write_Block_To_Filesystem
        (Filesystem,
         Writing_Process,
         Sector_To_Block (Sector_Number, Sector_Size),
         Result);
   end Write_Sector_To_Filesystem;

end Filesystems.Block_Cache;
