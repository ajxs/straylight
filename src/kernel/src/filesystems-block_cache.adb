-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Devices.Ramdisk;
with Devices.Virtio.Block;
with RISCV;

package body Filesystems.Block_Cache is
   function Can_Block_Cache_Entry_Be_Invalidated
     (Cache        : Block_Cache_T;
      Cache_Index  : Positive;
      Current_Time : Unsigned_64) return Boolean is
   begin
      if Cache_Index > Cache.Entries'Last then
         return False;
      end if;

      return
        (Current_Time - Cache.Entries (Cache_Index).Last_Access)
        > Cache_Entry_Age_Threshold;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Can_Block_Cache_Entry_Be_Invalidated");
         return False;
   end Can_Block_Cache_Entry_Be_Invalidated;

   procedure Find_And_Claim_Available_Block_Cache_Entry_Unlocked
     (Cache       : in out Block_Cache_T;
      Cache_Index : out Positive;
      Result      : out Function_Result) is
   begin
      --  Search for an unused entry.
      for I in Cache.Entries'Range loop
         if not Cache.Entries (I).Entry_Used then
            Cache_Index := I;
            goto Claim_Entry;
         end if;
      end loop;

      --  No unused entries, search for an entry we can invalidate.
      for I in Cache.Entries'Range loop
         if Can_Block_Cache_Entry_Be_Invalidated
              (Cache, I, RISCV.Get_System_Time)
         then
            Cache_Index := I;
            goto Claim_Entry;
         end if;
      end loop;

      Result := Cache_Exhausted;
      return;

      <<Claim_Entry>>
      Cache.Entries (Cache_Index).Entry_Used := True;
      Cache.Entries (Cache_Index).Filesystem := null;
      Cache.Entries (Cache_Index).Block_Number := 0;
      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error
           ("Constraint_Error: "
            & "Find_And_Claim_Available_Block_Cache_Entry_Unlocked");
         Result := Constraint_Exception;
   end Find_And_Claim_Available_Block_Cache_Entry_Unlocked;

   procedure Find_And_Claim_Available_Block_Cache_Entry
     (Cache       : in out Block_Cache_T;
      Cache_Index : out Positive;
      Result      : out Function_Result) is
   begin
      Acquire_Spinlock (Cache.Spinlock);
      Find_And_Claim_Available_Block_Cache_Entry_Unlocked
        (Cache, Cache_Index, Result);
      Release_Spinlock (Cache.Spinlock);
   end Find_And_Claim_Available_Block_Cache_Entry;

   procedure Find_Existing_Block_In_Cache
     (Cache        : in out Block_Cache_T;
      Filesystem   : Filesystem_Access;
      Block_Number : Unsigned_64;
      Cache_Index  : out Positive;
      Result       : out Function_Result) is
   begin
      --  Acquire the block cache spinlock prior to searching the cache.
      --  We need to ensure the block we're looking for isn't being modified
      --  by another process while we're searching.
      Acquire_Spinlock (Cache.Spinlock);

      for I in Cache.Entries'Range loop
         if Cache.Entries (I).Entry_Used
           and then Cache.Entries (I).Filesystem = Filesystem
           and then Cache.Entries (I).Block_Number = Block_Number
         then
            Cache_Index := I;
            Result := Success;
            Release_Spinlock (Cache.Spinlock);
            return;
         end if;
      end loop;

      Release_Spinlock (Cache.Spinlock);

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
      Block_Number         : Unsigned_64;
      Data_Virtual_Address : out Virtual_Address_T;
      Result               : out Function_Result)
   is
      Cache_Index : Positive := 1;

      Cache_Entry_Address_Virtual  : Virtual_Address_T := Null_Address;
      Cache_Entry_Address_Physical : Physical_Address_T :=
        Null_Physical_Address;
   begin
      if not Is_Valid_Filesystem_Pointer (Filesystem) then
         Log_Error ("Read_Block_From_Filesystem: Unsupported filesystem");
         Result := Invalid_Argument;
         return;
      end if;

      Log_Debug
        ("Read_Block_From_Filesystem: " & Block_Number'Image,
         Logging_Tags_Block_Cache);

      Find_Existing_Block_In_Cache
        (System_Block_Cache, Filesystem, Block_Number, Cache_Index, Result);

      --  Two possible results: Success / Cache_Entry_Not_Found.
      if Result = Success then
         Log_Debug
           ("Found existing block in cache.", Logging_Tags_Block_Cache);

         --  Acquire the cache entry's sleeplock.
         --  This means that if this block is currently being used by another
         --  process, the current process will 'sleep' here until the other
         --  process is done with it.
         --  If free, the current process will acquire the lock and continue.
         Acquire_Sleeplock
           (System_Block_Cache.Entries (Cache_Index).Sleeplock,
            Reading_Process.Process_Id);
      elsif Result = Cache_Entry_Not_Found then
         Log_Debug
           ("Block not found in cache; Reading from filesystem...",
            Logging_Tags_Block_Cache);

         --  If the block isn't already in the cache, allocate a new cache
         --  entry, then read the data from the filesystem into that entry.
         Find_And_Claim_Available_Block_Cache_Entry
           (System_Block_Cache, Cache_Index, Result);
         if Is_Error (Result) then
            return;
         end if;

         --  Acquire the cache entry's sleeplock.
         --  In this case, it's unlikely the newly allocated block will be in
         --  use by another process.
         Acquire_Sleeplock
           (System_Block_Cache.Entries (Cache_Index).Sleeplock,
            Reading_Process.Process_Id);

         Acquire_Spinlock (System_Block_Cache.Spinlock);

         System_Block_Cache.Entries (Cache_Index).Filesystem := Filesystem;
         System_Block_Cache.Entries (Cache_Index).Block_Number := Block_Number;
         System_Block_Cache.Entries (Cache_Index).Last_Access :=
           RISCV.Get_System_Time;

         Release_Spinlock (System_Block_Cache.Spinlock);

         Read_Block_From_Filesystem_Into_Cache_Entry
           (System_Block_Cache,
            Filesystem,
            Reading_Process,
            Block_Number,
            Cache_Index,
            Result);
         if Is_Error (Result) then
            --  In the case that reading the block from the filesystem failed,
            --  release the cache entry we allocated.
            System_Block_Cache.Entries (Cache_Index).Entry_Used := False;

            return;
         end if;
      end if;

      Get_Block_Cache_Entry_Data_Address
        (System_Block_Cache,
         Cache_Index,
         Cache_Entry_Address_Virtual,
         Cache_Entry_Address_Physical,
         Result);
      if Is_Error (Result) then
         return;
      end if;

      Data_Virtual_Address := Cache_Entry_Address_Virtual;

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Read_Block_From_Filesystem");
         Result := Constraint_Exception;
   end Read_Block_From_Filesystem;

   procedure Read_Sector_From_Filesystem
     (Filesystem           : Filesystem_Access;
      Reading_Process      : in out Process_Control_Block_T;
      Sector_Number        : Unsigned_64;
      Sector_Size          : Natural;
      Data_Virtual_Address : out Virtual_Address_T;
      Result               : out Function_Result)
   is
      Cache_Entry_Address_Virtual : Virtual_Address_T := Null_Address;
   begin
      Read_Block_From_Filesystem
        (Filesystem,
         Reading_Process,
         Sector_To_Block (Sector_Number, Sector_Size),
         Cache_Entry_Address_Virtual,
         Result);
      if Is_Error (Result) then
         return;
      end if;

      Data_Virtual_Address :=
        Cache_Entry_Address_Virtual
        + Get_Sector_Offset_Within_Block (Sector_Number, Sector_Size);
      Result := Success;
   end Read_Sector_From_Filesystem;

   procedure Read_Block_From_Filesystem_Into_Cache_Entry
     (Cache           : in out Block_Cache_T;
      Filesystem      : Filesystem_Access;
      Reading_Process : in out Process_Control_Block_T;
      Block_Number    : Unsigned_64;
      Cache_Index     : Positive;
      Result          : out Function_Result)
   is
      Current_Sector             : Unsigned_64 := 0;
      Current_Read_Addr_Virtual  : Virtual_Address_T := Null_Address;
      Current_Read_Addr_Physical : Physical_Address_T := Null_Physical_Address;
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

      --  @TODO: This currently assumes 512-byte sectors on all devices.
      Current_Sector := (Block_Number * Block_Size) / 512;

      for I in 0 .. 7 loop
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

         Current_Sector := Current_Sector + 1;

         Current_Read_Addr_Virtual :=
           Current_Read_Addr_Virtual + Storage_Offset (512);
         Current_Read_Addr_Physical :=
           Current_Read_Addr_Physical + Storage_Offset (512);
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

   procedure Release_Block
     (Filesystem   : Filesystem_Access;
      Block_Number : Unsigned_64;
      Result       : out Function_Result)
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
      elsif Result = Success then
         Log_Debug
           ("Found block to release in cache.", Logging_Tags_Block_Cache);

         Release_Sleeplock
           (System_Block_Cache.Entries (Cache_Index).Sleeplock);
      elsif Result = Cache_Entry_Not_Found then
         Log_Error ("Block to release not found in cache.");
         Result := Invalid_Argument;
         return;
      end if;

      Result := Success;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint_Error: Release_Block_Unlocked");
         Result := Constraint_Exception;
   end Release_Block;

   procedure Release_Sector
     (Filesystem    : Filesystem_Access;
      Sector_Number : Unsigned_64;
      Sector_Size   : Natural;
      Result        : out Function_Result) is
   begin
      Release_Block
        (Filesystem, Sector_To_Block (Sector_Number, Sector_Size), Result);
   end Release_Sector;

end Filesystems.Block_Cache;
