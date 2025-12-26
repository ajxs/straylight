-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Locks;            use Locks;
with Locks.Sleeplocks; use Locks.Sleeplocks;

private package Filesystems.Block_Cache is
   pragma Preelaborate;

   procedure Read_Block_From_Filesystem
     (Filesystem           : Filesystem_Access;
      Reading_Process      : in out Process_Control_Block_T;
      Block_Number         : Unsigned_64;
      Data_Virtual_Address : out Virtual_Address_T;
      Result               : out Function_Result);

   procedure Release_Block
     (Filesystem   : Filesystem_Access;
      Block_Number : Unsigned_64;
      Result       : out Function_Result);

   ----------------------------------------------------------------------------
   --  These procedures are functionally identical to the block read/release
   --  procedures above, but allow users to reason about filesystem operations
   --  in terms of sectors rather than blocks.
   --  Many filesystems reason internally in terms of sectors, so these methods
   --  provide a more convenient interface for those filesystems.
   --  The underlying implementation still operates in terms of blocks. e.g.
   --  If you read sector 1, with size 512 bytes, block 0 (of 4KiB) will still
   --  be read from the filesystem into the cache.
   ----------------------------------------------------------------------------
   procedure Read_Sector_From_Filesystem
     (Filesystem           : Filesystem_Access;
      Reading_Process      : in out Process_Control_Block_T;
      Sector_Number        : Unsigned_64;
      Sector_Size          : Natural;
      Data_Virtual_Address : out Virtual_Address_T;
      Result               : out Function_Result);

   procedure Release_Sector
     (Filesystem    : Filesystem_Access;
      Sector_Number : Unsigned_64;
      Sector_Size   : Natural;
      Result        : out Function_Result);

   --  The block cache entries don't require an individual spinlock.
   --  The block cache as a whole is protected by a single spinlock. No
   --  individual entry can be modified without holding the main block cache
   --  spinlock.
   type Block_Cache_Entry_T is record
      Sleeplock    : Sleeplock_T;
      Filesystem   : Filesystem_Access := null;
      Block_Number : Unsigned_64 := 0;
      Last_Access  : Unsigned_64 := 0;
      Entry_Used   : Boolean := False;
   end record;

   type Block_Cache_Entry_Array_T is array (1 .. 64) of Block_Cache_Entry_T;

   type Block_Cache_T is record
      Spinlock              : Spinlock_T;
      Data_Address_Virtual  : Virtual_Address_T;
      Data_Address_Physical : Physical_Address_T;
      Entries               : Block_Cache_Entry_Array_T;
   end record;

   System_Block_Cache : Block_Cache_T;

private
   Logging_Tags_Block_Cache : constant Log_Tags :=
     [Log_Tag_Filesystems_Block_Cache];

   Cache_Entry_Age_Threshold : constant Unsigned_64 := 1000000;

   procedure Find_Available_Block_Cache_Entry
     (Cache  : Block_Cache_T;
      Index  : out Positive;
      Result : out Function_Result);

   procedure Get_Block_Cache_Entry_Data_Address
     (Cache                 : Block_Cache_T;
      Cache_Index           : Positive;
      Data_Address_Virtual  : out Virtual_Address_T;
      Data_Address_Physical : out Physical_Address_T;
      Result                : out Function_Result);

   function Can_Block_Cache_Entry_Be_Invalidated
     (Cache        : Block_Cache_T;
      Cache_Index  : Positive;
      Current_Time : Unsigned_64) return Boolean;

   procedure Find_Existing_Block_In_Cache
     (Cache        : Block_Cache_T;
      Filesystem   : Filesystem_Access;
      Block_Number : Unsigned_64;
      Cache_Index  : out Positive;
      Result       : out Function_Result);

   procedure Read_Block_From_Filesystem_Into_Cache_Entry
     (Cache           : in out Block_Cache_T;
      Filesystem      : Filesystem_Access;
      Reading_Process : in out Process_Control_Block_T;
      Block_Number    : Unsigned_64;
      Cache_Index     : Positive;
      Result          : out Function_Result);

   ----------------------------------------------------------------------------
   --  The following methods are the 'unlocked' versions of the above methods
   --  which are called once the spinlock has been acquired.
   --  These functions are only called from the 'locked' versions above.
   --  They are structured this way so that all happy/unhappy paths all lead to
   --  the same exit point, making it easier to ensure the spinlock is always
   --  released.
   ----------------------------------------------------------------------------
   procedure Release_Block_Unlocked
     (Filesystem   : Filesystem_Access;
      Block_Number : Unsigned_64;
      Result       : out Function_Result);

end Filesystems.Block_Cache;
