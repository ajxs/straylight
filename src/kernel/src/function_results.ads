-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

package Function_Results is
   pragma Pure;

   type Function_Result is new Integer;

   Unset                : constant := 0;
   --  Success is given an arbitrary value to increase the Hamming distance
   --  between it and other values.
   Success              : constant := 9999_9999;
   Unhandled_Exception  : constant := -1;
   Constraint_Exception : constant := -2;

   No_Free_Entries : constant := -1111_0000;
   Not_Found       : constant := -1111_0001;
   Not_Supported   : constant := -1111_0002;

   --  Memory function result codes.
   Address_Not_In_Heap                 : constant := -7777_0000;
   Invalid_Argument                    : constant := -7777_0003;
   Invalid_Non_Aligned_Address         : constant := -7777_0004;
   Invalid_Physical_Memory_Size        : constant := -7777_0006;
   Invalid_Table_Index                 : constant := -7777_0007;
   Heap_Exhausted                      : constant := -7777_0012;
   No_Block_Small_Enough               : constant := -7777_0014;
   No_Block_Large_Enough               : constant := -7777_0015;
   Memory_Block_Cannot_Be_Consolidated : constant := 7777_0019;
   Memory_Block_Not_Found              : constant := -7777_0020;
   Memory_Map_Array_Exhausted          : constant := -7777_0021;
   Memory_Map_Not_Present              : constant := -7777_0022;
   Region_Not_Free                     : constant := -7777_0023;
   Region_Not_Mapped                   : constant := -7777_0024;
   Region_Is_Overlapping               : constant := -7777_0027;
   Region_Array_Exhausted              : constant := -7777_0025;
   Virtual_Memory_Exhausted            : constant := -7777_0026;

   --  Process function result codes.
   Maximum_Process_Count_Reached : constant := -5555_0000;
   Process_Not_Found             : constant := -5555_0001;

   No_Remaining_VirtIO_Descriptors : constant := -3333_0000;

   --  Filesystem function result codes.
   File_Not_Found        : constant := 1777_0000;
   Cache_Exhausted       : constant := -1777_0001;
   Cache_Entry_Not_Found : constant := 1777_0002;
   Invalid_Filesystem    : constant := -1777_0003;

   function Is_Error (Result : Function_Result) return Boolean
   is (Result <= 0)
   with Pure_Function;

end Function_Results;
