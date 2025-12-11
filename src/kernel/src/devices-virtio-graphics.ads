-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

package Devices.VirtIO.Graphics is
   pragma Preelaborate;

   procedure Created_2d_Resource
     (Reading_Process : in out Process_Control_Block_T;
      Device          : in out Device_T;
      Resource_Id     : Unsigned_32;
      Result          : out Function_Result);

   procedure Get_Display_Info
     (Reading_Process : in out Process_Control_Block_T;
      Device          : in out Device_T;
      Result          : out Function_Result);

   procedure Attach_Framebuffer_To_Resource
     (Reading_Process              : in out Process_Control_Block_T;
      Device                       : in out Device_T;
      Resource_Id                  : Unsigned_32;
      Framebuffer_Physical_Address : Physical_Address_T;
      Result                       : out Function_Result);

   procedure Set_Scanout
     (Reading_Process : in out Process_Control_Block_T;
      Device          : in out Device_T;
      Resource_Id     : Unsigned_32;
      Scanout_Id      : Unsigned_32;
      Width           : Unsigned_32;
      Height          : Unsigned_32;
      Result          : out Function_Result);

   procedure Transfer_To_Host_2d
     (Reading_Process : in out Process_Control_Block_T;
      Device          : in out Device_T;
      Resource_Id     : Unsigned_32;
      X               : Unsigned_32;
      Y               : Unsigned_32;
      Width           : Unsigned_32;
      Height          : Unsigned_32;
      Result          : out Function_Result);

   procedure Resource_Flush
     (Reading_Process : in out Process_Control_Block_T;
      Device          : in out Device_T;
      Resource_Id     : Unsigned_32;
      X               : Unsigned_32;
      Y               : Unsigned_32;
      Width           : Unsigned_32;
      Height          : Unsigned_32;
      Result          : out Function_Result);

private
   procedure Attach_Framebuffer_To_Resource_Unlocked
     (Reading_Process              : in out Process_Control_Block_T;
      Device                       : in out Device_T;
      Resource_Id                  : Unsigned_32;
      Framebuffer_Physical_Address : Physical_Address_T;
      Result                       : out Function_Result);

   procedure Created_2d_Resource_Unlocked
     (Reading_Process : in out Process_Control_Block_T;
      Device          : in out Device_T;
      Resource_Id     : Unsigned_32;
      Result          : out Function_Result);

   procedure Get_Display_Info_Unlocked
     (Reading_Process : in out Process_Control_Block_T;
      Device          : in out Device_T;
      Result          : out Function_Result);

   procedure Set_Scanout_Unlocked
     (Reading_Process : in out Process_Control_Block_T;
      Device          : in out Device_T;
      Resource_Id     : Unsigned_32;
      Scanout_Id      : Unsigned_32;
      Width           : Unsigned_32;
      Height          : Unsigned_32;
      Result          : out Function_Result);

   procedure Transfer_To_Host_2d_Unlocked
     (Reading_Process : in out Process_Control_Block_T;
      Device          : in out Device_T;
      Resource_Id     : Unsigned_32;
      X               : Unsigned_32;
      Y               : Unsigned_32;
      Width           : Unsigned_32;
      Height          : Unsigned_32;
      Result          : out Function_Result);

   procedure Resource_Flush_Unlocked
     (Reading_Process : in out Process_Control_Block_T;
      Device          : in out Device_T;
      Resource_Id     : Unsigned_32;
      X               : Unsigned_32;
      Y               : Unsigned_32;
      Width           : Unsigned_32;
      Height          : Unsigned_32;
      Result          : out Function_Result);

   Device_Control_Queue_Index : constant := 0;
   Device_Cursor_Queue_Index  : constant := 1;

   Logging_Tags_VirtIO_Graphics : constant Log_Tags :=
     [Log_Tag_Devices,
      Log_Tag_Devices_VirtIO,
      Log_Tag_Devices_VirtIO_Graphics];

   type Virtio_Gpu_Format is
     (VIRTIO_GPU_FORMAT_B8G8R8A8_UNORM,
      VIRTIO_GPU_FORMAT_B8G8R8X8_UNORM,
      VIRTIO_GPU_FORMAT_A8R8G8B8_UNORM,
      VIRTIO_GPU_FORMAT_X8R8G8B8_UNORM,
      VIRTIO_GPU_FORMAT_R8G8B8A8_UNORM,
      VIRTIO_GPU_FORMAT_X8B8G8R8_UNORM,
      VIRTIO_GPU_FORMAT_A8B8G8R8_UNORM,
      VIRTIO_GPU_FORMAT_R8G8B8X8_UNORM)
   with Size => 32;
   for Virtio_Gpu_Format use
     (VIRTIO_GPU_FORMAT_B8G8R8A8_UNORM => 1,
      VIRTIO_GPU_FORMAT_B8G8R8X8_UNORM => 2,
      VIRTIO_GPU_FORMAT_A8R8G8B8_UNORM => 3,
      VIRTIO_GPU_FORMAT_X8R8G8B8_UNORM => 4,
      VIRTIO_GPU_FORMAT_R8G8B8A8_UNORM => 67,
      VIRTIO_GPU_FORMAT_X8B8G8R8_UNORM => 68,
      VIRTIO_GPU_FORMAT_A8B8G8R8_UNORM => 121,
      VIRTIO_GPU_FORMAT_R8G8B8X8_UNORM => 134);

   --  These match section 5.7 of the VirtIO spec.
   type Gpu_Ctrl_Type is
     (Cmd_Get_Display_Info,
      Cmd_Resource_Create_2D,
      Cmd_Resource_Unref,
      Cmd_Set_Scanout,
      Cmd_Resource_Flush,
      Cmd_Transfer_To_Host_2D,
      Cmd_Resource_Attach_Backing,
      Cmd_Resource_Detach_Backing,
      Cmd_Get_Capset_Info,
      Cmd_Get_Capset,
      Cmd_Get_Edid,

      --  Cursor Commands
      Cmd_Update_Cursor,
      Cmd_Move_Cursor,

      --  Success Responses
      Resp_Ok_NoData,
      Resp_Ok_Display_Info,
      Resp_Ok_Capset_Info,
      Resp_Ok_Capset,
      Resp_Ok_Edid,

      --  Error Responses
      Resp_Err_Unspec,
      Resp_Err_Out_Of_Memory,
      Resp_Err_Invalid_Scanout_Id,
      Resp_Err_Invalid_Resource_Id,
      Resp_Err_Invalid_Context_Id,
      Resp_Err_Invalid_Parameter)
   with Size => 32;
   for Gpu_Ctrl_Type use
     (Cmd_Get_Display_Info         => 16#0100#,
      Cmd_Resource_Create_2D       => 16#0101#,
      Cmd_Resource_Unref           => 16#0102#,
      Cmd_Set_Scanout              => 16#0103#,
      Cmd_Resource_Flush           => 16#0104#,
      Cmd_Transfer_To_Host_2D      => 16#0105#,
      Cmd_Resource_Attach_Backing  => 16#0106#,
      Cmd_Resource_Detach_Backing  => 16#0107#,
      Cmd_Get_Capset_Info          => 16#0108#,
      Cmd_Get_Capset               => 16#0109#,
      Cmd_Get_Edid                 => 16#010A#,

      Cmd_Update_Cursor            => 16#0300#,
      Cmd_Move_Cursor              => 16#0301#,

      Resp_Ok_NoData               => 16#1100#,
      Resp_Ok_Display_Info         => 16#1101#,
      Resp_Ok_Capset_Info          => 16#1102#,
      Resp_Ok_Capset               => 16#1103#,
      Resp_Ok_Edid                 => 16#1104#,

      Resp_Err_Unspec              => 16#1200#,
      Resp_Err_Out_Of_Memory       => 16#1201#,
      Resp_Err_Invalid_Scanout_Id  => 16#1202#,
      Resp_Err_Invalid_Resource_Id => 16#1203#,
      Resp_Err_Invalid_Context_Id  => 16#1204#,
      Resp_Err_Invalid_Parameter   => 16#1205#);

   VIRTIO_GPU_MAX_SCANOUTS : constant := 16;

   type Virtio_Gpu_Rect is record
      X      : Unsigned_32;
      Y      : Unsigned_32;
      Width  : Unsigned_32;
      Height : Unsigned_32;
   end record
   with Convention => C;

   type Virtio_Gpu_Ctrl_Hdr is record
      Ctrl_Type : Gpu_Ctrl_Type;  -- 32-bit enum per spec
      Flags     : Unsigned_32;
      Fence_Id  : Unsigned_64;
      Ctx_Id    : Unsigned_32;
      Padding   : Unsigned_32;
   end record
   with Convention => C;

   --  Per-scanout information (inner struct virtio_gpu_display_one)
   type Virtio_Gpu_Display_One is record
      R       : Virtio_Gpu_Rect;
      Enabled : Unsigned_32;
      Flags   : Unsigned_32;
   end record
   with Convention => C;

   --  struct virtio_gpu_resp_display_info
   type Virtio_Gpu_Display_Array is
     array (Natural range 0 .. VIRTIO_GPU_MAX_SCANOUTS - 1)
     of Virtio_Gpu_Display_One;

   type Virtio_Gpu_Resp_Display_Info is record
      Hdr    : Virtio_Gpu_Ctrl_Hdr;
      Pmodes : Virtio_Gpu_Display_Array;
   end record
   with Convention => C;

   type Virtio_Gpu_Mem_Entry is record
      Addr    : Unsigned_64;
      Length  : Unsigned_32;
      Padding : Unsigned_32;
   end record
   with Convention => C;

   type Request_Resource_Create_2D is record
      Hdr         : Virtio_Gpu_Ctrl_Hdr;
      Resource_Id : Unsigned_32;
      Format      : Virtio_Gpu_Format;
      Width       : Unsigned_32;
      Height      : Unsigned_32;
   end record
   with Convention => C;

   type Request_Resource_Attach_Backing is record
      Hdr         : Virtio_Gpu_Ctrl_Hdr;
      Resource_Id : Unsigned_32;
      Num_Entries : Unsigned_32;
   end record
   with Convention => C;

   type Request_Set_Scanout is record
      Hdr         : Virtio_Gpu_Ctrl_Hdr;
      R           : Virtio_Gpu_Rect;
      Scanout_Id  : Unsigned_32;
      Resource_Id : Unsigned_32;
   end record
   with Convention => C;

   --  Because this will be a more common request, it might be ideal to have a
   --  statically allocated structure for it. This is possible because the
   --  device is spinlocked.
   type Request_Transfer_To_Host_2D is record
      Hdr         : Virtio_Gpu_Ctrl_Hdr;
      R           : Virtio_Gpu_Rect;
      Offset      : Unsigned_64;
      Resource_Id : Unsigned_32;
      Padding     : Unsigned_32;
   end record
   with Convention => C;

   type Request_Resource_Flush is record
      Hdr         : Virtio_Gpu_Ctrl_Hdr;
      R           : Virtio_Gpu_Rect;
      Resource_Id : Unsigned_32;
      Padding     : Unsigned_32;
   end record
   with Convention => C;

end Devices.VirtIO.Graphics;
