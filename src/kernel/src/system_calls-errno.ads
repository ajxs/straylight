-------------------------------------------------------------------------------
--  Copyright (c) 2025, Ajxs.
--  SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------

with Ada.Unchecked_Conversion;
with Interfaces;

package System_Calls.Errno
  with Preelaborate
is
   type Syscall_Error_Result_T is new Integer with Convention => C, Size => 64;

   --  Argument list too long.
   E2BIG           : constant Syscall_Error_Result_T := 1;
   --  Permission denied.
   EACCES          : constant Syscall_Error_Result_T := 2;
   --  Address in use.
   EADDRINUSE      : constant Syscall_Error_Result_T := 3;
   --  Address not available.
   EADDRNOTAVAIL   : constant Syscall_Error_Result_T := 4;
   --  Address family not supported.
   EAFNOSUPPORT    : constant Syscall_Error_Result_T := 5;
   --  Resource unavailable, try again (may be the same value as
   --  EWOULDBLOCK).
   EAGAIN          : constant Syscall_Error_Result_T := 6;
   --  Connection already in progress.
   EALREADY        : constant Syscall_Error_Result_T := 7;
   --  Bad file descriptor.
   EBADF           : constant Syscall_Error_Result_T := 8;
   --  Bad message.
   EBADMSG         : constant Syscall_Error_Result_T := 9;
   --  Device or resource busy.
   EBUSY           : constant Syscall_Error_Result_T := 10;
   --  Operation canceled.
   ECANCELED       : constant Syscall_Error_Result_T := 11;
   --  No child processes.
   ECHILD          : constant Syscall_Error_Result_T := 12;
   --  Connection aborted.
   ECONNABORTED    : constant Syscall_Error_Result_T := 13;
   --  Connection refused.
   ECONNREFUSED    : constant Syscall_Error_Result_T := 14;
   --  Connection reset.
   ECONNRESET      : constant Syscall_Error_Result_T := 15;
   --  Resource deadlock would occur.
   EDEADLK         : constant Syscall_Error_Result_T := 16;
   --  Destination address required.
   EDESTADDRREQ    : constant Syscall_Error_Result_T := 17;
   --  Mathematics argument out of domain of function.
   EDOM            : constant Syscall_Error_Result_T := 18;
   --  Reserved.
   EDQUOT          : constant Syscall_Error_Result_T := 19;
   --  File exists.
   EEXIST          : constant Syscall_Error_Result_T := 20;
   --  Bad address.
   EFAULT          : constant Syscall_Error_Result_T := 21;
   --  File too large.
   EFBIG           : constant Syscall_Error_Result_T := 22;
   --  Host is unreachable.
   EHOSTUNREACH    : constant Syscall_Error_Result_T := 23;
   --  Identifier removed.
   EIDRM           : constant Syscall_Error_Result_T := 24;
   --  Illegal byte sequence.
   EILSEQ          : constant Syscall_Error_Result_T := 25;
   --  Operation in progress.
   EINPROGRESS     : constant Syscall_Error_Result_T := 26;
   --  Interrupted function.
   EINTR           : constant Syscall_Error_Result_T := 27;
   --  Invalid argument.
   EINVAL          : constant Syscall_Error_Result_T := 28;
   --  I/O error.
   EIO             : constant Syscall_Error_Result_T := 29;
   --  Socket is connected.
   EISCONN         : constant Syscall_Error_Result_T := 30;
   --  Is a directory.
   EISDIR          : constant Syscall_Error_Result_T := 31;
   --  Too many levels of symbolic links.
   ELOOP           : constant Syscall_Error_Result_T := 32;
   --  File descriptor value too large.
   EMFILE          : constant Syscall_Error_Result_T := 33;
   --  Too many links.
   EMLINK          : constant Syscall_Error_Result_T := 34;
   --  Message too large.
   EMSGSIZE        : constant Syscall_Error_Result_T := 35;
   --  Reserved.
   EMULTIHOP       : constant Syscall_Error_Result_T := 36;
   --  Filename too long.
   ENAMETOOLONG    : constant Syscall_Error_Result_T := 37;
   --  Network is down.
   ENETDOWN        : constant Syscall_Error_Result_T := 38;
   --  Connection aborted by network.
   ENETRESET       : constant Syscall_Error_Result_T := 39;
   --  Network unreachable.
   ENETUNREACH     : constant Syscall_Error_Result_T := 40;
   --  Too many files open in system.
   ENFILE          : constant Syscall_Error_Result_T := 41;
   --  No buffer space available.
   ENOBUFS         : constant Syscall_Error_Result_T := 42;
   --  No message is available on the STREAM head read queue.
   ENODATA         : constant Syscall_Error_Result_T := 43;
   --  No such device.
   ENODEV          : constant Syscall_Error_Result_T := 44;
   --  No such file or directory.
   ENOENT          : constant Syscall_Error_Result_T := 45;
   --  Executable file format error.
   ENOEXEC         : constant Syscall_Error_Result_T := 46;
   --  No locks available.
   ENOLCK          : constant Syscall_Error_Result_T := 47;
   --  Reserved.
   ENOLINK         : constant Syscall_Error_Result_T := 48;
   --  Not enough space.
   ENOMEM          : constant Syscall_Error_Result_T := 49;
   --  No message of the desired type.
   ENOMSG          : constant Syscall_Error_Result_T := 50;
   --  Protocol not available.
   ENOPROTOOPT     : constant Syscall_Error_Result_T := 51;
   --  No space left on device.
   ENOSPC          : constant Syscall_Error_Result_T := 52;
   --  No STREAM resources.
   ENOSR           : constant Syscall_Error_Result_T := 53;
   --  Not a STREAM.
   ENOSTR          : constant Syscall_Error_Result_T := 54;
   --  Function not supported.
   ENOSYS          : constant Syscall_Error_Result_T := 55;
   --  The socket is not connected.
   ENOTCONN        : constant Syscall_Error_Result_T := 56;
   --  Not a directory or a symbolic link to a directory.
   ENOTDIR         : constant Syscall_Error_Result_T := 57;
   --  Directory not empty.
   ENOTEMPTY       : constant Syscall_Error_Result_T := 58;
   --  State not recoverable.
   ENOTRECOVERABLE : constant Syscall_Error_Result_T := 59;
   --  Not a socket.
   ENOTSOCK        : constant Syscall_Error_Result_T := 60;
   --  Not supported (may be the same value as EOPNOTSUPP).
   ENOTSUP         : constant Syscall_Error_Result_T := 61;
   --  Inappropriate I/O control operation.
   ENOTTY          : constant Syscall_Error_Result_T := 62;
   --  No such device or address.
   ENXIO           : constant Syscall_Error_Result_T := 63;
   --  Operation not supported on socket (may be the same value as
   --  ENOTSUP).
   EOPNOTSUPP      : constant Syscall_Error_Result_T := 64;
   --  Value too large to be stored in data type.
   EOVERFLOW       : constant Syscall_Error_Result_T := 65;
   --  Previous owner died.
   EOWNERDEAD      : constant Syscall_Error_Result_T := 66;
   --  Operation not permitted.
   EPERM           : constant Syscall_Error_Result_T := 67;
   --  Broken pipe.
   EPIPE           : constant Syscall_Error_Result_T := 68;
   --  Protocol error.
   EPROTO          : constant Syscall_Error_Result_T := 69;
   --  Protocol not supported.
   EPROTONOSUPPORT : constant Syscall_Error_Result_T := 70;
   --  Protocol wrong type for socket.
   EPROTOTYPE      : constant Syscall_Error_Result_T := 71;
   --  Result too large.
   ERANGE          : constant Syscall_Error_Result_T := 72;
   --  Read-only file system.
   EROFS           : constant Syscall_Error_Result_T := 73;
   --  Invalid seek.
   ESPIPE          : constant Syscall_Error_Result_T := 74;
   --  No such process.
   ESRCH           : constant Syscall_Error_Result_T := 75;
   --  Reserved.
   ESTALE          : constant Syscall_Error_Result_T := 76;
   --  Stream ioctl() timeout.
   ETIME           : constant Syscall_Error_Result_T := 77;
   --  Connection timed out.
   ETIMEDOUT       : constant Syscall_Error_Result_T := 78;
   --  Text file busy.
   ETXTBSY         : constant Syscall_Error_Result_T := 79;
   --  Operation would block (may be the same value as EAGAIN).
   EWOULDBLOCK     : constant Syscall_Error_Result_T := 80;
   --  Cross-device link.
   EXDEV           : constant Syscall_Error_Result_T := 81;

   function Syscall_Error_Result_To_Unsigned_64 is new
     Ada.Unchecked_Conversion
       (Source => Syscall_Error_Result_T,
        Target => Interfaces.Unsigned_64);

end System_Calls.Errno;
