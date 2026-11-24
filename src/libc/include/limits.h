#ifndef STRAYLIGHT_LIBC_LIMITS_H
#define STRAYLIGHT_LIBC_LIMITS_H 1

// The name of this file clashes with GCC's own limits.h.
// This file exists to export our own Straylight-specific definitions of POSIX
// pathname limits in their standard location (limits.h).
// In order to make the two work together, this file includes GCC's limits.h
// using #include_next, which instructs the compiler to keep searching from the
// next location, effectively appending GCC's limits.h to this file.
#pragma GCC system_header
#include_next <limits.h>

// The longest filename, in bytes, not counting the terminator.
#define NAME_MAX 256

// The longest path, in bytes, counting the terminator. The kernel's limit for
// a path is 256 bytes, without one.
#define PATH_MAX 257

#endif
