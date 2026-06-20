#include <sys/types.h>

#ifndef STRAYLIGHT_LIBC_FCNTL_H
#define STRAYLIGHT_LIBC_FCNTL_H 1

/** File Access Flags */
#define FILE_ACCESS_FLAGS_OFFSET 0

#define O_RDONLY (0 << FILE_ACCESS_FLAGS_OFFSET)
#define O_WRONLY (1 << FILE_ACCESS_FLAGS_OFFSET)
#define O_RDWR (2 << FILE_ACCESS_FLAGS_OFFSET)
#define O_EXEC (3 << FILE_ACCESS_FLAGS_OFFSET)
#define O_SEARCH (4 << FILE_ACCESS_FLAGS_OFFSET)

#define O_ACCMODE (O_RDONLY | O_WRONLY | O_RDWR | O_EXEC | O_SEARCH)

/** File Creation Flags */
#define FILE_CREATION_FLAGS_OFFSET 3

#define O_CREAT ((1 << 0) << FILE_CREATION_FLAGS_OFFSET)
#define O_DIRECTORY ((1 << 1) << FILE_CREATION_FLAGS_OFFSET)
#define O_TRUNC ((1 << 2) << FILE_CREATION_FLAGS_OFFSET)

/** File Status Flags */
#define FILE_STATUS_FLAGS_OFFSET 6

#define O_APPEND ((1 << 0) << FILE_STATUS_FLAGS_OFFSET)
#define O_NONBLOCK ((1 << 1) << FILE_STATUS_FLAGS_OFFSET)
#define O_SYNC ((1 << 2) << FILE_STATUS_FLAGS_OFFSET)

int open(const char *pathname, int flags, ...);

#endif
