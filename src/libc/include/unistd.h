#ifndef STRAYLIGHT_LIBC_UNISTD_H
#define STRAYLIGHT_LIBC_UNISTD_H 1

#include <sys/types.h>

int close(int fd);

ssize_t read(int fildes, void *buf, size_t nbyte);

int truncate(const char *path, off_t length);

int ftruncate(int fd, off_t length);

#endif
