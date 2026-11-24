#ifndef STRAYLIGHT_LIBC_DIRENT_H
#define STRAYLIGHT_LIBC_DIRENT_H 1

#include <limits.h>

// The values of d_type. These are the letters that 'find -type' uses. They're
// also the kernel's values for a directory entry's type, which readdir copies
// straight into d_type. A mount point is reported as a directory.
#define DT_CHR 'c'
#define DT_DIR 'd'
#define DT_REG 'f'
#define DT_UNKNOWN '?'

struct dirent
{
	unsigned char d_type;
	char d_name[NAME_MAX + 1];
};

typedef struct straylight_libc_directory_stream DIR;

DIR *opendir(const char *dirname);

struct dirent *readdir(DIR *dirp);

int closedir(DIR *dirp);

#endif
