#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <straylight_syscall.h>
#include <string.h>
#include <unistd.h>

int truncate(const char *path, off_t length)
{
	const int open_flags = O_WRONLY;

	int fd = open((const char *)path, open_flags);
	if (fd == -1)
	{
		// errno already set by open.
		return -1;
	}

	int ftruncate_result = ftruncate(fd, length);
	if (ftruncate_result == -1)
	{
		close(fd);
		return -1;
	}

	return close(fd);
}

int ftruncate(int fd, off_t length)
{
	int64_t result =
	    straylight_libc_do_syscall(STRAYLIGHT_SYSCALL_FILE_TRUNCATE, fd, length);
	if (is_syscall_result_error(result))
	{
		errno = -result;

		return -1;
	}

	return 0;
}
