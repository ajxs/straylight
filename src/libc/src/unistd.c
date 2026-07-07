#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <straylight_syscall.h>
#include <string.h>
#include <unistd.h>

// https://pubs.opengroup.org/onlinepubs/9799919799/functions/close.html
int close(int fd)
{
	int64_t result =
	    straylight_libc_do_syscall(STRAYLIGHT_SYSCALL_FILE_CLOSE, fd);
	if (is_syscall_result_error(result))
	{
		errno = -result;
		return -1;
	}

	return 0;
}

ssize_t read(int fildes, void *buf, size_t nbyte)
{
	int64_t result = straylight_libc_do_syscall(STRAYLIGHT_SYSCALL_FILE_READ,
	                                            fildes, buf, nbyte);
	if (is_syscall_result_error(result))
	{
		errno = -result;
		return -1;
	}

	return result;
}
