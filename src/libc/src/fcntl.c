#include <errno.h>
#include <fcntl.h>
#include <stdarg.h>
#include <straylight_syscall.h>
#include <string.h>

int open(const char *pathname, int flags, ...)
{
	mode_t mode = 0;

	if (flags & O_CREAT)
	{
		va_list ap;
		va_start(ap, flags);
		mode = va_arg(ap, mode_t);
		va_end(ap);
	}

	int64_t result = straylight_libc_do_syscall(
	    STRAYLIGHT_SYSCALL_FILE_OPEN, pathname, strlen(pathname), flags, mode);
	if (is_syscall_result_error(result))
	{
		errno = -result;

		return -1;
	}

	return result;
}
