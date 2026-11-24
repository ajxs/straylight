#include <errno.h>
#include <straylight_syscall.h>
#include <string.h>
#include <unistd.h>

// https://pubs.opengroup.org/onlinepubs/9799919799/functions/chdir.html
int chdir(const char *path)
{
	int64_t result = straylight_libc_do_syscall(
	    STRAYLIGHT_SYSCALL_CHANGE_WORKING_DIRECTORY, path, strlen(path));
	if (is_syscall_result_error(result))
	{
		errno = -result;
		return -1;
	}

	return 0;
}

// https://pubs.opengroup.org/onlinepubs/9799919799/functions/getcwd.html
char *getcwd(char *buf, size_t size)
{
	// The size counts the terminator, which the kernel doesn't write.
	if (size == 0)
	{
		errno = EINVAL;
		return NULL;
	}

	int64_t result = straylight_libc_do_syscall(
	    STRAYLIGHT_SYSCALL_GET_WORKING_DIRECTORY, buf, size - 1);
	if (is_syscall_result_error(result))
	{
		errno = -result;
		return NULL;
	}

	// Add the null terminator at the end of the string.
	buf[result] = '\0';

	return buf;
}
