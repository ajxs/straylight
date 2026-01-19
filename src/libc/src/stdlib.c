#include <errno.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <straylight_syscall.h>

void *malloc(size_t size)
{
	int64_t result =
	    straylight_libc_do_syscall(STRAYLIGHT_SYSCALL_ALLOCATE_MEMORY, size);
	if (is_syscall_result_error(result))
	{
		errno = result;

		return NULL;
	}

	return (void *)result;
}

void exit(int exit_code)
{
	straylight_libc_do_syscall(STRAYLIGHT_SYSCALL_PROCESS_EXIT, exit_code);

	__builtin_unreachable();
}
