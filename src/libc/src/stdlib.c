#include "heap.h"
#include "libc.h"
#include <errno.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <straylight_syscall.h>

void *aligned_alloc(size_t alignment, size_t size)
{
	const bool alignment_is_power_of_2 = (alignment & (alignment - 1)) == 0;

	if (alignment == 0 || !alignment_is_power_of_2)
	{
		errno = EINVAL;

		return NULL;
	}

	uintptr_t result = allocate_memory(&program_heap, size, alignment);
	if (result == (uintptr_t)NULL)
	{
		errno = ENOMEM;

		return NULL;
	}

	return (void *)result;
}

void exit(int exit_code)
{
	fclose(stdout);
	fclose(stderr);
	fclose(stdin);

	straylight_libc_do_syscall(STRAYLIGHT_SYSCALL_PROCESS_EXIT, exit_code);

	__builtin_unreachable();
}

void free(void *ptr) { free_memory(&program_heap, (uintptr_t)ptr); }

void *malloc(size_t size)
{
	uintptr_t result = allocate_memory(&program_heap, size, 1);
	if (result == (uintptr_t)NULL)
	{
		errno = ENOMEM;

		return NULL;
	}

	return (void *)result;
}
