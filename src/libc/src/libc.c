#include "libc.h"
#include <errno.h>
#include <stdio.h>

Program_Heap program_heap;

void initialise_straylight_libc()
{
	errno = 0;

	initialise_heap(&program_heap, USERSPACE_HEAP_ADDRESS,
	                USERSPACE_HEAP_STARTING_SIZE);

	stdout = fopen("/Devices/Serial", "w");
	if (stdout == NULL)
	{
		// errno already set by fopen.
		exit(1);
	}

	stdout->buffering_mode = _IOLBF;

	stderr = fopen("/Devices/Serial", "w");
	if (stderr == NULL)
	{
		// errno already set by fopen.
		exit(1);
	}

	stderr->buffering_mode = _IONBF;
}
