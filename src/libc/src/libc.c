#include "libc.h"
#include <errno.h>

Program_Heap program_heap;

void initialise_straylight_libc()
{
	errno = 0;

	initialise_heap(&program_heap, USERSPACE_HEAP_ADDRESS,
	                USERSPACE_HEAP_STARTING_SIZE);
}
