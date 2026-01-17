#ifndef STRAYLIGHT_LIBC_STDIO_H
#define STRAYLIGHT_LIBC_STDIO_H 1

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

#define NULL ((void *)0)

#define MAX_PATH 256

typedef struct
{
	uint64_t file_handle_id;
	uintptr_t buffer_address;
	size_t buffer_size;
	size_t current_position;
	bool eof;
} FILE;

#endif
