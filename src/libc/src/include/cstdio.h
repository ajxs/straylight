#ifndef STRAYLIGHT_LIBC_CSTDIO_H
#define STRAYLIGHT_LIBC_CSTDIO_H 1

#include <stddef.h>
#include <stdint.h>

#define NULL ((void *)0)

typedef struct {
	uintptr_t buffer_address;
	size_t buffer_size;
	size_t current_position;
	bool eof;
} FILE;

#endif
