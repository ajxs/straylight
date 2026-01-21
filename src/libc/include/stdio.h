#ifndef STRAYLIGHT_LIBC_STDIO_H
#define STRAYLIGHT_LIBC_STDIO_H 1

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

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

FILE *fopen(const char *restrict file_path, const char *restrict mode);

size_t fread(void *restrict ptr, size_t size, size_t count,
             FILE *restrict stream);

int fclose(FILE *stream);

int fseek(FILE *stream, long offset, int origin);

#endif
