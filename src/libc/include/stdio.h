#ifndef STRAYLIGHT_LIBC_STDIO_H
#define STRAYLIGHT_LIBC_STDIO_H 1

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#define NULL ((void *)0)

#define MAX_PATH 256

#define EOF (-1)

typedef struct
{
	uint32_t file_handle_id;

	int mode_flags;

	void *read_buffer_address;
	size_t read_buffer_size;
	size_t read_buffer_offset;
	size_t read_buffer_valid_bytes;

	void *write_buffer_address;
	size_t write_buffer_size;
	size_t write_buffer_offset;

	bool eof;
} FILE;

FILE *fopen(const char *restrict file_path, const char *restrict mode);

size_t fread(void *restrict ptr, size_t size, size_t count,
             FILE *restrict stream);

int fclose(FILE *stream);

int fseek(FILE *stream, long offset, int whence);

int feof(FILE *stream);

int fflush(FILE *stream);

#endif
