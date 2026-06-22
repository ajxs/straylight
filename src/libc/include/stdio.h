#ifndef STRAYLIGHT_LIBC_STDIO_H
#define STRAYLIGHT_LIBC_STDIO_H 1

#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#define NULL ((void *)0)

#define MAX_PATH 256

#define EOF (-1)

#define SEEK_SET 0
#define SEEK_CUR 1
#define SEEK_END 2

#define _IOFBF 0
#define _IOLBF 1
#define _IONBF 2

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

	int buffering_mode;

	bool eof;
} FILE;

extern FILE *stdin;
extern FILE *stdout;
extern FILE *stderr;

int fclose(FILE *stream);

int feof(FILE *stream);

int fflush(FILE *stream);

FILE *fopen(const char *restrict pathname, const char *restrict mode);

__attribute__((format(printf, 2, 3))) int fprintf(FILE *stream,
                                                  const char *format, ...);

__attribute__((format(printf, 1, 2))) int printf(const char *format, ...);

int fputc(int c, FILE *stream);

size_t fread(void *restrict ptr, size_t size, size_t count,
             FILE *restrict stream);

int fseek(FILE *stream, long offset, int whence);

size_t fwrite(const void *restrict ptr, size_t size, size_t count,
              FILE *restrict stream);

int putchar(int c);

int puts(const char *s);

int vprintf(const char *format, va_list ap);

int vfprintf(FILE *stream, const char *format, va_list ap);

#endif
