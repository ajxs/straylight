#ifndef STRAYLIGHT_LIBC_STDLIB_H
#define STRAYLIGHT_LIBC_STDLIB_H 1

#include <stddef.h>
#include <stdint.h>

#define EXIT_SUCCESS 0
#define EXIT_FAILURE 1

void *aligned_alloc(size_t alignment, size_t size);

[[noreturn]] void exit(int exit_code);

void free(void *ptr);

void *malloc(size_t size);

#endif
