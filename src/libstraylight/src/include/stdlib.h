#ifndef STRAYLIGHT_STDLIB_H
#define STRAYLIGHT_STDLIB_H 1

#include <stddef.h>
#include <stdint.h>

#define EXIT_SUCCESS 0
#define EXIT_FAILURE 1

void *malloc(size_t size);

[[noreturn]] void exit(int exit_code);

#endif
