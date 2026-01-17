#ifndef STRAYLIGHT_LIBC_STRING_H
#define STRAYLIGHT_LIBC_STRING_H 1

#include <stddef.h>
#include <stdint.h>

int memcmp(const void *a, const void *b, size_t size);

void *memcpy(void *dest, const void *src, size_t size);

void *memmove(void *dest, const void *src, size_t size);

void *memset(void *dest, int value, size_t size);

size_t strlen(const char *str);

#endif
