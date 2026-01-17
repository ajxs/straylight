#ifndef STRAYLIGHT_LIBC_WCHAR_H
#define STRAYLIGHT_LIBC_WCHAR_H

#include <stddef.h>
#include <stdint.h>

size_t wcslen(const wchar_t *ws);

size_t wcsnlen(const wchar_t *ws, size_t maxlen);

typedef struct
{
	int __unused;
} mbstate_t;

size_t mbstowcs(wchar_t *dest, const char *src, size_t n);

#endif
