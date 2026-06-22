#include "string.h"
#include <stddef.h>
#include <stdint.h>

int memcmp(const void *a, const void *b, size_t size)
{
	const uint8_t *pa = (const uint8_t *)a;
	const uint8_t *pb = (const uint8_t *)b;

	for (size_t i = 0; i < size; i++)
	{
		if (pa[i] != pb[i])
		{
			return (int)(pa[i] - pb[i]);
		}
	}

	return 0;
}

void *memcpy(void *dest, const void *src, size_t size)
{
	for (size_t i = 0; i < size; i++)
	{
		((uint8_t *)dest)[i] = ((uint8_t *)src)[i];
	}

	return dest;
}

void *memmove(void *dest, const void *src, size_t size)
{
	if (dest < src)
	{
		for (size_t i = 0; i < size; i++)
		{
			((uint8_t *)dest)[i] = ((uint8_t *)src)[i];
		}
	}
	else if (dest > src)
	{
		for (size_t i = size; i > 0; i--)
		{
			((uint8_t *)dest)[i - 1] = ((uint8_t *)src)[i - 1];
		}
	}

	return dest;
}

void *memset(void *dest, int value, size_t size)
{
	for (size_t i = 0; i < size; i++)
	{
		((uint8_t *)dest)[i] = (uint8_t)value;
	}

	return dest;
}

size_t strlen(const char *str)
{
	size_t len = 0;
	while (str[len] != '\0')
	{
		len++;
	}

	return len;
}

int strcmp(const char *lhs, const char *rhs)
{
	size_t i = 0;
	while (lhs[i] != '\0' && rhs[i] != '\0')
	{
		if (lhs[i] != rhs[i])
		{
			return (int)((uint8_t)lhs[i] - (uint8_t)rhs[i]);
		}

		i++;
	}

	return (int)((uint8_t)lhs[i] - (uint8_t)rhs[i]);
}

char *strcat(char *restrict dst, const char *restrict src)
{
	size_t dst_len = strlen(dst);
	size_t i = 0;

	while (src[i] != '\0')
	{
		dst[dst_len + i] = src[i];
		i++;
	}

	dst[dst_len + i] = '\0';

	return dst;
}

char *strchr(const char *s, int c)
{
	char ch = (char)c;

	while (*s != '\0')
	{
		if (*s == ch)
		{
			return (char *)s;
		}

		s++;
	}

	if (ch == '\0')
	{
		return (char *)s;
	}

	return NULL;
}

char *strcpy(char *restrict dst, const char *restrict src)
{
	size_t i = 0;

	while (src[i] != '\0')
	{
		dst[i] = src[i];
		i++;
	}

	dst[i] = '\0';

	return dst;
}
