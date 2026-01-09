#include <stddef.h>
#include <stdint.h>
#include "string.h"

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

void *memcpy(
		void *dest,
		const void *src,
		size_t size)
{
	for (size_t i = 0; i < size; i++)
	{
		((uint8_t *)dest)[i] = ((uint8_t *)src)[i];
	}
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
