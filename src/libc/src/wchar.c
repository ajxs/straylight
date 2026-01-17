#include <wchar.h>

size_t wcslen(const wchar_t *ws)
{
	size_t length = 0;
	while (ws[length] != L'\0')
	{
		length++;
	}

	return length;
}

size_t wcsnlen(const wchar_t *ws, size_t maxlen)
{
	size_t length = 0;
	while (length < maxlen && ws[length] != L'\0')
	{
		length++;
	}

	return length;
}

size_t mbstowcs(wchar_t *dest, const char *src, size_t n)
{
	size_t i = 0;

	if (!src)
		return 0;

	if (!dest)
	{
		/* Query mode: return length */
		while (src[i] != '\0')
			i++;
		return i;
	}

	while (i < n && src[i] != '\0')
	{
		unsigned char c = (unsigned char)src[i];

		/* ASCII only */
		dest[i] = (wchar_t)c;
		i++;
	}

	if (i < n)
		dest[i] = L'\0';

	return i;
}
