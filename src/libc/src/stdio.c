#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <straylight_syscall.h>
#include <string.h>

FILE *fopen(const char *restrict file_path, const char *restrict mode)
{
	size_t filename_length = strlen(file_path);

	int64_t result = straylight_libc_do_syscall(STRAYLIGHT_SYSCALL_FILE_OPEN,
	                                            file_path, filename_length, 0);
	if (is_syscall_result_error(result))
	{
		errno = result;

		return NULL;
	}

	FILE *file = (FILE *)malloc(sizeof(FILE));
	if (file == NULL)
	{
		// errno already set by malloc.
		return NULL;
	}

	file->file_handle_id = (uint64_t)result;
	file->buffer_address = 0;
	file->buffer_size = 0;
	file->current_position = 0;
	file->eof = false;

	return file;
}

size_t fread(void *restrict ptr, size_t size, size_t count,
             FILE *restrict stream)
{
	size_t total_bytes_to_read = size * count;
	size_t total_bytes_read = 0;

	int64_t result = straylight_libc_do_syscall(STRAYLIGHT_SYSCALL_FILE_READ,
	                                            stream->file_handle_id, ptr,
	                                            total_bytes_to_read);
	if (is_syscall_result_error(result))
	{
		errno = result;
		return 0;
	}

	if (result == 0)
	{
		stream->eof = true;
		return 0;
	}

	total_bytes_read = (size_t)result;

	return total_bytes_read / size;
}

int fseek(FILE *stream, long offset, int origin)
{
	int64_t result = straylight_libc_do_syscall(STRAYLIGHT_SYSCALL_FILE_SEEK,
	                                            stream->file_handle_id, offset);
	if (is_syscall_result_error(result))
	{
		errno = result;
		return -1;
	}

	return 0;
}
