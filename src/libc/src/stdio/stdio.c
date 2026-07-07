#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <straylight_syscall.h>
#include <string.h>
#include <unistd.h>

#define FWRITE_BUFFER_SIZE 4096

FILE *stdin;
FILE *stdout;
FILE *stderr;

int fgetc(FILE *stream)
{
	unsigned char c = 0;
	size_t result = fread(&c, sizeof(char), 1, stream);
	if (result == 1)
	{
		return (int)c;
	}

	return EOF;
}

char *fgets(char *restrict str, int n, FILE *restrict stream)
{
	if (n <= 0)
	{
		errno = EINVAL;
		return NULL;
	}

	int i = 0;
	while (i < n - 1)
	{
		int c = fgetc(stream);
		if (c == EOF)
		{
			if (i == 0)
			{
				return NULL; // No characters read before EOF
			}

			break; // Stop reading on EOF
		}

		str[i++] = (char)c;

		if (c == '\n')
		{
			break; // Stop reading on newline
		}
	}

	str[i] = '\0'; // Null-terminate the string
	return str;
}

int fclose(FILE *stream)
{
	if (stream == NULL)
	{
		errno = EINVAL;
		return EOF;
	}

	int flush_result = fflush(stream);
	int saved_errno = errno;

	int close_result = close(stream->file_handle_id);

	if (stream->read_buffer_address != NULL)
	{
		free(stream->read_buffer_address);
	}

	if (stream->write_buffer_address != NULL)
	{
		free(stream->write_buffer_address);
	}

	free(stream);

	if (flush_result == EOF)
	{
		errno = saved_errno;
		return EOF;
	}

	return close_result == -1 ? EOF : 0;
}

int feof(FILE *stream) { return stream->eof ? 1 : 0; }

static int get_file_open_flags_from_mode_string(const char *mode)
{
	if (strcmp(mode, "r") == 0)
	{
		return O_RDONLY;
	}

	if (strcmp(mode, "w") == 0)
	{
		return O_WRONLY | O_CREAT | O_TRUNC;
	}

	if (strcmp(mode, "a") == 0)
	{
		return O_WRONLY | O_CREAT | O_APPEND;
	}

	if (strcmp(mode, "r+") == 0)
	{
		return O_RDWR;
	}

	if (strcmp(mode, "w+") == 0)
	{
		return O_RDWR | O_CREAT | O_TRUNC;
	}

	if (strcmp(mode, "a+") == 0)
	{
		return O_RDWR | O_CREAT | O_APPEND;
	}

	return -1;
}

FILE *fopen(const char *restrict pathname, const char *restrict mode)
{
	int open_flags = get_file_open_flags_from_mode_string(mode);
	if (open_flags == -1)
	{
		errno = EINVAL;
		return NULL;
	}

	FILE *file = (FILE *)malloc(sizeof(FILE));
	if (file == NULL)
	{
		// errno already set by malloc.
		return NULL;
	}

	int fd = open((const char *)pathname, open_flags, (mode_t)0666);
	if (fd == -1)
	{
		free(file);

		// errno already set by open.
		return NULL;
	}

	file->file_handle_id = (uint32_t)fd;
	file->read_buffer_address = NULL;
	file->read_buffer_size = 0;
	file->read_buffer_offset = 0;
	file->read_buffer_valid_bytes = 0;
	file->write_buffer_address = NULL;
	file->write_buffer_size = 0;
	file->write_buffer_offset = 0;
	file->buffering_mode = _IOFBF;
	file->eof = false;
	file->mode_flags = open_flags;

	return file;
}

int fflush(FILE *stream)
{
	int file_access_mode = stream->mode_flags & O_ACCMODE;

	// If the file is open for reading, discard any data in the read buffer.
	if (file_access_mode == O_RDONLY || file_access_mode == O_RDWR)
	{
		stream->read_buffer_offset = 0;
	}

	// If the file is open for writing, flush any data in the write buffer to
	// the kernel.
	if (file_access_mode == O_WRONLY || file_access_mode == O_RDWR)
	{
		if (stream->write_buffer_address != NULL)
		{
			if (stream->write_buffer_offset > 0)
			{
				int64_t write_result = straylight_libc_do_syscall(
				    STRAYLIGHT_SYSCALL_FILE_WRITE, stream->file_handle_id,
				    (uint64_t)stream->write_buffer_address,
				    stream->write_buffer_offset);
				if (is_syscall_result_error(write_result))
				{
					errno = -write_result;
					return EOF;
				}

				stream->write_buffer_offset = 0;
			}
		}
	}

	return 0;
}

int fseek(FILE *stream, long offset, int whence)
{
	int flush_result = fflush(stream);
	if (flush_result == EOF)
	{
		// errno already set by fflush.
		return -1;
	}

	int64_t result = straylight_libc_do_syscall(
	    STRAYLIGHT_SYSCALL_FILE_SEEK, stream->file_handle_id, offset, whence);
	if (is_syscall_result_error(result))
	{
		errno = -result;
		return -1;
	}

	stream->eof = false;

	return 0;
}

static bool does_buffer_contain_newline(const char *buffer, size_t size)
{
	for (size_t i = 0; i < size; i++)
	{
		if (buffer[i] == '\n')
		{
			return true;
		}
	}

	return false;
}

static bool flush_file_stream_after_write_if_needed(FILE *stream,
                                                    size_t bytes_written)
{
	if (stream->buffering_mode == _IONBF)
	{
		// If the file stream is unbuffered, flush the buffer after every write.
		int result = fflush(stream);
		if (result == EOF)
		{
			return false;
		}
	}
	else if (stream->buffering_mode == _IOLBF)
	{
		// If the file stream is line buffered, flush the buffer if a newline
		// character was written to the stream.
		const char *written_bytes = (const char *)stream->write_buffer_address +
		                            stream->write_buffer_offset - bytes_written;

		const bool buffer_contains_newline =
		    does_buffer_contain_newline(written_bytes, bytes_written);

		if (buffer_contains_newline)
		{
			int flush_result = fflush(stream);
			if (flush_result == EOF)
			{
				return false;
			}
		}
	}

	return true;
}

size_t fwrite(const void *buffer, size_t size, size_t count, FILE *stream)
{
	if (count == 0 || size == 0)
	{
		return 0;
	}

	size_t total_bytes_to_write = size * count;
	size_t total_bytes_written = 0;
	size_t current_bytes_to_write = 0;
	size_t available_buffer_space = 0;

	while (total_bytes_written < total_bytes_to_write)
	{
		current_bytes_to_write = total_bytes_to_write - total_bytes_written;
		if (current_bytes_to_write > FWRITE_BUFFER_SIZE)
		{
			current_bytes_to_write = FWRITE_BUFFER_SIZE;
		}

		if (stream->write_buffer_address == NULL)
		{
			// Allocate the stream write buffer if it hasn't been allocated yet.
			stream->write_buffer_size = FWRITE_BUFFER_SIZE;
			stream->write_buffer_address = malloc(stream->write_buffer_size);
			if (stream->write_buffer_address == NULL)
			{
				// errno already set to ENOMEM by malloc
				return total_bytes_written / size;
			}
		}

		available_buffer_space =
		    stream->write_buffer_size - stream->write_buffer_offset;

		if (current_bytes_to_write > available_buffer_space)
		{
			int result = fflush(stream);
			if (result == EOF)
			{
				// errno already set by fflush.
				return total_bytes_written / size;
			}
		}

		memcpy((uint8_t *)stream->write_buffer_address +
		           stream->write_buffer_offset,
		       (uint8_t *)buffer + total_bytes_written, current_bytes_to_write);

		stream->write_buffer_offset += current_bytes_to_write;

		total_bytes_written += current_bytes_to_write;

		// In the case that the flush failed, return the number of full items that
		// were written to the stream buffer.
		if (!flush_file_stream_after_write_if_needed(stream,
		                                             current_bytes_to_write))
		{
			// errno already set by fflush.
			return total_bytes_written / size;
		}
	}

	return count;
}

int fputc(int c, FILE *stream)
{
	char ch = (char)c;

	size_t result = fwrite(&ch, sizeof(char), 1, stream);
	if (result == 1)
	{
		return c;
	}

	return EOF;
}

int putchar(int c) { return fputc(c, stdout); }

int puts(const char *s)
{
	while (*s)
	{
		if (putchar(*s++) == EOF)
		{
			return EOF;
		}
	}

	return putchar('\n');
}
