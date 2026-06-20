#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <straylight_syscall.h>
#include <string.h>
#include <unistd.h>

int fclose(FILE *stream)
{
	int result = close(stream->file_handle_id);
	if (result == -1)
	{
		// errno already set by close.
		return EOF;
	}

	free(stream->buffer_address);
	free(stream);
	return 0;
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

FILE *fopen(const char *restrict file_path, const char *restrict mode)
{
	int open_flags = get_file_open_flags_from_mode_string(mode);
	if (open_flags == -1)
	{
		errno = EINVAL;
		return NULL;
	}

	int fd = open((const char *)file_path, open_flags, (mode_t)0666);
	if (fd == -1)
	{
		// errno already set by open.
		return NULL;
	}

	FILE *file = (FILE *)malloc(sizeof(FILE));
	if (file == NULL)
	{
		// errno already set by malloc.
		return NULL;
	}

	file->file_handle_id = (uint32_t)fd;
	file->buffer_address = 0;
	file->buffer_size = 0;
	file->buffer_offset = 0;
	file->buffer_valid_bytes = 0;
	file->eof = false;

	return file;
}

size_t fread(void *restrict ptr, size_t size, size_t count,
             FILE *restrict stream)
{
	if (count == 0 || size == 0)
	{
		return 0;
	}

	size_t total_bytes_to_read = size * count;
	size_t total_bytes_read = 0;
	size_t remaining_bytes_to_read = 0;
	size_t current_bytes_to_copy_to_userspace = 0;

	// Keep reading data until we've read the requested amount or reached EOF.
	while (total_bytes_read < total_bytes_to_read)
	{
		remaining_bytes_to_read = total_bytes_to_read - total_bytes_read;

		// If the stream buffer offset is 0, we need to fill the buffer.
		if (stream->buffer_offset == 0)
		{
			// Is the stream buffer initialized?
			if (stream->buffer_size == 0)
			{
				// Allocate the stream buffer.
				stream->buffer_size = FREAD_BUFFER_SIZE;
				stream->buffer_address = malloc(stream->buffer_size);
				if (stream->buffer_address == NULL)
				{
					// errno already set to ENOMEM by malloc
					return 0;
				}
			}

			int64_t read_result = straylight_libc_do_syscall(
			    STRAYLIGHT_SYSCALL_FILE_READ, stream->file_handle_id,
			    stream->buffer_address, FREAD_BUFFER_SIZE);
			if (is_syscall_result_error(read_result))
			{
				errno = -read_result;
				return 0;
			}

			// If there's no more data to read, exit.
			if (read_result == 0)
			{
				stream->eof = true;
				break;
			}

			// Set the number of bytes in the buffer based upon the amount of data
			// returned from the kernel.
			// This variable becomes useful when the end of the file is reached, and
			// less data is read than the full buffer size.
			stream->buffer_valid_bytes = read_result;
		}

		// Is all the remaining data to read already inside the buffer?
		if (remaining_bytes_to_read <=
		    stream->buffer_valid_bytes - stream->buffer_offset)
		{
			current_bytes_to_copy_to_userspace = remaining_bytes_to_read;
		}
		else
		{
			// Copy all the remaining data in the buffer, and loop back around to
			// keep reading the rest of the data.
			current_bytes_to_copy_to_userspace =
			    stream->buffer_valid_bytes - stream->buffer_offset;
		}

		// Copy the data from the stream buffer to the userspace buffer.
		memcpy((uint8_t *)ptr + total_bytes_read,
		       (uint8_t *)stream->buffer_address + stream->buffer_offset,
		       current_bytes_to_copy_to_userspace);

		total_bytes_read += current_bytes_to_copy_to_userspace;
		stream->buffer_offset += current_bytes_to_copy_to_userspace;

		// If we've read all the data in the buffer, reset the current buffer
		// offset to 0 to indicate that the buffer needs refilling.
		if (stream->buffer_offset >= stream->buffer_valid_bytes)
		{
			stream->buffer_offset = 0;
		}
	}

	return total_bytes_read / size;
}

int fseek(FILE *stream, long offset, int whence)
{
	int64_t result = straylight_libc_do_syscall(
	    STRAYLIGHT_SYSCALL_FILE_SEEK, stream->file_handle_id, offset, whence);
	if (is_syscall_result_error(result))
	{
		errno = -result;
		return -1;
	}

	// Reset the stream buffer state so that the next read call will refill it.
	// The next read call will also set the EOF flag if needed.
	stream->buffer_offset = 0;
	stream->eof = false;

	return 0;
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

	while (total_bytes_written < total_bytes_to_write)
	{
		current_bytes_to_write = total_bytes_to_write - total_bytes_written;

		int64_t write_result = straylight_libc_do_syscall(
		    STRAYLIGHT_SYSCALL_FILE_WRITE, stream->file_handle_id,
		    (uint64_t)buffer + total_bytes_written, current_bytes_to_write);
		if (is_syscall_result_error(write_result))
		{
			errno = -write_result;
			return total_bytes_written / size;
		}

		if (write_result == 0)
		{
			break;
		}

		total_bytes_written += write_result;
	}

	return count;
}
