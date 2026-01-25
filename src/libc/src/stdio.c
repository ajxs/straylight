#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <straylight_syscall.h>
#include <string.h>

int fclose(FILE *stream)
{
	int64_t result = straylight_libc_do_syscall(STRAYLIGHT_SYSCALL_FILE_CLOSE,
	                                            stream->file_handle_id);
	if (is_syscall_result_error(result))
	{
		errno = result;
		return -1;
	}

	free(stream->buffer_address);
	free(stream);
	return 0;
}

int feof(FILE *stream) { return stream->eof ? 1 : 0; }

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
	file->buffer_offset = 0;
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
	size_t bytes_in_buffer = stream->buffer_size;
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
				int64_t buffer_allocation_result = (int64_t)malloc(stream->buffer_size);
				if (buffer_allocation_result < 0)
				{
					errno = buffer_allocation_result;
					return 0;
				}

				stream->buffer_address = (void *)buffer_allocation_result;
			}

			int64_t read_result = straylight_libc_do_syscall(
			    STRAYLIGHT_SYSCALL_FILE_READ, stream->file_handle_id,
			    stream->buffer_address, FREAD_BUFFER_SIZE);
			if (is_syscall_result_error(read_result))
			{
				errno = read_result;
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
			bytes_in_buffer = read_result;
		}

		// Is all the remaining data to read already inside the buffer?
		if (remaining_bytes_to_read <= bytes_in_buffer - stream->buffer_offset)
		{
			current_bytes_to_copy_to_userspace = remaining_bytes_to_read;
		}
		else
		{
			// Copy all the remaining data in the buffer, and loop back around to
			// keep reading the rest of the data.
			current_bytes_to_copy_to_userspace =
			    bytes_in_buffer - stream->buffer_offset;
		}

		// Copy the data from the stream buffer to the userspace buffer.
		memcpy((uint8_t *)ptr + total_bytes_read,
		       (uint8_t *)stream->buffer_address + stream->buffer_offset,
		       current_bytes_to_copy_to_userspace);

		total_bytes_read += current_bytes_to_copy_to_userspace;
		stream->buffer_offset += current_bytes_to_copy_to_userspace;

		// If we've read all the data in the buffer, reset the current buffer
		// offset to 0 to indicate that the buffer needs refilling.
		if (stream->buffer_offset >= bytes_in_buffer)
		{
			stream->buffer_offset = 0;
		}
	}

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

	// Reset the stream buffer state so that the next read call will refill it.
	// The next read call will also set the EOF flag if needed.
	stream->buffer_offset = 0;
	stream->eof = false;

	return 0;
}
