#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <unistd.h>

#define FREAD_BUFFER_SIZE 4096

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
		if (stream->read_buffer_offset == 0)
		{
			// Is the stream buffer initialized?
			if (stream->read_buffer_size == 0)
			{
				// Allocate the stream buffer.
				stream->read_buffer_size = FREAD_BUFFER_SIZE;
				stream->read_buffer_address = malloc(stream->read_buffer_size);
				if (stream->read_buffer_address == NULL)
				{
					// errno already set to ENOMEM by malloc
					return total_bytes_read / size;
				}
			}

			ssize_t read_result =
			    read(stream->file_handle_id, stream->read_buffer_address,
			         FREAD_BUFFER_SIZE);
			if (read_result == -1)
			{
				// errno already set by read
				return total_bytes_read / size;
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
			stream->read_buffer_valid_bytes = read_result;
		}

		// Is all the remaining data to read already inside the buffer?
		if (remaining_bytes_to_read <=
		    stream->read_buffer_valid_bytes - stream->read_buffer_offset)
		{
			current_bytes_to_copy_to_userspace = remaining_bytes_to_read;
		}
		else
		{
			// Copy all the remaining data in the buffer, and loop back around to
			// keep reading the rest of the data.
			current_bytes_to_copy_to_userspace =
			    stream->read_buffer_valid_bytes - stream->read_buffer_offset;
		}

		// Copy the data from the stream buffer to the userspace buffer.
		memcpy((uint8_t *)ptr + total_bytes_read,
		       (uint8_t *)stream->read_buffer_address + stream->read_buffer_offset,
		       current_bytes_to_copy_to_userspace);

		total_bytes_read += current_bytes_to_copy_to_userspace;
		stream->read_buffer_offset += current_bytes_to_copy_to_userspace;

		// If we've read all the data in the buffer, reset the current buffer
		// offset to 0 to indicate that the buffer needs refilling.
		if (stream->read_buffer_offset >= stream->read_buffer_valid_bytes)
		{
			stream->read_buffer_offset = 0;
		}
	}

	return total_bytes_read / size;
}
