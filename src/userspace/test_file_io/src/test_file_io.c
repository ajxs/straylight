#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main()
{
	printf("Testing File IO!\n");

	char *buffer = (char *)malloc(64);
	if (buffer == 0)
	{
		fprintf(stderr, "Failed to allocate memory!\n");
		exit(EXIT_FAILURE);
	}

	FILE *words_file = fopen("/Devices/Disk_B/seven_letter_words.txt", "r");
	if (words_file == NULL)
	{
		fprintf(stderr, "Failed to open file!\n");
		return EXIT_FAILURE;
	}

	FILE *test_write_file = fopen("/Devices/Disk/test_file.txt", "r+");
	if (test_write_file == NULL)
	{
		fprintf(stderr, "Failed to open file!\n");
		return EXIT_FAILURE;
	}

	fseek(test_write_file, 0, SEEK_SET);

	for (int i = 0; i < 70; i++)
	{
		fread((void *)buffer, 1, 8, words_file);
		((char *)buffer)[8] = '\0';

		fwrite(buffer, 1, 8, test_write_file);
	}

	printf("Wrote to file!\n");

	int fclose_result = fclose(words_file);
	if (fclose_result == EOF)
	{
		fprintf(stderr, "Failed to close words_file!\n");
		return EXIT_FAILURE;
	}

	fclose_result = fclose(test_write_file);
	if (fclose_result == EOF)
	{
		fprintf(stderr, "Failed to close test_write_file!\n");
		return EXIT_FAILURE;
	}

	FILE *new_file = fopen("/Devices/Disk/nonexistent_file.txt", "w+");
	if (new_file == NULL)
	{
		fprintf(stderr, "Failed to create new_file!\n");
		return EXIT_FAILURE;
	}

	FILE *new_file_with_long_name =
	    fopen("/Devices/Disk/Programs/"
	          "nonexistent_file_with_really_long_and_silly_name.txt",
	          "w+");
	if (new_file_with_long_name == NULL)
	{
		fprintf(stderr, "Failed to create new_file_with_long_name!\n");
		return EXIT_FAILURE;
	}

	fclose_result = fclose(new_file);
	if (fclose_result == EOF)
	{
		fprintf(stderr, "Failed to close new_file!\n");
		return EXIT_FAILURE;
	}

	fclose_result = fclose(new_file_with_long_name);
	if (fclose_result == EOF)
	{
		fprintf(stderr, "Failed to close new_file_with_long_name!\n");
		return EXIT_FAILURE;
	}

	FILE *serial_device = fopen("/Devices/Serial", "w");
	if (serial_device == NULL)
	{
		fprintf(stderr, "Failed to open serial device!\n");
		return EXIT_FAILURE;
	}

	const char *message = "Hello, Serial Device!\n";
	size_t serial_write_result =
	    fwrite(message, 1, strlen(message), serial_device);
	if (serial_write_result != strlen(message))
	{
		fprintf(stderr, "Failed to write to serial_device!\n");
		return EXIT_FAILURE;
	}

	fclose_result = fclose(serial_device);
	if (fclose_result == EOF)
	{
		fprintf(stderr, "Failed to close serial_device!\n");
		return EXIT_FAILURE;
	}

	printf("stdin: %u\nstdout: %u\nstderr: %u\n", stdin->file_handle_id,
	       stdout->file_handle_id, stderr->file_handle_id);
	fflush(stdout);

	return EXIT_SUCCESS;
}
