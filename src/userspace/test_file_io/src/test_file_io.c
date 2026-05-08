#include <stdio.h>
#include <stdlib.h>
#include <straylight/logging.h>
#include <string.h>

int main()
{
	print_to_serial("Testing File IO!\n");

	char *buffer = (char *)malloc(64);
	if (buffer == 0)
	{
		print_to_serial("Failed to allocate memory!\n");
		exit(EXIT_FAILURE);
	}

	FILE *file = fopen("/Devices/Disk/test_file.txt", "r+");
	if (file == NULL)
	{
		print_to_serial("Failed to open file!\n");
		return EXIT_FAILURE;
	}

	fread((void *)buffer, 1, 20, file);
	((char *)buffer)[21] = '\0';

	print_to_serial((char *)buffer);

	const char *string_to_write = "Hello from Straylight!";
	const size_t string_length = strlen(string_to_write);
	fwrite(string_to_write, 1, string_length, file);

	print_to_serial("Wrote to file!\n");

	return EXIT_SUCCESS;
}
