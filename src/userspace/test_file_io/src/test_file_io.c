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

	FILE *words_file = fopen("/Devices/Disk_B/seven_letter_words.txt", "r");
	if (words_file == NULL)
	{
		print_to_serial("Failed to open file!\n");
		return EXIT_FAILURE;
	}

	FILE *test_write_file = fopen("/Devices/Disk/test_file.txt", "r+");
	if (test_write_file == NULL)
	{
		print_to_serial("Failed to open file!\n");
		return EXIT_FAILURE;
	}

	fseek(test_write_file, 0, SEEK_SET);

	for (int i = 0; i < 70; i++)
	{
		fread((void *)buffer, 1, 8, words_file);
		((char *)buffer)[8] = '\0';

		fwrite(buffer, 1, 8, test_write_file);
	}

	print_to_serial("Wrote to file!\n");

	FILE *new_file = fopen("/Devices/Disk/nonexistent_file.txt", "w+");
	if (new_file == NULL)
	{
		print_to_serial("Failed to create file!\n");
		return EXIT_FAILURE;
	}

	FILE *new_file_with_long_name =
	    fopen("/Devices/Disk/Programs/"
	          "nonexistent_file_with_really_long_and__silly_name.txt",
	          "w+");
	if (new_file_with_long_name == NULL)
	{
		print_to_serial("Failed to create file!\n");
		return EXIT_FAILURE;
	}

	return EXIT_SUCCESS;
}
