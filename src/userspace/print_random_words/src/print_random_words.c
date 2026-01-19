#include <stdbool.h>
#include <stdio.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <straylight/logging.h>

int main()
{
	print_to_serial("Printing random words!\n");

	char *buffer = (char *)malloc(64);
	if (buffer == 0)
	{
		print_to_serial("Failed to allocate memory!\n");
		exit(EXIT_FAILURE);
	}

	FILE *file = fopen("/Devices/Disk_B/seven_letter_words.txt", "r");
	if (file == NULL)
	{
		print_to_serial("Failed to open file!\n");
		exit(EXIT_FAILURE);
	}

	while (true)
	{
		if (fseek(file, 0, SEEK_SET) != 0)
		{
			print_to_serial("Failed to seek to start of file!\n");
			exit(EXIT_FAILURE);
		}

		const int number_of_words = 10;
		for (int i = 0; i < number_of_words; i++)
		{
			fread((void *)buffer, 1, 8, file);
			((char *)buffer)[8] = '\0';

			print_to_serial((char *)buffer);
		}
	}

	exit(EXIT_SUCCESS);
}
