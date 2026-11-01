#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main()
{
	const int buffer_size = 256;

	char *input_buffer = (char *)malloc(buffer_size);
	if (input_buffer == 0)
	{
		fprintf(stderr, "Failed to allocate memory!\n");
		exit(EXIT_FAILURE);
	}

	while (true)
	{
		// We need to flush the output stream here since the stream is line
		// buffered, and the prompt string doesn't end in a newline.
		printf("> ");
		if (fflush(stdout) == EOF)
		{
			fprintf(stderr, "Failed to flush stdout!\n");
			return EXIT_FAILURE;
		}

		char *fgets_result = fgets(input_buffer, buffer_size, stdin);
		if (fgets_result == NULL)
		{
			if (feof(stdin))
			{
				break;
			}

			fprintf(stderr, "Failed to read input!\n");
			return EXIT_FAILURE;
		}

		if (strcmp(input_buffer, "exit\n") == 0)
		{
			break;
		}

		printf("%s", input_buffer);
	}

	printf("Exiting shell.\n");

	return EXIT_SUCCESS;
}
