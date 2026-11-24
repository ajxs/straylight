#include "simple_shell.h"
#include <ctype.h>
#include <limits.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define MAXIMUM_ARGUMENT_COUNT 16

// Splits the line into tokens separated by whitespace, in place, and stores a
// pointer to each token in tokens.
// Returns the number of tokens, or -1 if there are more than maximum_count.
static int tokenise_line(char *line, char **tokens, int maximum_count)
{
	int count = 0;
	char *c = line;

	while (*c != '\0')
	{
		if (isspace((unsigned char)*c))
		{
			*c++ = '\0';
			continue;
		}

		if (count == maximum_count)
		{
			return -1;
		}

		tokens[count++] = c;
		while (*c != '\0' && !isspace((unsigned char)*c))
		{
			c++;
		}
	}

	return count;
}

int main()
{
	const int buffer_size = 4096;

	char *input_buffer = (char *)malloc(buffer_size);
	if (input_buffer == 0)
	{
		fprintf(stderr, "Failed to allocate memory!\n");
		exit(EXIT_FAILURE);
	}

	while (true)
	{
		char working_directory[PATH_MAX];
		if (getcwd(working_directory, sizeof(working_directory)) == NULL)
		{
			strcpy(working_directory, "?");
		}

		// We need to flush the output stream here since the stream is line
		// buffered, and the prompt string doesn't end in a newline.
		printf("%s> ", working_directory);
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

		// Check whether user input overflowed the input buffer.
		// If no newline character exists in the buffer, and the EOF hasn't been
		// reached, the buffer filled up before the end of the line.
		if (strchr(input_buffer, '\n') == NULL && !feof(stdin))
		{
			// If the next character ends the line, the line was exactly as long
			// as the buffer, and only its newline was left unread. The whole
			// line is in the buffer, so it can still be run.
			int c = fgetc(stdin);
			if (c != '\n' && c != EOF)
			{
				// Read and discard the rest of the line, so that it isn't run
				// as the next command.
				while ((c = fgetc(stdin)) != EOF && c != '\n')
				{
				}

				fprintf(stderr, "Line too long\n");
				continue;
			}
		}

		char *arguments[MAXIMUM_ARGUMENT_COUNT];
		int argument_count =
		    tokenise_line(input_buffer, arguments, MAXIMUM_ARGUMENT_COUNT);
		if (argument_count == -1)
		{
			fprintf(stderr, "Too many arguments\n");
			continue;
		}

		if (argument_count == 0)
		{
			continue;
		}

		const char *command = arguments[0];
		if (strcmp(command, "exit") == 0)
		{
			break;
		}
		else if (strcmp(command, "cd") == 0)
		{
			change_directory(argument_count, arguments);
		}
		else if (strcmp(command, "ls") == 0)
		{
			list_directory(argument_count, arguments);
		}
		else if (strcmp(command, "pwd") == 0)
		{
			print_working_directory(argument_count);
		}
		else
		{
			fprintf(stderr, "%s: unknown command\n", command);
		}
	}

	printf("Exiting shell.\n");

	return EXIT_SUCCESS;
}
