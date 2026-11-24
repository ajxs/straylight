#include "simple_shell.h"
#include <dirent.h>
#include <errno.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>

void list_directory(int argument_count, char **arguments)
{
	if (argument_count > 2)
	{
		fprintf(stderr, "ls: too many arguments\n");
		return;
	}

	const char *path = (argument_count == 2) ? arguments[1] : ".";
	DIR *directory = opendir(path);
	if (directory == NULL)
	{
		fprintf(stderr, "ls: %s: %s\n", path, strerror(errno));
		return;
	}

	while (true)
	{
		// readdir leaves errno unchanged at the end of the directory, so it's
		// cleared first to tell the end apart from an error.
		errno = 0;
		struct dirent *entry = readdir(directory);
		if (entry == NULL)
		{
			if (errno != 0)
			{
				fprintf(stderr, "ls: %s: %s\n", path, strerror(errno));
			}

			break;
		}

		printf("%s%s\n", entry->d_name, (entry->d_type == DT_DIR) ? "/" : "");
	}

	closedir(directory);
}
