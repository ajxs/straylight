#include "simple_shell.h"
#include <errno.h>
#include <limits.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

void print_working_directory(int argument_count)
{
	if (argument_count > 1)
	{
		fprintf(stderr, "pwd: too many arguments\n");
		return;
	}

	char path[PATH_MAX];
	if (getcwd(path, sizeof(path)) == NULL)
	{
		fprintf(stderr, "pwd: %s\n", strerror(errno));
		return;
	}

	printf("%s\n", path);
}
