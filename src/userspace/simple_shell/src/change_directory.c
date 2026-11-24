#include "simple_shell.h"
#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

void change_directory(int argument_count, char **arguments)
{
	if (argument_count > 2)
	{
		fprintf(stderr, "cd: too many arguments\n");
		return;
	}

	const char *path = (argument_count == 2) ? arguments[1] : "/";
	if (chdir(path) == -1)
	{
		fprintf(stderr, "cd: %s: %s\n", path, strerror(errno));
	}
}
