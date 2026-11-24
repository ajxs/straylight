#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <stdlib.h>
#include <straylight_syscall.h>
#include <string.h>
#include <unistd.h>

// readdir copies the entry name from the kernel into d_name, which has room
// for NAME_MAX bytes and the terminator.
static_assert(STRAYLIGHT_DIRECTORY_ENTRY_NAME_LENGTH <= NAME_MAX);

struct straylight_libc_directory_stream
{
	int fd;
	// readdir returns a pointer to this entry, and the next call to readdir
	// overwrites it, as POSIX allows.
	struct dirent entry;
};

// https://pubs.opengroup.org/onlinepubs/9799919799/functions/opendir.html
DIR *opendir(const char *dirname)
{
	int fd = open(dirname, O_RDONLY);
	if (fd == -1)
	{
		// errno already set by open.
		return NULL;
	}

	DIR *dirp = malloc(sizeof(DIR));
	if (dirp == NULL)
	{
		close(fd);
		errno = ENOMEM;
		return NULL;
	}

	dirp->fd = fd;

	return dirp;
}

// https://pubs.opengroup.org/onlinepubs/9799919799/functions/readdir.html
struct dirent *readdir(DIR *dirp)
{
	struct straylight_directory_entry entry;
	int64_t result = straylight_libc_do_syscall(
	    STRAYLIGHT_SYSCALL_READ_DIRECTORY_ENTRY, dirp->fd, &entry);
	if (is_syscall_result_error(result))
	{
		errno = -result;
		return NULL;
	}

	// At the end of the directory, errno is left unchanged.
	if (result == 0)
	{
		return NULL;
	}

	dirp->entry.d_type = entry.type;
	memcpy(dirp->entry.d_name, entry.name, entry.name_length);
	dirp->entry.d_name[entry.name_length] = '\0';

	return &dirp->entry;
}

// https://pubs.opengroup.org/onlinepubs/9799919799/functions/closedir.html
int closedir(DIR *dirp)
{
	int result = close(dirp->fd);
	free(dirp);

	return result;
}
