#ifndef STRAYLIGHT_LIBC_SYSCALL_H
#define STRAYLIGHT_LIBC_SYSCALL_H

#include <stdbool.h>
#include <stdint.h>

#define STRAYLIGHT_SYSCALL_ALLOCATE_MEMORY 54460004
#define STRAYLIGHT_SYSCALL_FREE_MEMORY 54460005
#define STRAYLIGHT_SYSCALL_PROCESS_EXIT 54460000
#define STRAYLIGHT_SYSCALL_FILE_OPEN 54460107
#define STRAYLIGHT_SYSCALL_FILE_READ 54460108
#define STRAYLIGHT_SYSCALL_FILE_SEEK 54460109
#define STRAYLIGHT_SYSCALL_FILE_WRITE 54460110
#define STRAYLIGHT_SYSCALL_FILE_CLOSE 54460111
#define STRAYLIGHT_SYSCALL_FILE_TRUNCATE 54460112
#define STRAYLIGHT_SYSCALL_GET_WORKING_DIRECTORY 54460113
#define STRAYLIGHT_SYSCALL_CHANGE_WORKING_DIRECTORY 54460114
#define STRAYLIGHT_SYSCALL_READ_DIRECTORY_ENTRY 54460115

// The entry types in a straylight_directory_entry.
// These are ASCII letters, as used by 'find -type'.
// A mount point is reported as a directory.
#define STRAYLIGHT_DIRECTORY_ENTRY_TYPE_UNKNOWN '?'
#define STRAYLIGHT_DIRECTORY_ENTRY_TYPE_DEVICE 'c'
#define STRAYLIGHT_DIRECTORY_ENTRY_TYPE_DIRECTORY 'd'
#define STRAYLIGHT_DIRECTORY_ENTRY_TYPE_REGULAR_FILE 'f'

#define STRAYLIGHT_DIRECTORY_ENTRY_NAME_LENGTH 256

// The record filled in by STRAYLIGHT_SYSCALL_READ_DIRECTORY_ENTRY. This
// mirrors the kernel's System_Calls.Files.Directory_Entry_T. The name isn't
// terminated.
struct straylight_directory_entry
{
	uint8_t type;
	uint16_t name_length;
	char name[STRAYLIGHT_DIRECTORY_ENTRY_NAME_LENGTH];
};

int64_t straylight_libc_do_syscall(uint64_t syscall_number, ...);

bool is_syscall_result_error(int64_t result);

#endif
