#ifndef STRAYLIGHT_LIBC_SYSCALL_H
#define STRAYLIGHT_LIBC_SYSCALL_H

#include <stdint.h>
#include <stdbool.h>

#define STRAYLIGHT_SYSCALL_ALLOCATE_MEMORY 54460004
#define STRAYLIGHT_SYSCALL_PROCESS_EXIT 54460000
#define STRAYLIGHT_SYSCALL_FILE_OPEN 54460107
#define STRAYLIGHT_SYSCALL_FILE_READ 54460108
#define STRAYLIGHT_SYSCALL_FILE_SEEK 54460109

int64_t straylight_libc_do_syscall(uint64_t syscall_number, ...);

bool is_syscall_result_error(int64_t result);

#endif
