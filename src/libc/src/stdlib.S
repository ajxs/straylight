/**
 *  Copyright (c) 2026, Ajxs.
 *  SPDX-License-Identifier: GPL-3.0-or-later
 */

#include <straylight_syscall.h>

.global malloc
.type malloc, @function
malloc:
	mv a1, a0
	li a0, STRAYLIGHT_SYSCALL_ALLOCATE_MEMORY
	li a2, 1
	ecall

	mv a0, a1
	ret


.global exit
.type exit, @function
exit:
	li a0, STRAYLIGHT_SYSCALL_PROCESS_EXIT
	mv a1, a0
	ecall
	ret
