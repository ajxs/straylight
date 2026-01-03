###############################################################################
#  Copyright (c) 2025, Ajxs.
#  SPDX-License-Identifier: GPL-3.0-or-later
###############################################################################

.section .text

.global sbi_set_timer
.type sbi_set_timer, @function
sbi_set_timer:
	li a7, 0x54494D45
	li a6, 0x0
	ecall

	ret

.global sbi_debug_console_print
.type sbi_debug_console_print, @function
sbi_debug_console_print:
	li a7, 0x4442434E
	li a6, 0x0
	ecall

	ret

.global sbi_hart_start
.type sbi_hart_start, @function
sbi_hart_start:
	li a7, 0x48534D
	li a6, 0x0
	ecall

	ret

.global sbi_get_hart_status
.type sbi_get_hart_status, @function
sbi_get_hart_status:
	li a7, 0x48534D
	li a6, 0x2
	ecall

	ret
