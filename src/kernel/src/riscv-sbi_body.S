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
