###############################################################################
#  Copyright (c) 2025, Ajxs.
#  SPDX-License-Identifier: GPL-3.0-or-later
###############################################################################

.section .boot_text

###############################################################################
# Prints a string to the SBI debug console during the early boot section.
# a0: String length.
# a1: String address.
###############################################################################
.global boot_early_debug_console_print
boot_early_debug_console_print:
# Split the string address into its high and low words, with the 
# high word in a2 and the low word in a1.
	srli a2, a1, 32

	li   t2, 0xFFFFFFFF
	and  a1, a1, t2

	li   a7, 0x4442434E
	li   a6, 0x0
	ecall

	ret
