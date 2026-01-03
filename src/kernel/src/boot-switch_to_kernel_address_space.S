###############################################################################
#  Copyright (c) 2025, Ajxs.
#  SPDX-License-Identifier: GPL-3.0-or-later
###############################################################################

#include "riscv.h"

.section .text

###############################################################################
# Arguments:
# a0: Base page table address
# a1: stack top address
# a2: Return address.
###############################################################################
.global boot_switch_to_kernel_address_space
boot_switch_to_kernel_address_space:
	# Load the kernel address space page table.
	sfence.vma zero, zero
	csrw satp, a0
	sfence.vma zero, zero

	mv sp, a1

	# Jump to the specified return address.
	jalr a2
