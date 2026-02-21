###############################################################################
#  Copyright (c) 2025, Ajxs.
#  SPDX-License-Identifier: GPL-3.0-or-later
###############################################################################

#include "riscv.h"

.section .text

###############################################################################
# Arguments:
# a0: hart id
# a1: Base page table address
# a2: stack top address
# a3: Return address.
###############################################################################
.global boot_switch_to_kernel_address_space
boot_switch_to_kernel_address_space:
	# Load the kernel address space page table.
	sfence.vma zero, zero
	csrw satp, a1
	sfence.vma zero, zero

	mv sp, a2

	# Jump to the specified return address.
	jalr a3
