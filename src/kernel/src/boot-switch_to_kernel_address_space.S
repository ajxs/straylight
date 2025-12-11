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
###############################################################################
.global switch_to_kernel_address_space
switch_to_kernel_address_space:
	# Load the kernel address space page table.
	sfence.vma zero, zero
	csrw satp, a0
	sfence.vma zero, zero

	mv sp, a1

	# Jump into the kernel proper.
	j boot_initialise_kernel_services
