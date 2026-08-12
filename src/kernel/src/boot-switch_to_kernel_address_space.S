###############################################################################
#  Copyright (c) 2025, Ajxs.
#  SPDX-License-Identifier: GPL-3.0-or-later
###############################################################################

#include "riscv.h"
#include "hart.h"

.section .text

###############################################################################
# Note that tp already holds the current hart state by this point.
# Arguments:
# a0: hart id
# a1: Base page table address
# a2: Stack top address
# a3: Stack size
# a4: Return address
###############################################################################
.global boot_switch_to_kernel_address_space
boot_switch_to_kernel_address_space:
# Load the kernel address space page table.
	sfence.vma zero, zero
	csrw satp, a1
	sfence.vma zero, zero

	mv   sp, a2

# Subtract the stack size from the stack top to get the stack limit, and 
# install it alongside sp, so that a supervisor-mode trap can detect an 
# overflow of the boot secondary stack.
	sub  t0, a2, a3
	sd   t0, HART_STATE_STACK_LIMIT_OFFSET(tp)

	# Jump to the specified return address.
	jalr a4
