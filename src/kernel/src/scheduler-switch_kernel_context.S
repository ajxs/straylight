###############################################################################
#  Copyright (c) 2025, Ajxs.
#  SPDX-License-Identifier: GPL-3.0-or-later
###############################################################################

#include "riscv.h"
#include "traps.h"

.section .text

###############################################################################
# Switches kernel context.
# Saves the old context, loads the new.
# a0: New SATP.
# a1: New ASID.
# a2: New task pointer.
# a3: Old kernel context pointer.
###############################################################################
.globl scheduler_switch_kernel_context
scheduler_switch_kernel_context:
	# Save all the callee-saved registers.
	sd ra, 0(a3)
	sd sp, 8(a3)
	sd s0, 16(a3)
	sd s1, 24(a3)
	sd s2, 32(a3)
	sd s3, 40(a3)
	sd s4, 48(a3)
	sd s5, 56(a3)
	sd s6, 64(a3)
	sd s7, 72(a3)
	sd s8, 80(a3)
	sd s9, 88(a3)
	sd s10, 96(a3)
	sd s11, 104(a3)
	# Falls-through below.

# This is the entry point for loading a new kernel context without saving
# the current one.
# This is used in the case that there is no running process.
.globl scheduler_load_kernel_context
scheduler_load_kernel_context:
	# Load the new process' memory space.
	csrw satp, a0
	# Refer to: https://blog.stephenmarz.com/2021/02/01/wrong-about-sfence/
	sfence.vma zero, a1

	# Store a pointer to this process in the 'tp' register.
	mv tp, a2

	# Add the offset of the kernel context to the thread pointer.
	addi a2, a2, PCB_OFFSET_KERNEL_CONTEXT

	# Restore all of the callee-saved registers from the kernel context.
	ld ra, 0(a2)
	ld sp, 8(a2)
	ld s0, 16(a2)
	ld s1, 24(a2)
	ld s2, 32(a2)
	ld s3, 40(a2)
	ld s4, 48(a2)
	ld s5, 56(a2)
	ld s6, 64(a2)
	ld s7, 72(a2)
	ld s8, 80(a2)
	ld s9, 88(a2)
	ld s10, 96(a2)
	ld s11, 104(a2)

scheduler_return_to_new_context:
	ret
