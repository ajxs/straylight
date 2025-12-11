###############################################################################
#  Copyright (c) 2025, Ajxs.
#  SPDX-License-Identifier: GPL-3.0-or-later
###############################################################################

#include "riscv.h"

.section .text

.global riscv_interrupts_enable_supervisor_interrupts
.type riscv_interrupts_enable_supervisor_interrupts, @function
riscv_interrupts_enable_supervisor_interrupts:
	li   t0, SSTATUS_SIE
	csrs sstatus, t0
	ret

.global riscv_interrupts_disable_supervisor_interrupts
.type riscv_interrupts_disable_supervisor_interrupts, @function
riscv_interrupts_disable_supervisor_interrupts:
	li   t0, SSTATUS_SIE
	csrc sstatus, t0
	ret

.global riscv_interrupts_are_supervisor_interrupts_enabled
.type riscv_interrupts_are_supervisor_interrupts_enabled, @function
riscv_interrupts_are_supervisor_interrupts_enabled:
	csrr t0, sstatus
	andi t0, t0, SSTATUS_SIE
	seqz a0, t0
	xori a0, a0, 1
	ret
