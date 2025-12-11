###############################################################################
#  Copyright (c) 2025, Ajxs.
#  SPDX-License-Identifier: GPL-3.0-or-later
###############################################################################

#include "riscv.h"

.section .text

.global scheduler_enter_new_process
.type scheduler_enter_new_process, @function
scheduler_enter_new_process:
	csrw sepc, a1

	mv sp, a2

	# Clear 'sstatus' so that we return into user mode.
	li   t0, SSTATUS_SPP
	# Set SPIE so that supervisor interrupts are enabled after sret.
	# If they're currently disabled, they will remain disabled.
	ori  t0, t0, SSTATUS_SPIE

	csrc sstatus, t0

	# Return into the user process.
	sret
