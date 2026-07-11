###############################################################################
#  Copyright (c) 2025, Ajxs.
#  SPDX-License-Identifier: GPL-3.0-or-later
###############################################################################

#include "riscv.h"

.section .text

.global processes_enter_new_process
.type processes_enter_new_process, @function
processes_enter_new_process:
	csrw sepc, a1

	mv sp, a2

	# Clear 'sstatus' so that we return into user mode.
	li   t0, SSTATUS_SPP
	# Set SPIE so that supervisor interrupts are enabled after sret.
	# If they're currently disabled, they will remain disabled.
	ori  t0, t0, SSTATUS_SPIE

	csrc sstatus, t0

	# Establish the user-mode invariant before entering the process: sscratch
	# holds the hart state pointer (currently in tp) so the first trap can
	# bootstrap via the swap in traps_entry, and tp is reset to a clean thread
	# pointer for the new process.
	csrw sscratch, tp
	li   tp, 0

	# Return into the user process.
	sret
