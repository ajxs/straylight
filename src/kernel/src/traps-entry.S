###############################################################################
#  Copyright (c) 2025, Ajxs.
#  SPDX-License-Identifier: GPL-3.0-or-later
###############################################################################

#include "hart.h"
#include "riscv.h"
#include "traps.h"

.altmacro

.macro save_gp_register i, base
	sd   x\i, ((\i)*8)(\base)
.endm

.macro restore_gp_register i, base
	ld   x\i, ((\i)*8)(\base)
.endm

# These macros use a separate offset parameter, since the register number and
# the offset will be different, register number f0 is stored at offset 32 in
# the process' context.
.macro save_fp_register i, offset, base
	fsd  f\i, ((\offset)*8)(\base)
.endm

.macro restore_fp_register i, offset, base
	fld   f\i, ((\offset)*8)(\base)
.endm


.section .text

# The address of the trap vector must be aligned on a 4-byte boundary.
# Since the last two bits of the stvec register are not used in the address.
.align 4
.global traps_entry
.type traps_entry, @function
traps_entry:
# Determine whether the trap came from user mode or supervisor mode.
# t6 will be restored below.
	csrw  sscratch, t6
	csrr  t6, sstatus
	andi  t6, t6, SSTATUS_SPP

# If SPP == 1, the trap has come from supervisor mode, and we don't need to
# reload the kernel stack pointer.
	bnez  t6, save_process_state_to_kernel_stack

user_mode_trap:
# Since t6 has been saved in sscratch is currently a scratch variable, use 
# this to load the PCB pointer from the Hart state (tp).
	ld t6, HART_STATE_PCB_POINTER_OFFSET(tp)

# Save the user stack pointer to the PCB.
# We can safely do this because user-mode traps can't be nested, and this value
# won't be changed until we need to return to user mode.
	sd sp, PCB_OFFSET_USER_SP(t6)

# Load the kernel stack pointer.
# The offset points to the entry for sp in the kernel context saved registers.
	ld sp, KERNEL_CONTEXT_OFFSET_SP(t6)

save_process_state_to_kernel_stack:
# Restore t6 from sscratch.
	csrr t6, sscratch

	addi sp, sp, -TRAP_CONTEXT_SIZE

# Note: For supervisor-mode traps, the stack pointer in x2 saved here points
# to the kernel stack frame, not the stack at the time the trap occurred.
	.set  i, 0
	.rept 32
		save_gp_register %i, sp
		.set i, i+1
	.endr

# Read the FS field from sstatus to determine whether we need to save
# the floating point registers.
	csrr t0, sstatus
	srli t0, t0, 13
	andi t0, t0, 3       # extract FS field (bits 14:13)
	beqz t0, skip_fp_save

	.set  i, 0
	.set offset, 32
	.rept 32
		save_fp_register %i, %offset, sp
		.set i, i+1
		.set offset, offset+1
	.endr

skip_fp_save:

# Since we've saved the hart state, we can use whatever gp registers we need
# below this point.
# Save sepc.
	csrr t6, sepc
	sd t6, TRAP_CONTEXT_OFFSET_SEPC(sp)

# Save sstatus.
# This is done to ensure that SPP is preserved across traps. Otherwise we'd
# always return to supervisor mode in the case of nested traps.
	csrr t6, sstatus
	sd t6, TRAP_CONTEXT_OFFSET_SSTATUS(sp)

# Save scause.
# Done for debugging purposes.
	csrr t6, scause
	sd t6, TRAP_CONTEXT_OFFSET_SCAUSE(sp)

	ld t5, HART_STATE_PCB_POINTER_OFFSET(tp)

# Save the trap context pointer.
# This is important, so that we can recover the previous trap context after 
# returning from the trap handler.
# It's possible to get this value directly from the stack pointer, but this 
# method is more robust in case the stack pointer is modified in the trap
# handler.
	ld t6, PCB_OFFSET_TRAP_CONTEXT(t5)
	sd t6, TRAP_CONTEXT_OFFSET_TP(sp)

# Now the trap context is saved on the stack, store a pointer in the 
# process control block, so that we can access the trap context in the handler
# routines.
	sd sp, PCB_OFFSET_TRAP_CONTEXT(t5)

	# Jump to the trap handler routine.
	mv   a0, t5
	csrr a1, scause
	csrr a2, sepc
	csrr a3, stval

	csrr t6, sstatus
	andi  t6, t6, SSTATUS_SPP
	bnez  t6, call_supervisor_trap_handler

	call trap_handler_user_mode
	j restore_trap_frame

call_supervisor_trap_handler:
	call trap_handler_supervisor_mode

restore_trap_frame:
# Restore the trap context pointer.
	ld t5, HART_STATE_PCB_POINTER_OFFSET(tp)
	ld t6, TRAP_CONTEXT_OFFSET_TP(sp)
	sd t6, PCB_OFFSET_TRAP_CONTEXT(t5)

# Restore scause.
	ld t6, TRAP_CONTEXT_OFFSET_SCAUSE(sp)
	csrw scause, t6

# Restore sstatus.
	ld t6, TRAP_CONTEXT_OFFSET_SSTATUS(sp)
	csrw sstatus, t6

# Restore sepc.
	ld t6, TRAP_CONTEXT_OFFSET_SEPC(sp)
	csrw sepc, t6

# Read the FS field from sstatus to determine whether we need to restore 
# the floating point registers.
	csrr t6, sstatus
	srli t6, t6, 13
	andi t6, t6, 3       # extract FS field (bits 14:13)
	beqz t6, skip_fp_restore

	.set  i, 0
	.set offset, 32
	.rept 32
		restore_fp_register %i, %offset, sp
		.set i, i+1
		.set offset, offset+1
	.endr

skip_fp_restore:

# Test whether we're returning to supervisor mode.
# This determines whether we reload the user stack pointer from the PCB.
	csrr  t6, sstatus
	andi  t6, t6, SSTATUS_SPP

# If SPP == 1, the trap has come from supervisor mode, and we don't need to
# reload the stack pointer.
	bnez  t6, restore_gp_pointers_and_return_to_supervisor_mode

	.set  i, 0
	.rept 32
		restore_gp_register %i, sp
		.set i, i+1
	.endr

# Re-save the kernel stack pointer before restoring the user stack pointer.
# This is important, as the process may have been pre-empted during the trap,
# and the kernel stack pointer with the trap frame may have been persisted in
# the PCB's kernel context by the scheduler. This will ensure that the correct
# stack pointer is saved.
	addi sp, sp, TRAP_CONTEXT_SIZE

# temporarily save t6 in sscratch, then use it to load the PCB pointer.
	csrw sscratch, t6

	ld t6, HART_STATE_PCB_POINTER_OFFSET(tp)
	sd sp, KERNEL_CONTEXT_OFFSET_SP(t6)

	ld sp, PCB_OFFSET_USER_SP(t6)

# Restore t6 from sscratch before returning to user mode.
	csrr t6, sscratch

return_to_user_mode:
	sret

restore_gp_pointers_and_return_to_supervisor_mode:
	.set  i, 0
	.rept 32
		restore_gp_register %i, sp
		.set i, i+1
	.endr

# temporarily save t6 in sscratch, then use it to load the PCB pointer.
	csrw sscratch, t6
	ld t6, HART_STATE_PCB_POINTER_OFFSET(tp)

	addi sp, sp, TRAP_CONTEXT_SIZE
	sd sp, KERNEL_CONTEXT_OFFSET_SP(t6)

# Restore t6 from sscratch before returning to supervisor mode.
	csrr t6, sscratch

return_to_supervisor_mode:
	sret

