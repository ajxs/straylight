###############################################################################
#  Copyright (c) 2025, Ajxs.
#  SPDX-License-Identifier: GPL-3.0-or-later
###############################################################################

#include "riscv.h"

#define BOOT_STACK_SIZE 0x2000
#define BOOT_STACKS_HIGHER_HALF_BASE 0xFFFFFFFFFF000000

.section .boot_text

###############################################################################
# Boot entry point.
# a0: Hart ID
# a1: Pointer to device tree blob
###############################################################################
.global _start
.type _start, @function
_start:
.option push
.option norelax
# Load the global pointer location defined in the linker script into gp.
	lla  gp, __global_pointer$
.option pop

# Setup the boot stack pointer.
# SP := __boot_stacks_bottom + (Hart_ID * BOOT_STACK_SIZE) + BOOT_STACK_SIZE
# It's important to set up the stack this way, because SBI doesn't guarantee
# that hart 0 will be the boot hart.
# Realistically, the boot hart will be finished with its boot stack by the time
# the non-boot harts start up, so it could use whatever memory it needed, 
# but this is done for the sake of correctness.
	li t1, BOOT_STACK_SIZE
	mul t2, t1, a0

	lla t0, __boot_stacks_bottom
	add t0, t0, t2
	add t0, t0, t1
	mv sp, t0

# Save the Hart ID and devicetree pointer to the stack, since calling the 
# kernel init functions in Ada will overwrite a0 and a1.
	addi sp, sp, -16
	sd   a0, 8(sp)
	sd   a1, 0(sp)

	# Enable supervisor user memory access.
	li   t0, SSTATUS_SUM
	csrs sstatus, t0

# Set `Supervisor Address Translation and Protection (satp)` register to 0 to
# disable any address translation.
	csrw satp, zero

# Set up the supervisor-mode trap vector.
	lla   t0, traps_entry
	csrw stvec, t0

# Enable supervisor-mode timer and external interrupts, but leave interrupts 
# disabled until we're ready to jump into userspace.
	li   t0, SIE_SEIE | SIE_STIE
	csrs sie, t0

# Disable supervisor-mode interrupts.
	li   t0, SSTATUS_SIE
	csrc sstatus, t0

# This call jumps into the Ada code to set up the boot page tables.
	call boot_initialise_boot_page_tables
# If the boot page tables were not successfully initialised, zero will have
# been returned. In this case, halt the CPU.
	beq a0, zero, cpu_halt

# Install the new base page table address into the `satp` register.
	csrw satp, a0
	sfence.vma zero, zero

# Reload the Hart ID and devicetree pointer from the stack into a0 and a1
# as arguments for the kernel init function, before we reload the stack.
	ld   a1, 0(sp)
	ld   a0, 8(sp)
	addi sp, sp, 16

# Load the higher-half boot stack.
# This virtual address is the boot stack address mapped into the higher-half.
# This is mapped in boot-early.adb in Initialise_Boot_Page_Tables.
	li t1, BOOT_STACK_SIZE
	mul t2, t1, a0
	lla  t0, BOOT_STACKS_HIGHER_HALF_BASE
	add t0, t0, t2
	add t0, t0, t1

	mv  sp, t0

# Jump to the kernel initialisation function in Ada.
# Load the address of the function into a register first to avoid
# any potential issues with the linker and large binaries, due to using
# a higher-half kernel.
	lui a2, %hi(boot_initialise_kernel_memory)
	addi a2, a2, %lo(boot_initialise_kernel_memory)

	jalr zero, 0(a2)

# CPU spin function.
# This is where unused CPUs are parked in a spinlock.
.global cpu_halt
.type cpu_halt, @function
cpu_halt:
	wfi
	j cpu_halt


###############################################################################
# Non-boot hart entry point.
# Once non-boot harts have been started by the boot hart via SBI,
# they will jump to this entry point.
# a0: Hart ID
# a1: Opaque pointer (unused)
###############################################################################
.global non_boot_hart_entry
.type non_boot_hart_entry, @function
non_boot_hart_entry:
.option push
.option norelax
# Load the global pointer location defined in the linker script into gp.
	lla  gp, __global_pointer$
.option pop

	# Enable supervisor user memory access.
	li   t0, SSTATUS_SUM
	csrs sstatus, t0

# Set up the supervisor-mode trap vector.
	la   t0, traps_entry
	csrw stvec, t0

# Create SATP value for boot page table.
	ld   t0, boot_root_page_table
	srli t0, t0, 12
# Mask PPN.
	li   t1, (1 << 44) - 1
	and  t0, t0, t1
# Set MODE to Sv39
	li   t1, 8
	slli t1, t1, 60
# Combine MODE and PPN.
	or   t0, t0, t1

	sfence.vma zero, zero
	csrw satp, t0
	sfence.vma zero, zero

# Setup the boot stack pointer.
# SP := __boot_stacks_bottom + (Hart_ID * BOOT_STACK_SIZE) + BOOT_STACK_SIZE
	li t1, BOOT_STACK_SIZE
	mul t2, t1, a0

	la t0, __boot_stacks_bottom
	add t0, t0, t2
	add t0, t0, t1
	mv sp, t0

# Jump to the kernel initialisation function in Ada.
# Load the address of the function into a register first to avoid
# any potential issues with the linker and large binaries, due to using
# a higher-half kernel.
	lui t0, %hi(boot_non_boot_hart_entry)
	addi t0, t0, %lo(boot_non_boot_hart_entry)

	jalr zero, 0(t0)
