/**
 *  Copyright (c) 2025, Ajxs.
 *  SPDX-License-Identifier: GPL-3.0-or-later
 */

#ifndef TRAPS_H
#define TRAPS_H

// These PCB offsets are pinned by the representation clause on
// Process_Control_Block_T in processes.ads.
#define PCB_OFFSET_KERNEL_CONTEXT 0
// Must match Kernel_Context_Register_List_T in processes.ads.
#define KERNEL_CONTEXT_SIZE (14 * 8)
#define KERNEL_CONTEXT_OFFSET_SP (PCB_OFFSET_KERNEL_CONTEXT + (1 * 8))
#define PCB_OFFSET_TRAP_CONTEXT (PCB_OFFSET_KERNEL_CONTEXT + KERNEL_CONTEXT_SIZE)
#define TRAP_CONTEXT_SIZE (68 * 8)

#define PCB_OFFSET_USER_SP (PCB_OFFSET_TRAP_CONTEXT + 8)
// Skips Kernel_Stack_Phys_Addr, which precedes it in the record.
#define PCB_OFFSET_KERNEL_STACK_VIRT_ADDR (PCB_OFFSET_USER_SP + (2 * 8))

// Offsets of general-purpose registers within the saved trap context.
// These are used where a specific register slot must be patched independently
// of the generic save/restore loop (x2 = sp, x4 = tp).
#define TRAP_CONTEXT_OFFSET_SP (2 * 8)
#define TRAP_CONTEXT_OFFSET_TP (4 * 8)

// Offsets of special registers within the saved trap context.
#define TRAP_CONTEXT_OFFSET_SEPC 512
#define TRAP_CONTEXT_OFFSET_SSTATUS 520
#define TRAP_CONTEXT_OFFSET_SCAUSE 528
#define TRAP_CONTEXT_OFFSET_PREV_TP 536

#endif
