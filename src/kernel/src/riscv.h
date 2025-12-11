/**
 *  Copyright (c) 2025, Ajxs.
 *  SPDX-License-Identifier: GPL-3.0-or-later
 *
 *  Authors:
 *     Anthony <ajxs [at] panoptic.online>
 */

#ifndef RISCV_H
#define RISCV_H

#define SIE_SEIE (1L << 9) // External
#define SIE_STIE (1L << 5) // Timer

#define SSTATUS_SUM  (1L << 18) // Supervisor User Memory access enable.
#define SSTATUS_SPP  (1L << 8)  // Supervisor Previous Privilege mode.
#define SSTATUS_SPIE (1L << 5)  // Supervisor Previous Interrupt Enable.
#define SSTATUS_SIE  (1L << 1)  // Supervisor mode interrupt enable.
#define SSTATUS_UIE  (1L << 0)  // User mode interrupt enable.

#endif
