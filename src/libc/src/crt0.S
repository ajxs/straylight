/**
 *  Copyright (c) 2026, Ajxs.
 *  SPDX-License-Identifier: GPL-3.0-or-later
 */

.section .text

.global _start
_start:
	call load_gp
	call initialise_straylight_libc
	call main
# The exit status returned by main is already in a0
	j    exit

# Taken from: https://github.com/bminor/glibc/blob/master/sysdeps/riscv/start.S
load_gp:
.option push
.option norelax
	lla gp, __global_pointer$
.option pop
	ret
