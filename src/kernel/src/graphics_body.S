###############################################################################
#  Copyright (c) 2025, Ajxs.
#  SPDX-License-Identifier: GPL-3.0-or-later
###############################################################################

.section .text

###############################################################################
# Arguments:
# a0: Framebuffer_Base_Address (virtual address)
# a1: Framebuffer_Width
# a2: Framebuffer_Height
# a3: Colour (RGBA format: A[31:24], B[23:16], G[15:8], R[7:0])
###############################################################################
.global graphics_fill_framebuffer
.type graphics_fill_framebuffer, @function
graphics_fill_framebuffer:
# Total iterations = (width * height * sizeof(u32)) / 2
	mul  t0, a1, a2
	srli t0, t0, 1

# 64-bit value with two copies of the 32-bit pixel:
# t1 = (a3 << 32) | a3
	slli t1, a3, 32
	or   t1, t1, a3

.fill_loop:
	sd   t1, 0(a0)
	addi a0, a0, 8
	addi t0, t0, -1
	bgtz t0, .fill_loop

	ret

###############################################################################
# Arguments:
# a0: R (Unsigned_8)
# a1: G (Unsigned_8)
# a2: B (Unsigned_8)
# a3: A (Unsigned_8)
# Returns:
# a0: Colour (RGBA format: R[31:24], G[23:16], B[15:8], A[7:0])
###############################################################################
.global graphics_make_colour
.type graphics_make_colour, @function
graphics_make_colour:
	slli t0, a3, 24
	slli t1, a2, 16
	slli t2, a1, 8
	or t0, t0, t1
	or t0, t0, t2
	or t0, t0, a0
	mv a0, t0
	ret
