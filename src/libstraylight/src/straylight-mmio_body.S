###############################################################################
#  Copyright (c) 2025, Ajxs.
#  SPDX-License-Identifier: GPL-3.0-or-later
###############################################################################

.section .text

.global mmio_read_unsigned_8
.type mmio_read_unsigned_8, @function
mmio_read_unsigned_8:
	lb a0, 0(a0)
	ret

.global mmio_read_unsigned_32
.type mmio_read_unsigned_32, @function
mmio_read_unsigned_32:
	lw a0, 0(a0)
	ret

.global mmio_read_unsigned_64
.type mmio_read_unsigned_64, @function
mmio_read_unsigned_64:
	ld a0, 0(a0)
	ret

.global mmio_write_unsigned_8
.type mmio_write_unsigned_8, @function
mmio_write_unsigned_8:
	sb a1, 0(a0)
	ret

.global mmio_write_unsigned_32
.type mmio_write_unsigned_32, @function
mmio_write_unsigned_32:
	sw a1, 0(a0)
	ret

.global mmio_write_unsigned_64
.type mmio_write_unsigned_64, @function
mmio_write_unsigned_64:
	sd a1, 0(a0)
	ret
