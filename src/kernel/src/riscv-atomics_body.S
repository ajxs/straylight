###############################################################################
#  Copyright (c) 2025, Ajxs.
#  SPDX-License-Identifier: GPL-3.0-or-later
###############################################################################

.section .text
.global riscv_atomics_atomic_swap_and_return_unsigned_32
riscv_atomics_atomic_swap_and_return_unsigned_32:
	# The use of .aq (acquire) ensures that subsequent memory operations
	# are not reordered by the hart before this atomic operation.
	amoswap.w.aq a2, a1, (a0)
	mv a0, a2
	ret

.global riscv_atomics_atomic_swap_and_return_unsigned_64
riscv_atomics_atomic_swap_and_return_unsigned_64:
	amoswap.d.aq a2, a1, (a0)
	mv a0, a2
	ret

.global riscv_atomics_atomic_clear_unsigned_32
riscv_atomics_atomic_clear_unsigned_32:
	# The use of .rl (release) ensures that previous memory operations
	# are not reordered by the hart after this atomic operation.
	amoswap.w.rl zero, zero, (a0)
	ret

.global riscv_atomics_atomic_clear_unsigned_64
riscv_atomics_atomic_clear_unsigned_64:
	amoswap.d.rl zero, zero, (a0)
	ret

.global riscv_atomics_fence
riscv_atomics_fence:
	fence
	ret
