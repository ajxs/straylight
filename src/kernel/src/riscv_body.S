###############################################################################
#  Copyright (c) 2025, Ajxs.
#  SPDX-License-Identifier: GPL-3.0-or-later
###############################################################################

.section .text

.global riscv_get_system_time
riscv_get_system_time:
	csrr a0, time

	ret
