###############################################################################
#  Copyright (c) 2025, Ajxs.
#  SPDX-License-Identifier: GPL-3.0-or-later
###############################################################################

#include "hart.h"

.section .text

.global hart_state_get_current_running_process
.type hart_state_get_current_running_process, @function
hart_state_get_current_running_process:
	ld    a0, HART_STATE_PCB_POINTER_OFFSET(tp)
	ret

.global hart_state_get_hart_id
.type hart_state_get_hart_id, @function
hart_state_get_hart_id:
	lw    a0, HART_STATE_HART_ID_OFFSET(tp)
	ret

.global hart_state_get_hart_state
.type hart_state_get_hart_state, @function
hart_state_get_hart_state:
	mv    a0, tp
	ret

.global store_hart_state_pointer
store_hart_state_pointer:
	mv tp, a0
	ret
