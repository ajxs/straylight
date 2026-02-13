###############################################################################
#  Copyright (c) 2025, Ajxs.
#  SPDX-License-Identifier: GPL-3.0-or-later
###############################################################################

.section .text

###############################################################################
# Extracts the high 32 bits from a 64-bit address
# Arguments:
# a0: Addr (64-bit address)
# Returns:
# a0: high 32 bits
###############################################################################
.global addresses_get_address_word_high
.type addresses_get_address_word_high, @function
addresses_get_address_word_high:
	srli   a0, a0, 32
	sext.w a0, a0
	ret

###############################################################################
# Extracts the low 32 bits from a 64-bit address
# Arguments:
# a0: Addr (64-bit address)
# Returns:
# a0: low 32 bits
###############################################################################
.global addresses_get_address_word_low
.type addresses_get_address_word_low, @function
addresses_get_address_word_low:
	sext.w a0, a0
	ret
