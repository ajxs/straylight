###############################################################################
#  Copyright (c) 2026, Ajxs.
#  SPDX-License-Identifier: GPL-3.0-or-later
###############################################################################

.section .text

###############################################################################
# Converts a 32-bit big-endian unsigned integer to little-endian
# Arguments:
# a0: BEU32 (32-bit big-endian value)
# Returns:
# a0: LEU32 (32-bit little-endian value)
###############################################################################
.global utilities_convert_beu32_to_leu32
.type utilities_convert_beu32_to_leu32, @function
utilities_convert_beu32_to_leu32:
	# Extract byte 0 (LSB) and shift to MSB position
	andi  t0, a0, 0xff
	slli  t0, t0, 24
	
	# Extract byte 1 and shift to position 2
	li    t2, 0xff00
	and   t1, a0, t2
	slli  t1, t1, 8
	or    t0, t0, t1
	
	# Extract byte 2 and shift to position 1
	srli  t1, a0, 8
	li    t2, 0xff00
	and   t1, t1, t2
	or    t0, t0, t1
	
	# Extract byte 3 (MSB) and shift to LSB position
	srli  t1, a0, 24
	andi  t1, t1, 0xff
	or    a0, t0, t1
	
	# Ensure the result is sign-extended, to match the RISC-V ABI.
	sext.w a0, a0

	ret

###############################################################################
# Converts a 64-bit big-endian unsigned integer to little-endian
# Arguments:
# a0: BEU64 (64-bit big-endian value)
# Returns:
# a0: LEU64 (64-bit little-endian value)
###############################################################################
.global utilities_convert_beu64_to_leu64
.type utilities_convert_beu64_to_leu64, @function
utilities_convert_beu64_to_leu64:
	# Extract byte 0 (LSB) and shift to MSB position
	andi   t0, a0, 0xff
	slli   t0, t0, 56
	
	# Extract byte 1 and shift to position 6
	srli   t1, a0, 8
	andi   t1, t1, 0xff
	slli   t1, t1, 48
	or     t0, t0, t1
	
	# Extract byte 2 and shift to position 5
	srli   t1, a0, 16
	andi   t1, t1, 0xff
	slli   t1, t1, 40
	or     t0, t0, t1
	
	# Extract byte 3 and shift to position 4
	srli   t1, a0, 24
	andi   t1, t1, 0xff
	slli   t1, t1, 32
	or     t0, t0, t1
	
	# Extract byte 4 and shift to position 3
	srli   t1, a0, 32
	andi   t1, t1, 0xff
	slli   t1, t1, 24
	or     t0, t0, t1
	
	# Extract byte 5 and shift to position 2
	srli   t1, a0, 40
	andi   t1, t1, 0xff
	slli   t1, t1, 16
	or     t0, t0, t1
	
	# Extract byte 6 and shift to position 1
	srli   t1, a0, 48
	andi   t1, t1, 0xff
	slli   t1, t1, 8
	or     t0, t0, t1
	
	# Extract byte 7 (MSB) and shift to LSB position
	srli   t1, a0, 56
	or     a0, t0, t1
	
	ret
