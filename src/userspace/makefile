#####################################################################
#  Copyright (c) 2025, Ajxs.
#  SPDX-License-Identifier: GPL-3.0-or-later
#####################################################################

.POSIX:
.DELETE_ON_ERROR:
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules

.PHONY: all clean debug emu

BUILD_DIR := build

PRINT_FRACTAL_DIR    := print_fractal_pattern
PRINT_FRACTAL_BINARY := ${PRINT_FRACTAL_DIR}/build/print_fractal_pattern.elf

PRINT_WORDS_DIR    := print_random_words
PRINT_WORDS_BINARY := ${PRINT_WORDS_DIR}/bin/print_random_words.elf

PRINT_MORE_WORDS_DIR := print_more_words
PRINT_MORE_WORDS_BINARY := ${PRINT_MORE_WORDS_DIR}/bin/print_more_words.elf

USERSPACE_DRIVE_IMG     := ${BUILD_DIR}/userspace.img
USERSPACE_DRIVE_TAR_IMG := ${BUILD_DIR}/userspace.tar.img

all: ${USERSPACE_DRIVE_IMG} ${USERSPACE_DRIVE_TAR_IMG}

clean:
	make -C "${PRINT_FRACTAL_DIR}" clean
	alr -C "${PRINT_WORDS_DIR}" clean
	alr -C "${PRINT_MORE_WORDS_DIR}" clean
	rm -rf ${BUILD_DIR}

${PRINT_FRACTAL_BINARY}: 
	make -C "${PRINT_FRACTAL_DIR}"

${PRINT_WORDS_BINARY}: 
	alr -C "${PRINT_WORDS_DIR}" build

${PRINT_MORE_WORDS_BINARY}:
	alr -C "${PRINT_MORE_WORDS_DIR}" build

${BUILD_DIR}:
	mkdir -p ${BUILD_DIR}

MTOOLS_FLAGS := MTOOLS_VFAT=1 MTOOLS_SKIP_CHECK=1

${USERSPACE_DRIVE_IMG}: ${BUILD_DIR} ${PRINT_FRACTAL_BINARY} ${PRINT_WORDS_BINARY} ${PRINT_MORE_WORDS_BINARY}
	dd if=/dev/zero of=${USERSPACE_DRIVE_IMG} bs=1M count=16
	sudo mkfs.fat -F 16 ${USERSPACE_DRIVE_IMG}
	mmd -i ${USERSPACE_DRIVE_IMG} "::/Programs"
	${MTOOLS_FLAGS} mcopy -i ${USERSPACE_DRIVE_IMG} ${PRINT_FRACTAL_BINARY} "::/Programs"
	${MTOOLS_FLAGS} mcopy -i ${USERSPACE_DRIVE_IMG} ${PRINT_WORDS_BINARY} "::/Programs"
	${MTOOLS_FLAGS} mcopy -i ${USERSPACE_DRIVE_IMG} ${PRINT_MORE_WORDS_BINARY} "::/Programs"
	${MTOOLS_FLAGS} mcopy -i ${USERSPACE_DRIVE_IMG} nine_letter_words.txt "::/"

${USERSPACE_DRIVE_TAR_IMG}: ${PRINT_FRACTAL_BINARY} ${PRINT_WORDS_BINARY}
	tar --format=ustar -cf ${USERSPACE_DRIVE_TAR_IMG} seven_letter_words.txt
