#####################################################################
#  Copyright (c) 2026, Ajxs.
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

PRINT_RANDOM_WORDS_DIR 	:= print_random_words
PRINT_RANDOM_WORDS_BINARY := ${PRINT_RANDOM_WORDS_DIR}/build/print_random_words.elf

TEST_FILE_IO_DIR 	:= test_file_io
TEST_FILE_IO_BINARY := ${TEST_FILE_IO_DIR}/build/test_file_io.elf

USERSPACE_DRIVE_IMG     := ${BUILD_DIR}/userspace.img
USERSPACE_DRIVE_TAR_IMG := ${BUILD_DIR}/userspace.tar.img

all: ${USERSPACE_DRIVE_IMG} ${USERSPACE_DRIVE_TAR_IMG}

clean:
	make -C "${PRINT_FRACTAL_DIR}" clean
	make -C "${PRINT_RANDOM_WORDS_DIR}" clean
	make -C "${TEST_FILE_IO_DIR}" clean
	rm -rf ${BUILD_DIR}

${PRINT_FRACTAL_BINARY}: 
	make -C "${PRINT_FRACTAL_DIR}"

${PRINT_RANDOM_WORDS_BINARY}: 
	make -C "${PRINT_RANDOM_WORDS_DIR}"

${TEST_FILE_IO_BINARY}: 
	make -C "${TEST_FILE_IO_DIR}"

${BUILD_DIR}:
	mkdir -p ${BUILD_DIR}

MTOOLS_FLAGS := MTOOLS_VFAT=1 MTOOLS_SKIP_CHECK=1

${USERSPACE_DRIVE_IMG}: ${BUILD_DIR} ${PRINT_FRACTAL_BINARY} ${PRINT_RANDOM_WORDS_BINARY} ${TEST_FILE_IO_BINARY}
	dd if=/dev/zero of=${USERSPACE_DRIVE_IMG} bs=1M count=16
	mformat -i ${USERSPACE_DRIVE_IMG}
	mmd -i ${USERSPACE_DRIVE_IMG} "::/Programs"
	${MTOOLS_FLAGS} mcopy -i ${USERSPACE_DRIVE_IMG} ${PRINT_FRACTAL_BINARY} "::/Programs"
	${MTOOLS_FLAGS} mcopy -i ${USERSPACE_DRIVE_IMG} ${PRINT_RANDOM_WORDS_BINARY} "::/Programs"
	${MTOOLS_FLAGS} mcopy -i ${USERSPACE_DRIVE_IMG} ${TEST_FILE_IO_BINARY} "::/Programs"
	${MTOOLS_FLAGS} mcopy -i ${USERSPACE_DRIVE_IMG} nine_letter_words.txt "::/"
	${MTOOLS_FLAGS} mcopy -i ${USERSPACE_DRIVE_IMG} test_file.txt "::/"

${USERSPACE_DRIVE_TAR_IMG}:
	tar --format=ustar -cf ${USERSPACE_DRIVE_TAR_IMG} seven_letter_words.txt
