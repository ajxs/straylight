#####################################################################
#  Copyright (c) 2025, Ajxs.
#  SPDX-License-Identifier: GPL-3.0-or-later
#####################################################################

.POSIX:
.DELETE_ON_ERROR:
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules

INC_DIR := ../../../libc/src/

CC      := gcc
CFLAGS  := \
	-Wall    \
	-Wextra  \
	-g       \
	-I${INC_DIR}

LDFLAGS := -lcunit

SRCS      := test_heap.c ../../../libc/src/heap.c
BUILD_DIR := build
TARGET    := ${BUILD_DIR}/heap_tests

.PHONY: all run clean

all: ${TARGET}

${TARGET}: ${SRCS} ${BUILD_DIR}
	${CC} ${CFLAGS} ${SRCS} ${LDFLAGS} -o ${TARGET}

${BUILD_DIR}:
	mkdir -p ${BUILD_DIR}

run: ${TARGET}
	./${TARGET}

clean:
	rm -f ${TARGET}
