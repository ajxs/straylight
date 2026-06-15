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

SRCS    := test_heap.c ../../../libc/src/heap.c
TARGET  := heap_tests

.PHONY: all run clean

all: ${TARGET}

${TARGET}: ${SRCS}
	${CC} ${CFLAGS} ${SRCS} ${LDFLAGS} -o ${TARGET}

run: ${TARGET}
	./${TARGET}

clean:
	rm -f ${TARGET}
