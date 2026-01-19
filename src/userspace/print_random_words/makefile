#####################################################################
#  Copyright (c) 2026, Ajxs.
#  SPDX-License-Identifier: GPL-3.0-or-later
#####################################################################

.POSIX:
.DELETE_ON_ERROR:
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules

# Get the Alire compiler prefix for the RISC-V target.
ALIRE_TOOLCHAIN_PREFIX := $(shell \
	alr -C ../../kernel printenv --unix \
	| awk -F= '/GNAT_RISCV64_ELF_ALIRE_PREFIX=/ {gsub(/"/,"",$$2); print $$2}')

CC := $(ALIRE_TOOLCHAIN_PREFIX)/bin/riscv64-elf-gcc

SRC_DIR := src

SYSROOT_DIR       := ../../libc/

LIBSTRAYLIGHT_DIR := ../../libstraylight
LIBC_DIR          := ${SYSROOT_DIR}/lib

INCLUDE_DIRS := src/include ${LIBSTRAYLIGHT_DIR}/src/include
INCLUDE_FLAG := ${foreach d, ${INCLUDE_DIRS}, -I$d}

LIBSTRAYLIGHT   := libstraylight

CFLAGS :=                   \
	--sysroot=${SYSROOT_DIR}  \
	-nostdlib                 \
	${INCLUDE_FLAG}           \
	-ggdb                     \
	-std=gnu99                \
	-Wall                     \
	-Wextra

LDFLAGS := -nostdlib

LIBS := \
	-L${LIBC_DIR}              \
	-L${LIBSTRAYLIGHT_DIR}/lib \
	-lc                        \
	-l${LIBSTRAYLIGHT}

AS_SOURCES :=

C_SOURCES  :=               \
	${SRC_DIR}/print_random_words.c

OBJECTS    := ${AS_SOURCES:.S=.o}
OBJECTS    += ${C_SOURCES:.c=.o}

BUILD_DIR     := build
KERNEL_BINARY := ${BUILD_DIR}/print_random_words.elf

.PHONY: all clean

all: ${KERNEL_BINARY}

${BUILD_DIR}:
	mkdir -p ${BUILD_DIR}

clean:
	rm -f ${OBJECTS}
	rm -rf ${BUILD_DIR}

${KERNEL_BINARY}: ${OBJECTS} ${BUILD_DIR}
	${CC}      \
	${LDFLAGS} \
	${LIBC_DIR}/crt0.o \
	${OBJECTS} -o ${KERNEL_BINARY} ${LIBS}

%.o: %.S
	${CC} ${INCLUDE_FLAG} -g -c $< -o $@ ${CFLAGS}

%.o: %.c
	${CC} ${INCLUDE_FLAG} -g -c $< -o $@ ${CFLAGS}
