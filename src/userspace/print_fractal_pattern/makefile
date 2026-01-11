#####################################################################
#  Copyright (c) 2025, Ajxs.
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

INCLUDE_DIRS := src/include ../../libstraylight/src/include ../../libc/src/include
INCLUDE_FLAG := ${foreach d, ${INCLUDE_DIRS}, -I$d}

LIBSTRAYLIGHT   := ../../libstraylight/lib/libstraylight.a
STRAYLIGHT_LIBC := ../../libc/build/straylight_libc.a

CFLAGS :=                   \
	-fno-stack-protector      \
	-ffreestanding            \
	-ggdb                     \
	${INCLUDE_FLAG}           \
	-std=gnu99                \
	-Wall                     \
	-Wextra

LDFLAGS :=            \
	-ffreestanding      \
	-nostdlib

LIBS := -lgcc ${STRAYLIGHT_LIBC} ${LIBSTRAYLIGHT}

AS_SOURCES :=

C_SOURCES  :=               \
	${SRC_DIR}/main.c

OBJECTS    := ${AS_SOURCES:.S=.o}
OBJECTS    += ${C_SOURCES:.c=.o}

BUILD_DIR     := build
KERNEL_BINARY := ${BUILD_DIR}/print_fractal_pattern.elf

.PHONY: all clean

all: ${KERNEL_BINARY}

${BUILD_DIR}:
	mkdir -p ${BUILD_DIR}

clean:
	rm -f ${OBJECTS}
	rm -rf ${BUILD_DIR}

${KERNEL_BINARY}: ${OBJECTS} ${BUILD_DIR}
	${CC} ${LDFLAGS} ${OBJECTS} -o ${KERNEL_BINARY} ${LIBS}

%.o: %.S
	${CC} ${INCLUDE_FLAG} -g -c $< -o $@ ${CFLAGS}

%.o: %.c
	${CC} ${INCLUDE_FLAG} -g -c $< -o $@ ${CFLAGS}
