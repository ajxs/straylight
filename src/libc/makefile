#####################################################################
#  Copyright (c) 2019, AJXS.
#  This program is free software; you can redistribute it and/or modify it
#  under the terms of the GNU General Public License as published by the
#  Free Software Foundation; either version 3 of the License, or
#  (at your option) any later version.
#
#  Authors:
#     Anthony <ajxs [at] panoptic.online>
#####################################################################

.POSIX:
.DELETE_ON_ERROR:
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules

# Get the Alire compiler prefix for the RISC-V target.
ALIRE_TOOLCHAIN_PREFIX := $(shell \
	alr -C ../kernel printenv --unix \
	| awk -F= '/GNAT_RISCV64_ELF_ALIRE_PREFIX=/ {gsub(/"/,"",$$2); print $$2}')

TOOLCHAIN_PREFIX := $(ALIRE_TOOLCHAIN_PREFIX)/bin/riscv64-elf

CC := $(TOOLCHAIN_PREFIX)-gcc
AR := $(TOOLCHAIN_PREFIX)-ar


# Directories
SRC_DIR := src
OBJ_DIR := build
INC_DIR := ${SRC_DIR}/include

LIB := ${OBJ_DIR}/straylight_libc.a

# Flags
CFLAGS :=                   \
	-fno-stack-protector      \
	-ffreestanding            \
	-ggdb                     \
	-I$(INC_DIR)              \
	-std=gnu99                \
	-Wall                     \
	-Wextra

ASFLAGS  := -I$(INC_DIR)

# Sources
C_SRCS  := $(wildcard $(SRC_DIR)/*.c)
S_SRCS  := $(wildcard $(SRC_DIR)/*.S)

# Objects
C_OBJS  := $(C_SRCS:$(SRC_DIR)/%.c=$(OBJ_DIR)/%.o)
S_OBJS  := $(S_SRCS:$(SRC_DIR)/%.S=$(OBJ_DIR)/%.o)
OBJS    := $(C_OBJS) $(S_OBJS)

# Default target
all: $(LIB)

# Static library
$(LIB): $(OBJS)
	$(AR) $(ARFLAGS) $@ $^

# C compilation
$(OBJ_DIR)/%.o: $(SRC_DIR)/%.c | $(OBJ_DIR)
	$(CC) $(CFLAGS) -c $< -o $@

# Assembly compilation (.S = preprocessed)
$(OBJ_DIR)/%.o: $(SRC_DIR)/%.S | $(OBJ_DIR)
	$(CC) $(ASFLAGS) -c $< -o $@

# Ensure object directory exists
$(OBJ_DIR):
	mkdir -p $(OBJ_DIR)

# Cleanup
clean:
	rm -rf $(OBJ_DIR) $(LIB)

.PHONY: all clean

