#####################################################################
#  Copyright (c) 2025, Ajxs.
#  SPDX-License-Identifier: GPL-3.0-or-later
#####################################################################

.POSIX:
.DELETE_ON_ERROR:
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules

.PHONY: all clean debug emu userspace test_disk

KERNEL_DIR      := kernel
KERNEL_BINARY   := ${KERNEL_DIR}/bin/straylight.elf

LIBSTRAYLIGHT_DIR    := libstraylight
LIBSTRAYLIGHT_BINARY := ${LIBSTRAYLIGHT_DIR}/lib/libstraylight.a

SERIAL_FILENAME := serial.log

USERSPACE_DIR   := userspace
USERSPACE_IMG   := ${USERSPACE_DIR}/build/userspace.img
USERSPACE_TAR_IMG   := ${USERSPACE_DIR}/build/userspace.tar.img

OPENSBI_BIN_DIR := /home/ajxs/src/opensbi/build/platform/generic/firmware
OPENSBI_BIN     := ${OPENSBI_BIN_DIR}/fw_jump.bin
BIOS            := ${OPENSBI_BIN}

HART_COUNT := 3

# @TODO: Why did I specify the VirtIO MMIO buses here? I can't recall now.
QEMU_FLAGS :=        \
	-bios ${BIOS}      \
	-cpu rv64          \
	-m 128M            \
	-machine virt      \
	-usb               \
	-smp ${HART_COUNT}  \
	-net none          \
	-global virtio-mmio.force-legacy=false                   \
	-device virtio-blk-device,drive=x0,bus=virtio-mmio-bus.0 \
	-device virtio-blk-device,drive=x1,bus=virtio-mmio-bus.1 \
	-device virtio-gpu-device,bus=virtio-mmio-bus.2 \
	-drive file=${USERSPACE_IMG},if=none,format=raw,id=x0  \
	-drive file=${USERSPACE_TAR_IMG},if=none,format=raw,id=x1  \
	-kernel ${KERNEL_BINARY}

QEMU_DEBUG_FLAGS := cpu_reset,int,guest_errors,in_asm

QEMU_DEBUG_SERIAL_OUTPUT := file:${SERIAL_FILENAME}
QEMU_SERIAL_OUTPUT       := stdio
# https://kashyapc.wordpress.com/2016/02/11/qemu-command-line-behavior-of-serial-stdio-vs-serial-monstdio/
# QEMU_SERIAL_OUTPUT       := mon:stdio
QEMU_LOG_FILE            := qemu.log

# all: ${KERNEL_BINARY}
all: ${KERNEL_BINARY} ${USERSPACE_IMG}

clean:
	alr -C "${KERNEL_DIR}" clean
	alr -C "${LIBSTRAYLIGHT_DIR}" clean
	make -C "${USERSPACE_DIR}" clean

debug: kernel
	qemu-system-riscv64      \
		${QEMU_FLAGS}          \
		-d ${QEMU_DEBUG_FLAGS} \
		-D ${QEMU_LOG_FILE}    \
		-S                     \
		-gdb tcp::1234         \
		-monitor stdio         \
		-serial ${QEMU_DEBUG_SERIAL_OUTPUT}

emu: kernel
	qemu-system-riscv64     \
		${QEMU_FLAGS}         \
		-serial ${QEMU_SERIAL_OUTPUT}

emu-log: kernel
	qemu-system-riscv64     \
		${QEMU_FLAGS}         \
		-serial ${QEMU_DEBUG_SERIAL_OUTPUT}

kernel: ${KERNEL_BINARY}

libstraylight: ${LIBSTRAYLIGHT_BINARY}

userspace: ${USERSPACE_IMG}

${KERNEL_BINARY}: ${USERSPACE_IMG} ${BOOT_RAMDISK_IMAGE}
	alr -C "${KERNEL_DIR}" build

${LIBSTRAYLIGHT_BINARY}:
	alr -C "${LIBSTRAYLIGHT_DIR}" build

${BUILD_DIR}:
	mkdir -p ${BUILD_DIR}

${USERSPACE_IMG}: libstraylight
	make -C "${USERSPACE_DIR}"
