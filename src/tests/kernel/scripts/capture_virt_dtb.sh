#!/bin/sh
#  Captures the Devicetree blob that the kernel receives on QEMU's 'virt'
#  machine, and writes it to src/tests/kernel/data/virt.dtb for the
#  Devicetree host tests.
#
#  This is the DTB after OpenSBI has modified it, which adds the
#  'reserved-memory' node for OpenSBI's own memory. QEMU's 'dumpdtb' option
#  gives the DTB before that, so it can't be used here.
#
#  How: QEMU is started paused with a GDB stub, using the same flags as
#  'make emu-headless'. GDB stops at the kernel's entry point (_start), where
#  a1 holds the DTB's address, and dumps the memory there. The kernel hasn't
#  run yet, so nothing has overwritten the DTB.
#
#  Each capture differs only in /chosen/rng-seed, which QEMU randomises on
#  every boot, and which the parser ignores.
#
#  Needs: a full build ('make' in src/), and the riscv64-elf-gdb from the
#  kernel's Alire toolchain.
#  Run from anywhere: src/tests/kernel/scripts/capture_virt_dtb.sh
set -eu

SRC_DIR=$(cd "$(dirname "$0")/../../.." && pwd)
TESTS_DIR="$SRC_DIR/tests/kernel"
KERNEL_ELF="$SRC_DIR/kernel/bin/straylight.elf"
OUTPUT="$TESTS_DIR/data/virt.dtb"
WORK_DIR=$(mktemp -d)
trap 'rm -rf "$WORK_DIR"' EXIT

GDB=$(cd "$SRC_DIR/kernel" && alr exec -- sh -c 'command -v riscv64-elf-gdb')
ENTRY=$(nm "$KERNEL_ELF" | awk '$3 == "_start" { print "0x" $1 }')

cd "$SRC_DIR"
QEMU_COMMAND=$(make -s -n emu-headless QEMU_SERIAL_OUTPUT_DEBUG=null \
	| sed -n '/qemu-system-riscv64/,$p' | tr -d '\\' | tr -s ' \t\n' ' ')

# shellcheck disable=SC2086
$QEMU_COMMAND -S -gdb tcp::12345 > "$WORK_DIR/qemu.log" 2>&1 &
QEMU_PID=$!
sleep 2

"$GDB" -q -batch \
	-ex "target remote localhost:12345" \
	-ex "break *$ENTRY" \
	-ex "continue" \
	-ex "dump binary memory $WORK_DIR/raw.bin \$a1 \$a1 + 0x10000" \
	-ex "kill" > "$WORK_DIR/gdb.log" 2>&1 || true

kill "$QEMU_PID" 2> /dev/null || true

#  The dump is 64 KiB. Keep only the DTB, using the size in its header.
python3 - "$WORK_DIR/raw.bin" "$OUTPUT" << 'PYTHON'
import struct
import sys

raw_path, output_path = sys.argv[1], sys.argv[2]
data = open(raw_path, "rb").read()
magic, total_size = struct.unpack(">II", data[:8])
if magic != 0xD00DFEED:
    sys.exit("No DTB found: magic is 0x%08X" % magic)

with open(output_path, "wb") as output:
    output.write(data[:total_size])

print("Wrote %d bytes of DTB to %s" % (total_size, output_path))
PYTHON
