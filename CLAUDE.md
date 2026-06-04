# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

Straylight is a 64-bit RISC-V hobby monolithic kernel written in Ada, targeting QEMU's `virt` platform. It supports pre-emptive multitasking, VirtIO block and GPU devices, FAT16 and UStar filesystems, and ELF userspace programs written in Ada or C.

## Build Commands

All build commands are run from the `src/` directory unless noted.

| Command | Effect |
|---|---|
| `make` | Full build: kernel + userspace disk images |
| `make kernel` | Kernel only |
| `make emu` | Build and run in QEMU (serial → stdio) |
| `make emu-log` | Build and run, serial output to `serial.log` |
| `make debug` | Build and run with GDB stub on port 1234, serial to `serial.log` |
| `make bios` | Build OpenSBI (requires `riscv64-linux-gnu-` toolchain) |
| `make clean` | Clean all build artifacts |
| `./run` | Convenience script: build + run from repo root |
| `./run --quick` | As above, but only rebuilds the kernel |

The kernel is built with Alire: `alr -C kernel build`. Userspace is built with `make -C userspace`. OpenSBI is a git submodule.

## Repository Structure

```
src/
  kernel/          Ada kernel (Alire project, straylight.gpr)
  libstraylight/   Ada userspace library (Alire project)
  libc/            Minimal C library for userspace (make)
  userspace/       Userspace programs + disk image creation (make)
  opensbi/         OpenSBI bootloader (git submodule)
_dev/              Development notes, debug scripts, incident logs
```

## Boot Sequence

`boot-entry.S` (`_start`) is the ELF entry point. The boot hart:
1. Sets up a temporary per-hart boot stack.
2. Calls `boot_initialise_boot_page_tables` (Ada) to build initial Sv39 page tables.
3. Enables paging, switches to the higher-half stack, calls `adainit` (Ada runtime init).
4. Jumps to `Boot.Kernel_Main` which: parses the Devicetree, initialises the PMM, sets up the kernel address space and heap, then calls `Boot.Initialise_Kernel_Services`.
5. `Initialise_Kernel_Services` initialises devices, the block cache, filesystems, and creates the init process, then calls `Processes.Scheduler.Run`.

Non-boot harts enter at `non_boot_hart_entry` (assembly), then call `Boot.Non_Boot_Hart_Entry`.

## Kernel Architecture

### Memory Layout (virtual)
- `0xFFFF_FFC0_0000_0000` — kernel image base (higher-half)
- `0xFFFF_FFC4_0000_0000` — kernel page pool
- `0xFFFF_FFC8_0000_0000` — kernel heap
- `0xFFFF_FFD8_0000_0000` — physical memory direct map
- `0xFFFF_FFE8_C000_0000` — device MMIO mappings
- `0xFFFF_FFEA_0000_0000` — kernel stacks area
- Higher-half offset: `0xFFFF_FFFF_0000_0000`

`Physical_Address_T` and `Virtual_Address_T` (a subtype of `System.Address`) are **distinct types** — the compiler enforces that they cannot be mixed. Use `Get_Lower_Physical_Address` / `Address_To_Unsigned_64` etc. to convert.

### Trap Handling
`traps-entry.S` is the `stvec` handler. It saves context, determines user vs. supervisor mode, and calls either `Traps.Handle_User_Mode_Trap` or `Traps.Handle_Supervisor_Mode_Trap`. Timer interrupts trigger the scheduler; external interrupts are claimed from the PLIC and dispatched to the relevant device driver.

### Processes & Scheduler
`Process_Control_Block_T` (`processes.ads`) holds registers, memory space, heap, kernel stack, and a linked-list `Next_Process` pointer. The process queue is a singly-linked list protected by a spinlock. The scheduler (`Processes.Scheduler`) is round-robin. Processes can be blocked waiting on a `Blocking_Channel_T` (a `Unsigned_64` identifier); `Wake_Processes_Waiting_For_Channel` unblocks them.

### Devices
`Devices.System_Devices` is a fixed-size array of `Device_T` (discriminated record: class + bus type). Devices are statically assigned array slots in `Boot.Initialise_Devices`:
1. PLIC (interrupt controller)
2. UART (serial)
3. VirtIO block (FAT16 disk, `virtio-mmio-bus.0`)
4. Ramdisk (root filesystem memory device)
5. VirtIO block B (UStar disk, `virtio-mmio-bus.1`)
6. VirtIO GPU (`virtio-mmio-bus.2`)

### Filesystem
A root virtual filesystem (`Filesystems.Root`) sits at the top. Two real filesystems are mounted under `/Devices/`:
- `/Devices/Disk/` — FAT16 image (`Filesystems.FAT`, `Filesystems.FAT.FAT16`)
- `/Devices/Disk_B/` — UStar/tar image (`Filesystems.UStar`)

A block cache (`Filesystems.Block_Cache`) and a filesystem node cache (`Filesystems.Node_Cache`) are shared across all filesystems.

### System Calls
Custom syscall numbers with the prefix `5446_0xxx` (defined in `system_calls.ads` and mirrored in `libstraylight/src/straylight.ads`). Arguments are passed in registers via `straylight_syscall` (assembly in `libstraylight`). Result is returned in `a0`; 0 = success, negative = error.

## Ada Conventions

### Error Handling
All procedures return errors via an `out Function_Result` parameter (defined in `function_results.ads`). Use `Is_Error (Result)` to check — any value `<= 0` is an error. `Success` is `9999_9999` (high Hamming distance from other values). Panicking is done with `Hart_State.Panic`.

The kernel restrictions (`kernel.adc`) disable exception propagation. Every package body catches `Constraint_Error` locally and calls `Panic`. The compiler determines whether a `Constraint_Error` is .possible within a particular function body.

Procedures with `out Function_Result` parameters are preferred over functions where a `Constraint_Error` is possible, and where no in-band way of handling the constraint exception exists.

### Ada Restrictions
`kernel.adc` disables: exceptions propagation, dynamic dispatch, finalization, allocators, tasking, protected types, fixed-point, IO, streams, and implicit heap allocations. Do not use these features in kernel code.

### Logging
Debug output is filtered by tag and level. To enable per-subsystem debug logging, set the relevant tag to `True` in the `Active_Logging_Tags` array in `src/kernel/src/logging.ads`. All subsystem packages declare a `Logging_Tags` constant passed to `Log_Debug` / `Log_Error`.

### Fixed-Length Strings
`Fixed_Length_String_T` (in `utilities.ads`) is used for kernel-internal strings with a known maximum length (e.g., filesystem node names). It stores both the max-length buffer and the actual `Byte_Length`.

### Language Standard
Ada 2022 (`-gnat2022`) with GNAT extensions (`-gnatX0`). Inline declarations after `begin` (declaring constants mid-body using the Ada 2022 `declare` expression form) are valid and used throughout the kernel.
