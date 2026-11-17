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
| `make emu-headless` | As `emu-log`, but no display window, and a reset exits QEMU. Use for unattended runs: `timeout 60 make emu-headless` |
| `make debug` | Build and run with GDB stub on port 1234, serial to `serial.log` |
| `make bios` | Build OpenSBI (requires `riscv64-linux-gnu-` toolchain) |
| `make clean` | Clean all build artifacts |
| `./run` | Convenience script: build + run from repo root |
| `./run --quick` | As above, but only rebuilds the kernel (clean kernel build) |
| `./run --quicker` | As `--quick`, but skips the kernel clean (incremental kernel build) |

When working on the kernel, use `./run` for the first build, to check that the full build works. After that, if you've only changed the kernel (not userspace, libc or libstraylight), use `./run --quick` or `./run --quicker` to save time.

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
`traps-entry.S` is the `stvec` handler. It saves context, determines user vs. supervisor mode, and calls either `Traps.Handle_User_Mode_Trap` or `Traps.Handle_Supervisor_Mode_Trap`. Timer interrupts trigger the scheduler; external interrupts are claimed from `Devices.Attached_System_Devices.Interrupt_Controller` (the PLIC) and dispatched to the relevant device driver. The PLIC's state maps each interrupt line to the device that raises it, and each hart to its supervisor-mode PLIC context. `Boot.Device_Discovery.Initialise_Devices` fills the line map from each device's `Interrupt_Parent` when it enables the device's interrupt line, and `Create_PLIC_Device` fills the contexts from the PLIC's `interrupts-extended` in the Devicetree.

### Processes & Scheduler
`Process_Control_Block_T` (`processes.ads`) holds registers, memory space, heap, kernel stack, and a linked-list `Next_Process` pointer. The process queue is a singly-linked list protected by a spinlock. The scheduler (`Processes.Scheduler`) is round-robin. Processes can be blocked waiting on a `Blocking_Channel_T` (a `Unsigned_64` identifier); `Wake_Processes_Waiting_For_Channel` unblocks them.

### Devices
`Devices.Attached_System_Devices.System_Devices` is the single fixed-size array of every device. Each `Device_T` slot is discriminated by `Driver_Type` (the driver), and holds a `Device_Common_State_T` (addresses, IRQ, spinlock) plus the state record owned by that driver, such as `Devices.UART.UART_Device_State_T`. Driver procedures take `(Device : Device_Common_State_T; State : <Driver>_Device_State_T)`, and callers pick the driver with a `case` on `Driver_Type`. A driver that refers to another device holds an `access Devices.Attached_System_Devices.Device_T`, declared through a `limited with`, as the console does for its backend.

`Boot.Device_Discovery.Initialise_Devices` creates a device for each Devicetree node the parser recognises. It uses `System_Devices` entries in creation order, counted by `Devices.Attached_System_Devices.Attached_System_Device_Count`:
1. The PLIC, first, because every device that raises interrupts points at it. Only the first PLIC is used, and any other is logged and ignored. A device gets the PLIC as its `Interrupt_Parent` only if its Devicetree `interrupt-parent` is the PLIC's phandle. That property can be inherited from an ancestor node. Any other device gets no `Interrupt_Parent`, and its interrupts are never enabled.
2. The other Devicetree devices, in Devicetree order. Virtio slots are probed for a device ID, and empty ones get no device.
3. The console (backed by the first UART) and the ramdisk.

Code refers to devices through saved `Device_Access` values, never array indices:
- `Interrupt_Controller` and `Graphics_Device` are in `Devices.Attached_System_Devices`, because code outside `Boot` uses them.
- `Console_Device` and `Root_Filesystem_Memory_Device` are in `Boot.Device_Discovery`. The console's backend UART is `Console_Device.all.Console.Backend`.
- Nothing keeps a reference to a block device. `Boot.Mount_Disks` finds them in `System_Devices` by `Driver_Type`.

A device must be created by assigning to `System_Devices (Index)` itself, with the index from `Boot.Device_Discovery.Allocate_System_Device_Index`. Through a `Device_Access`, the entry is constrained to its current `Driver_Type`, so assigning a new device through one raises `Constraint_Error`.

### Console
`Devices.Console` is a line-buffered terminal layer over a backend device. Only a serial backend is implemented. It is mounted at `/Devices/Console`, and libc opens it as `stdin`, `stdout`, and `stderr`.

Reads block until a line is complete. `Read_Line_Unlocked` does the input processing that a Linux tty layer would:
- CR is converted to LF, since terminals send CR for the Enter key.
- LF and printable ASCII (`0x20`–`0x7E`) are stored and echoed. Other bytes are dropped, so the line buffer always matches the screen.
- Backspace (`0x08` or `0x7F`) erases the last uncommitted byte.
- Ctrl+D ends the read without being stored. On an empty line this returns 0 bytes, which libc treats as EOF.

The line buffer is 4096 bytes. Its last byte is reserved for the newline, so input past that point is discarded. `Line_Read_Lock` allows one read at a time, so other readers sleep until the current `read` returns. This isn't fair: a process that keeps reading can starve the others.

### Filesystem
A root virtual filesystem (`Filesystems.Root`) sits at the top. Two real filesystems are mounted under `/Devices/`:
- `/Devices/Disk/` — FAT16 image (`Filesystems.FAT`, `Filesystems.FAT.FAT16`)
- `/Devices/Disk_B/` — UStar/tar image (`Filesystems.UStar`)

`Boot.Mount_Disks` works out which block device holds which filesystem by reading each device's first sector, so the order of the Virtio buses doesn't matter. It checks for a FAT boot sector signature (`Filesystems.FAT.Is_FAT_Boot_Sector`) and for a UStar header (`Filesystems.UStar.Is_UStar_Header`). It runs from `Boot.Start_Init_Process`, because reading a Virtio device needs a process context. Until then, the two disk mounts have no device.

`/Devices/` also holds a node for each device:
- `/Devices/Console` is added by hand in `Boot.Initialise_Filesystem`, before the other device nodes, so a full node table can't leave it out.
- `Boot.Populate_Device_Nodes` adds `/Devices/Serial/<N>`, `/Devices/Disks/<N>` and `/Devices/Graphics/<N>`, one per device. Each type is numbered from 0, in `System_Devices` order (Devicetree order). The numbers depend on the Devicetree, so kernel code should never name a device by its number.
- Only the console and serial nodes support reads and writes. The others return `Not_Supported`, which userspace sees as `-ENOTSUP`.
- The root filesystem has 32 nodes. If they run out, `Add_Filesystem_Node_To_Root_Filesystem` logs an error, and the remaining devices get no node.

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
Debug logging is switched on per subsystem by the `Debug_*` constants at the top of `src/kernel/src/logging.ads`, such as `Debug_Boot` and `Debug_Boot_Device_Discovery`. Set one to `True` to enable that subsystem's debug output.

- Write debug calls as `pragma Debug (Debug_X, Log_Debug ("..."));`. When the flag is `False`, the call is compiled out.
- `Log_Error` and `Log_Constraint_Error` aren't gated, so they always print.
- Don't pass `File` or `Line`. Their defaults record the caller's location.
- Each line is loosely logfmt: `hart= process= time= level= location= message=`. New fields inside a message should stay `key=value`.
- If the only thing that can raise `Constraint_Error` in a subprogram is a debug log message, put the log in its own small procedure with a handler, as `Boot.Log_Hart` does. Otherwise the build fails with the flag either on or off. Build with the flag both ways.

### Fixed-Length Strings
`Fixed_Length_String_T` (in `utilities.ads`) is used for kernel-internal strings with a known maximum length (e.g., filesystem node names). It stores both the max-length buffer and the actual `Byte_Length`.

### Language Standard
Ada 2022 (`-gnat2022`) with GNAT extensions (`-gnatX0`). Inline declarations after `begin` (declaring constants mid-body using the Ada 2022 `declare` expression form) are valid and used throughout the kernel.
