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

## Host Tests

The host tests are an AUnit program in `src/tests/kernel`, built with the native toolchain: run `alr build`, then `./bin/straylight_kernel_tests`, in that directory. The project compiles the kernel's own sources, with these replacements from `src/tests/kernel/src`:
- `locks.adb` and `logging.adb` replace the kernel's bodies of those packages.
- `Host_Stubs` exports the symbols that the kernel gets from its assembly files and linker script, so kernel packages that depend on them, such as `Filesystems`, link on the host. A routine that only makes sense on the target raises `Program_Error`.

A unit outside a package's family can't `with` that package's private child, so a suite for a private child has to be a child of the parent, such as `Filesystems.Paths_Test_Suite` for `Filesystems.Paths`.

## Formatting

The IDE formats files when they're saved: Ada with the Ada language server (the AdaCore VS Code extension), and C with clang-format. Code laid out any other way is reformatted the next time the user saves the file, which puts unrelated line changes into later diffs. So after changing files, format every one the change touched or created, then build again:
- Ada (`.ads` and `.adb`): `_dev/scripts/format-ada.py FILE...` from the repository root. It sends a formatting request to the same language server the editor uses, with the editor's settings (3-space indent), started through `alr exec` in the file's crate. `_dev/` is gitignored, so the script isn't in the repository.
- C: `clang-format -i FILE...`, which uses the repository's `.clang-format`.

## Repository Structure

```
src/
  kernel/          Ada kernel (Alire project, straylight.gpr)
  libstraylight/   Ada userspace library (Alire project)
  libc/            Minimal C library for userspace (make)
  userspace/       Userspace programs + disk image creation (make)
  tests/kernel/    Host unit tests for the kernel (Alire project, AUnit)
  opensbi/         OpenSBI bootloader (git submodule)
_dev/              Development notes, debug scripts, incident logs
```

## Boot Sequence

`boot-entry.S` (`_start`) is the ELF entry point. The boot hart:
1. Sets up a temporary per-hart boot stack.
2. Calls `boot_initialise_boot_page_tables` (Ada) to build initial Sv39 page tables.
3. Enables paging, switches to the higher-half stack, calls `adainit` (Ada runtime init).
4. Jumps to `Boot.Kernel_Main` which: parses the Devicetree, initialises the PMM, sets up the kernel address space and heap, then calls `Boot.Initialise_Kernel_Services`.
5. `Initialise_Kernel_Services` initialises devices (`Boot.Device_Discovery`), the block cache, and the root filesystem (`Boot.Filesystem_Setup`). It creates the init process (`Boot.Init`), then calls `Processes.Scheduler.Run`. The init process mounts the disks and loads the startup programs.

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

Each process has a `Working_Directory`, its current directory as an absolute path. `Create_New_Process` sets it to `/` explicitly, because a process record is created in place in kernel memory, so its default values aren't applied. The same goes for any field added to the record.

A process record is about 2.7 KB, which is too large, and should be shrunk in future. Most of it is `Memory_Space.Memory_Map`, a fixed table of `Maximum_Virtual_Memory_Mapping_Entries` (64) mappings, about 2 KB. `Working_Directory` is another 264 bytes. Allocating these from the kernel heap at the size each process needs would shrink the record.

### Devices
`Devices.Attached_System_Devices.System_Devices` is the single fixed-size array of every device. Each `Device_T` slot is discriminated by `Driver_Type` (the driver), and holds a `Device_Common_State_T` (addresses, IRQ, spinlock) plus the state record owned by that driver, such as `Devices.UART.UART_Device_State_T`. Driver procedures take `(Device : Device_Common_State_T; State : <Driver>_Device_State_T)`, and callers pick the driver with a `case` on `Driver_Type`. A driver that refers to another device holds an `access Devices.Attached_System_Devices.Device_T`, declared through a `limited with`, as the console does for its backend.

`Boot.Device_Discovery.Initialise_Devices` creates a device for each Devicetree node the parser recognises. It first sorts the parsed nodes by base address, lowest first, keeping Devicetree order for equal addresses, so devices are created in address order however the Devicetree lists them. It uses `System_Devices` entries in creation order, counted by `Devices.Attached_System_Devices.Attached_System_Device_Count`:
1. The PLIC, first, because every device that raises interrupts points at it. Only the first PLIC (the one with the lowest address) is used, and any other is logged and ignored. A device gets the PLIC as its `Interrupt_Parent` only if its Devicetree `interrupt-parent` is the PLIC's phandle. That property can be inherited from an ancestor node. Any other device gets no `Interrupt_Parent`, and its interrupts are never enabled.
2. The other Devicetree devices, in address order. Virtio slots are probed for a device ID, and empty ones get no device.
3. The console (backed by the UART with the lowest address) and the ramdisk.

Code refers to devices through saved `Device_Access` values, never array indices:
- `Interrupt_Controller` and `Graphics_Device` are in `Devices.Attached_System_Devices`, because code outside `Boot` uses them.
- `Console_Device` and `Root_Filesystem_Memory_Device` are in `Boot.Device_Discovery`. The console's backend UART is `Console_Device.all.Console.Backend`.
- Nothing keeps a reference to a block device. `Boot.Filesystem_Setup.Mount_Disk_Filesystems` finds them in `System_Devices` by `Driver_Type`.

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
A root virtual filesystem (`Filesystems.Root`) sits at the top. Disk filesystems are mounted under `/Volumes/<N>`. Two filesystem types are supported: FAT16 (`Filesystems.FAT`, `Filesystems.FAT.FAT16`) and UStar/tar (`Filesystems.UStar`).

`Boot.Filesystem_Setup.Mount_Disk_Filesystems` reads the first sector of each block device, and checks for a FAT boot sector signature (`Filesystems.FAT.Is_FAT_Boot_Sector`) or a UStar header (`Filesystems.UStar.Is_UStar_Header`). It mounts each one it recognises in the next free `Filesystems.Mounted_Filesystems` slot, and adds its `/Volumes/<N>` node. Other disks are skipped. It runs from `Boot.Init.Start_Init_Process`, because reading a Virtio device needs a process context.
- Volumes are numbered from 0 in `System_Devices` order. On QEMU, `/Volumes/0` is the FAT disk on Virtio bus 0 and `/Volumes/1` the UStar disk on bus 1. Kernel code should never name a volume by its number.
- Init reads the entries of `/Volumes` in order, and loads its startup programs from the first volume that has a `/Programs` directory. It panics if none does.
- A filesystem must be mounted at one path only. The node cache keys a filesystem's root directory entries by the mount node's index, so a second mount of the same filesystem would get its own cached nodes for those entries, which could go stale.

`/Devices/` also holds a node for each device:
- `/Devices/Console` is added by hand in `Boot.Filesystem_Setup.Initialise_Filesystem`, before the other device nodes, so a full node table can't leave it out.
- `Boot.Filesystem_Setup.Populate_Device_Nodes` adds `/Devices/Serial/<N>`, `/Devices/Disks/<N>` and `/Devices/Graphics/<N>`, one per device. Each type is numbered from 0, in `System_Devices` order (address order). The numbers depend on the Devicetree, so kernel code should never name a device by its number.
- Only the console and serial nodes support reads and writes. The others return `Not_Supported`, which userspace sees as `-ENOTSUP`.
- The root filesystem has 32 nodes. If they run out, `Add_Filesystem_Node_To_Root_Filesystem` logs an error, and the remaining devices get no node.

`Filesystems.Read_Next_Directory_Entry` reads the name and node type of the next entry in a directory opened with `Open_File`, like POSIX `readdir` with its `d_type`:
- The handle's `Position` keeps the place between calls, so `Seek_File` to 0 starts again. In the root filesystem, the position is the node index of the last entry read.
- At the end it returns `End_Of_Directory`, which isn't an error.
- Only the root filesystem supports it. On a FAT or UStar directory, including a mount point, it returns `Not_Supported`.

`Filesystems.Paths`, a private child, handles the text of paths and never looks one up. `Get_Next_Path_Component` steps through a path's components, and `Split_Path` splits off the last component's name from its parent directory, for `Create_File`. `Resolve_Path` turns a path into an absolute one: a relative path is joined onto a working directory, `.` is dropped, `..` removes the component before it and stays at `/`, and repeated or trailing separators go. That's exact because there are no symbolic links. An empty path is `Invalid_Filename`, and a result that doesn't fit the caller's buffer is `Path_Too_Long`.

`Filesystems.Find_File` starts every absolute path at the root directory's node, which is named `/` and is the only node with no parent, so `/` opens like any other directory. Each path component is looked up by `Find_Child_Node`: first in the node cache, then by the filesystem that holds the directory.
- Inside path lookup, a missing entry is `Success` with a null node, in the filesystem drivers and in `Find_Child_Node`. FAT's directory-entry search (`Search_FAT_Directory_Entries_For_File`) works the same way, so its callers test the node, not the result.
- `File_Not_Found` is an error. `Find_File` returns it when the walk ends without a node, and `Open_File` and `Create_File` pass it on. The open system call maps it to `-ENOENT`.

A block cache (`Filesystems.Block_Cache`) and a filesystem node cache (`Filesystems.Node_Cache`) are shared across all filesystems.

### System Calls
Custom syscall numbers with the prefix `5446_0xxx` (defined in `system_calls.ads` and mirrored in `libstraylight/src/straylight.ads`). Arguments are passed in registers via `straylight_syscall` (assembly in `libstraylight`). Result is returned in `a0`; 0 = success, negative = error.

## Ada Conventions

### Error Handling
All procedures return errors via an `out Function_Result` parameter (defined in `function_results.ads`). Use `Is_Error (Result)` to check — any value `<= 0` is an error. `Success` is `9999_9999` (high Hamming distance from other values). Panicking is done with `Hart_State.Panic`.

The kernel restrictions (`kernel.adc`) disable exception propagation. Every package body catches `Constraint_Error` locally and calls `Panic`. The compiler determines whether a `Constraint_Error` is .possible within a particular function body.

Procedures with `out Function_Result` parameters are preferred over functions where a `Constraint_Error` is possible, and where no in-band way of handling the constraint exception exists.

- Make `Result := Success;` the last statement, never the first, so that a path that forgets to set `Result` doesn't report success. If an early `return` can happen before every path has set `Result`, also set `Result := Unset;` at the top. `Unset` is 0, so `Is_Error` treats it as an error.
- Check each call that can fail straight after it, with `if Is_Error (Result) then return; end if;`. Don't share one check after an `if`/`elsif` chain.
- Log an error once, in the procedure where it happens, then pass the `Function_Result` back up. Callers check it and return without logging it again. Context that only the caller knows, such as a path it built, goes in a `pragma Debug` line. A caller can still decide what a failure means, such as whether to `Panic`.

### Ada Restrictions
`kernel.adc` disables: exceptions propagation, dynamic dispatch, finalization, allocators, tasking, protected types, fixed-point, IO, streams, and implicit heap allocations. Do not use these features in kernel code.

### Logging
Debug logging is switched on per subsystem by the `Debug_*` constants at the top of `src/kernel/src/logging.ads`, such as `Debug_Boot` and `Debug_Boot_Device_Discovery`. Set one to `True` to enable that subsystem's debug output.

- Write debug calls as `pragma Debug (Debug_X, Log_Debug ("..."));`. When the flag is `False`, the call is compiled out.
- `Log_Error` and `Log_Constraint_Error` aren't gated, so they always print.
- Don't pass `File` or `Line`. Their defaults record the caller's location.
- Each line is loosely logfmt: `hart= process= time= level= location= message=`. New fields inside a message should stay `key=value`.
- If the only thing that can raise `Constraint_Error` in a subprogram is a debug log message, put the log in its own small procedure with a handler, as `Boot.Log_Hart` does. Otherwise the build fails with the flag either on or off. Build with the flag both ways.

### Code Style
- A procedure does only what its name says. Put new behaviour at the level that owns the decision, not in whichever helper is convenient. For example, `Devicetree.Parse_Reg_Property` only decodes `reg` into entries, and `Finalise_Node_Properties` decides whether they become memory regions, reserved regions or a device.
- When a loop handles only the items that match a condition, use an Ada 2022 iterator filter instead of an `if` around the whole loop body. The `when` filter also works in quantified expressions, aggregates and `'Reduce`. Don't rewrite existing loops just for this, unless the code is already being changed.
  ```ada
  for Device of
    System_Devices (System_Devices'First .. Attached_System_Device_Count)
    when Device.Driver_Type = Driver_Type_Virtio_Block
  loop
  ```
- In comments, in all code including C, don't use the word "shortcut", and don't refer to plans: no `_dev/*-plan.md` files, phases or plan items. To record a known limit, describe the limit and what would fix it in plain words:
  ```ada
  --  The console's backend is the first UART. Only a serial backend is
  --  implemented, so the console can't be created without one.
  --  In future, this should be replaced with a lookup of the Devicetree's
  --  /chosen/stdout-path property to determine the console backend.
  ```

### Fixed-Length Strings
`Fixed_Length_String_T` (in `utilities.ads`) is used for kernel-internal strings with a known maximum length (e.g., filesystem node names). It stores both the max-length buffer and the actual `Byte_Length`.

### Language Standard
Ada 2022 (`-gnat2022`) with GNAT extensions (`-gnatX0`). Inline declarations after `begin` (declaring constants mid-body using the Ada 2022 `declare` expression form) are valid and used throughout the kernel.
