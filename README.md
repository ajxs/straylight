
# Straylight Kernel

Straylight is a 64-bit RISC-V hobby monolithic kernel written in Ada. The Straylight kernel is currently in *alpha* stages, and the only supported target is QEMU's [*virt* platform](https://www.qemu.org/docs/master/system/riscv/virt.html).

The codebase includes a test filesystem with several test userspace programs demonstrating the kernel's features.

The name *Straylight* comes from William Gibson's groundbreaking cyberpunk novel *Neuromancer*.

## Features
|Feature|Status|Description|
|--|--|--|
|Multitasking|✅|Pre-emptive multitasking.
|Graphics Devices|✅|VirtIO GPU support.
|Block Devices|✅|VirtIO Block Device support.
|Filesystem|✅|Supports mounting FAT16 and USTAR images.
|Userspace|✅|Supports loading ELF binaries written in C and Ada.
|SMP|❌|Currently no SMP support. This will be added in a future update.
|Multi-platform|❌|Currently only supports QEMU's [*virt* platform](https://www.qemu.org/docs/master/system/riscv/virt.html). Support for device discovery via the Devicetree standard will be added in a future update.
|POSIX Compatibility|—|Partial POSIX compatibility is planned. Support is currently minimal.

## Build

Once all the build dependencies are installed, to build the kernel executablea and userspace filesystem image(s), run `make` in the `src` directory. Alternatively, the `run` script in the repository's root directory will build and run the executable in qemu.

## Requirements

- Alire
- FSF GNAT `riscv64-elf` + GPRBuild
- OpenSBI v1.7+
- RISC-V Linux GCC (If building OpenSBI)
- QEMU v10.1.0+
- GNU MTools

### OpenSBI 

OpenSBI is included as a git submodule, and is built as part of the full build process. OpenSBI's build system expects a Linux-targeting toolchain (`riscv64-linux-gnu-`).