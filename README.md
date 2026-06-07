
# Straylight Kernel

Straylight is a 64-bit RISC-V hobby monolithic kernel written in Ada. The Straylight kernel is currently in *alpha* stages, and the only supported target is QEMU's [*virt* platform](https://www.qemu.org/docs/master/system/riscv/virt.html).

The codebase includes a test filesystem with several test userspace programs demonstrating the kernel's features.

The name *Straylight* comes from William Gibson's groundbreaking cyberpunk novel *Neuromancer*.

## Features
|Feature|Status|Description|
|--|--|--|
|Multitasking|✅|Pre-emptive multitasking.
|Graphics Devices|✅|Virtio GPU support.
|Block Devices|✅|Virtio Block Device support.
|Filesystem|✅|Supports mounting FAT16 and UStar images.
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

## Statement on AI Usage

Straylight is developed with *the assistance of* AI. However Straylight is ***not*** *'vibe coded'*. Every line of code has been *hand-written* by a real human, with the *reasonable* assistance of various tools (IDEs, intellisense, autocomplete, etc.). AI *has* however been used to help review code, and as a [rubber duck](https://en.wikipedia.org/wiki/Rubber_duck_debugging). In case you're inclined not to trust me, you're welcome to try 'vibe coding' a kernel of your very own. Despite having an exhaustive knowledge of Unix internals, and being generally pretty good at spotting silly mistakes, AI tends to tie itself up in knots writing kernel code; Let alone writing kernel code in *Ada*. The *real* difficulties in building your own kernel lie in designing reliable, loosely-coupled subsystems, and managing the creeping complexity; Things AI doesn't necessarily help you with. Anyway, blindly letting AI do the hard work would remove all the joy of learning and creation. Which is the reason why I work on this project in the first place. 

If you have any concerns, or want any clarifications, please get in touch. - @ajxs (lead maintainer)
