
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
|Devicetree|❌|Support for Devicetree parsing will be added in a future update. Currently only a fixed hardware layout is supported.
|Multi-platform|❌|Currently only supports QEMU's [*virt* platform](https://www.qemu.org/docs/master/system/riscv/virt.html).
|POSIX Compatibility|❌|POSIX compatibility is not planned.
