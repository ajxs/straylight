#ifndef STRAYLIGHT_LIBC_STDINT_H
#define STRAYLIGHT_LIBC_STDINT_H 1

// GCC's own stdint.h includes the C library's stdint.h with #include_next
// when it isn't compiling with -ffreestanding, so libc has to provide this
// file. GCC's stdint-gcc.h has the full set of definitions.
#include <stdint-gcc.h>

#endif
