#ifndef STRAYLIGHT_LIBC_STDDEF_H
#define STRAYLIGHT_LIBC_STDDEF_H 1

/*
 * Use compiler-provided builtin types.
 * These are target-correct and ABI-safe.
 */
typedef __SIZE_TYPE__ size_t;
typedef __PTRDIFF_TYPE__ ptrdiff_t;
typedef __WCHAR_TYPE__ wchar_t;

/*
 * NULL definition
 */
#ifndef NULL
#define NULL ((void *)0)
#endif

/*
 * offsetof — required by the C standard
 */
#define offsetof(type, member) __builtin_offsetof(type, member)

#endif
