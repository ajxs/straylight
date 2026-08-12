/**
 *  Copyright (c) 2025, Ajxs.
 *  SPDX-License-Identifier: GPL-3.0-or-later
 */

MAX_HART_COUNT = 8;

BOOT_STACK_SIZE = 8K;
BOOT_STACKS_RESERVED_SPACE = BOOT_STACK_SIZE * MAX_HART_COUNT;

/* 
 * Assert that the boot stack size definition in boot-entry.S matches the same
 * definition here.
 */
ASSERT (__assert_boot_stack_size == BOOT_STACK_SIZE,
	"BOOT_STACK_SIZE in boot-entry.S != BOOT_STACK_SIZE in kernel.ld")

HART_EMERGENCY_STACK_SIZE = 4K;
HART_EMERGENCY_STACK_RESERVED_SPACE = HART_EMERGENCY_STACK_SIZE * MAX_HART_COUNT;

KERNEL_PHYSICAL_MEMORY_ADDRESS = 0x80200000;
KERNEL_VIRTUAL_MEMORY_ADDRESS = 0xFFFFFFFF00000000;

ENTRY (_start)

SECTIONS
{
	. = KERNEL_PHYSICAL_MEMORY_ADDRESS;

	__boot_start = .;
	.boot_text : ALIGN (4K)
	{
		__boot_text_start = .;
		*(.boot_text*)
		__boot_text_end = .;
	}

	.boot_data : ALIGN (4K)
	{
		__boot_data_start = .;
		*(.boot_data*)

		. = ALIGN (4K);
		__boot_stacks_base = .;
		. += BOOT_STACKS_RESERVED_SPACE;
		__boot_stacks_top = .;

		. = ALIGN (4K);
		__boot_page_tables = .;
		. += (16 * 4K);
		__boot_page_tables_end = .;
		__boot_data_end = .;
	}

	.boot_rodata : ALIGN (4K)
	{
		__boot_rodata_start = .;
		*(.boot_rodata*)
		__boot_rodata_end = .;
	}

	. = ALIGN(4K);
	__boot_end = .;

	. += KERNEL_VIRTUAL_MEMORY_ADDRESS;

	__kernel_start = .;
	.text ALIGN (4K) : AT (ADDR (.text) - KERNEL_VIRTUAL_MEMORY_ADDRESS)
	{
		__text_start = .;
		*(.text*)
		__text_end = .;
	} 

	.rodata ALIGN (4K) : AT (ADDR (.rodata) - KERNEL_VIRTUAL_MEMORY_ADDRESS)
	{
		__rodata_start = .;
		*(.rodata*)
		*(.srodata*)
		__rodata_end = .;
	}


	.data ALIGN (4K) : AT (ADDR (.data) - KERNEL_VIRTUAL_MEMORY_ADDRESS)
	{
		__data_start = .;
		*(.data*)
		PROVIDE(__global_pointer$ = . + 0x800);
		__data_end = .;
	}


	.bss ALIGN (4K) : AT (ADDR (.bss) - KERNEL_VIRTUAL_MEMORY_ADDRESS)
	{
		__bss_start = .;
		*(COMMON)
		*(.bss*)
		*(.sbss*)

		. = ALIGN (4K);
		__hart_emergency_stacks_base = .;
		. += HART_EMERGENCY_STACK_RESERVED_SPACE;
		__hart_emergency_stacks_top = .;

		__bss_end = .;
	}

	. = ALIGN(4K);
	__kernel_end = .;

	/** 
	 * These are the physical memory addresses of the boot section, with the
	 * higher-half offset added. This is so that they are loadable from the 
	 * higher-half kernel code to 'free' the physical boot memory.
	 */
	__boot_start_offset = __boot_start + KERNEL_VIRTUAL_MEMORY_ADDRESS;
	__boot_end_offset = __boot_end + KERNEL_VIRTUAL_MEMORY_ADDRESS;

	__physical_memory_start = 0x80000000;
	__physical_memory_end = 0x80000000 + 128M;
}
