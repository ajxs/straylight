/**
 *  Copyright (c) 2025, Ajxs.
 *  SPDX-License-Identifier: GPL-3.0-or-later
 */

/** 
 * The size of the boot stacks used for all harts. 
 * 8 stacks of 8KiB each are allocated.
 */
BOOT_STACKS_SIZE = 8K * 8;

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
		__boot_stacks_bottom = .;
		. += BOOT_STACKS_SIZE;
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
