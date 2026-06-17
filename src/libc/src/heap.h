#ifndef STRAYLIGHT_LIBC_INTERNAL_HEAP_H
#define STRAYLIGHT_LIBC_INTERNAL_HEAP_H

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

#define MAX_FREE_REGIONS 128

#define NULL_REGION_INDEX UINT16_MAX

typedef uint16_t region_index_t;

typedef struct allocation_header_t
{
	uint32_t identity;
	size_t size;
} Allocation_Header;

#define ALLOCATION_HEADER_IDENTITY 0xABCDABCD

typedef struct free_region_t
{
	uintptr_t address;
	uint64_t size;
	region_index_t next_region_index;
	bool entry_used;
} Free_Region;

typedef struct program_heap_t
{
	Free_Region free_region_list[MAX_FREE_REGIONS];
	region_index_t free_region_list_head_index;
	uintptr_t heap_start_address;
} Program_Heap;

uintptr_t allocate_memory(Program_Heap *program_heap, size_t size,
                          size_t alignment);

void free_memory(Program_Heap *program_heap, uintptr_t addr);

void initialise_heap(Program_Heap *program_heap, uintptr_t heap_start_address,
                     size_t heap_size);

#endif
