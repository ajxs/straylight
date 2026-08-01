#ifndef STRAYLIGHT_LIBC_INTERNAL_HEAP_H
#define STRAYLIGHT_LIBC_INTERNAL_HEAP_H

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

#define ALLOCATION_MINIMUM_ALIGNMENT 16

/**
 * The heap is an implicit list of contiguous blocks, each preceded by an
 * allocation header. Whether a block is free or allocated is encoded in the
 * header's checksum, which is calculated from the block's address, size, and
 * an identity marker denoting its allocation status. This mirrors the design
 * of the kernel heap allocator.
 */
typedef struct allocation_header_t
{
	uint32_t checksum;
	uint32_t reserved;
	size_t size;
} Allocation_Header;

#define ALLOCATION_HEADER_SIZE (sizeof(Allocation_Header))

_Static_assert(ALLOCATION_HEADER_SIZE % ALLOCATION_MINIMUM_ALIGNMENT == 0,
               "The allocation header size must be a multiple of the minimum "
               "alignment, or a block payload cannot share the block's "
               "alignment.");

typedef struct program_heap_t
{
	uintptr_t heap_start_address;
	size_t heap_size;
} Program_Heap;

uintptr_t allocate_memory(Program_Heap *program_heap, size_t size,
                          size_t alignment);

void free_memory(Program_Heap *program_heap, uintptr_t addr);

void initialise_heap(Program_Heap *program_heap, uintptr_t heap_start_address,
                     size_t heap_size);

/**
 * Tests whether the block at the provided address is a valid free block.
 * The address provided is that of the block's header, not its payload.
 */
bool is_block_free(uintptr_t block_address);

/**
 * Tests whether the block at the provided address is a valid allocated block.
 * The address provided is that of the block's header, not its payload.
 */
bool is_block_allocated(uintptr_t block_address);

#endif
