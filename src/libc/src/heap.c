#include "heap.h"
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

#define ALLOCATION_IDENTITY_FREE 0xAAAA5555
#define ALLOCATION_IDENTITY_ALLOCATED 0x5555AAAA

static uint32_t calculate_header_checksum(uint32_t block_identity,
                                          uintptr_t block_address,
                                          size_t block_size)
{
	const uint64_t address_bits = (uint64_t)block_address;
	const uint64_t size_bits = (uint64_t)block_size;

	return block_identity ^ (uint32_t)address_bits ^
	       (uint32_t)(address_bits >> 32) ^ (uint32_t)size_bits ^
	       (uint32_t)(size_bits >> 32);
}

static bool test_header_checksum(uint32_t block_identity,
                                 uintptr_t block_address)
{
	const Allocation_Header *header = (const Allocation_Header *)block_address;

	return header->checksum ==
	       calculate_header_checksum(block_identity, block_address, header->size);
}

bool is_block_free(uintptr_t block_address)
{
	return test_header_checksum(ALLOCATION_IDENTITY_FREE, block_address);
}

bool is_block_allocated(uintptr_t block_address)
{
	return test_header_checksum(ALLOCATION_IDENTITY_ALLOCATED, block_address);
}

static bool is_valid_header(uintptr_t block_address)
{
	return is_block_free(block_address) || is_block_allocated(block_address);
}

static bool is_valid_alignment(size_t alignment)
{
	return alignment > 0 && (alignment & (alignment - 1)) == 0;
}

/**
 * Tests whether an address is within the heap, and can hold a block header.
 */
static bool is_valid_header_address(const Program_Heap *program_heap,
                                    uintptr_t address)
{
	return program_heap->heap_size >= ALLOCATION_HEADER_SIZE &&
	       address >= program_heap->heap_start_address &&
	       address <= (program_heap->heap_start_address +
	                   program_heap->heap_size - ALLOCATION_HEADER_SIZE);
}

/**
 * Tests whether an address is valid for an allocation's payload.
 * This only tests that the address is within the bounds of the heap. It can't
 * determine whether the allocation is within a valid block.
 */
static bool is_allocated_address_in_heap(const Program_Heap *program_heap,
                                         uintptr_t address)
{
	return address >=
	           (program_heap->heap_start_address + ALLOCATION_HEADER_SIZE) &&
	       address < (program_heap->heap_start_address + program_heap->heap_size);
}

/**
 * Writes a block header, calculating its checksum from the block's address,
 * size, and its allocation status.
 */
static void set_block_header(uintptr_t block_address, size_t block_size,
                             bool block_is_free)
{
	Allocation_Header *header = (Allocation_Header *)block_address;

	header->size = block_size;
	header->reserved = 0;
	header->checksum = calculate_header_checksum(
	    block_is_free ? ALLOCATION_IDENTITY_FREE : ALLOCATION_IDENTITY_ALLOCATED,
	    block_address, block_size);
}

uintptr_t allocate_memory(Program_Heap *program_heap, size_t size,
                          size_t alignment)
{
	// From the POSIX spec:
	// "If the size of the space requested is 0, the behavior is
	// implementation-defined: either a null pointer shall be returned, or the
	// behavior shall be as if the size were some non-zero value, except that the
	// behavior is undefined if the returned pointer is used to access an object"
	if (size == 0 || size > program_heap->heap_size ||
	    !is_valid_alignment(alignment))
	{
		return 0;
	}

	// Ensure the alignment is at least the minimum required to align the header.
	if (alignment < ALLOCATION_MINIMUM_ALIGNMENT)
	{
		alignment = ALLOCATION_MINIMUM_ALIGNMENT;
	}

	// Round size up to the nearest multiple of the minimum alignment.
	size = (size + ALLOCATION_MINIMUM_ALIGNMENT - 1) &
	       ~(ALLOCATION_MINIMUM_ALIGNMENT - 1);

	uintptr_t current_block_address = program_heap->heap_start_address;

	while (is_valid_header_address(program_heap, current_block_address))
	{
		Allocation_Header *current_block =
		    (Allocation_Header *)current_block_address;

		// If an invalid block header is encountered, the heap has been
		// corrupted, so there's no safe way to continue walking it.
		if (!is_valid_header(current_block_address))
		{
			return 0;
		}

		if (is_block_free(current_block_address) && current_block->size >= size)
		{
			// If the current block is big enough to satisfy the allocation request,
			// check if we can split the block into two blocks. With the tail block
			// being the allocated block.
			// First check whether we can align the new second block address
			// downwards to satisfy the alignment requirement while still keeping
			// enough space within the original block to be valid.
			// If not, check whether we can allocate from the start of the
			// current block.
			const uintptr_t end_of_current_block =
			    current_block_address + ALLOCATION_HEADER_SIZE + current_block->size;

			// Note that it's the payload address that needs to be aligned, not
			// the address of the block header.
			const size_t alignment_offset = (end_of_current_block - size) % alignment;

			const uintptr_t new_block_data_address =
			    end_of_current_block - size - alignment_offset;

			const uintptr_t new_block_address =
			    new_block_data_address - ALLOCATION_HEADER_SIZE;

			// Test whether the space remaining in the original block is large
			// enough to hold a valid block. This is equivalent to testing that
			// the remaining size exceeds the size of a block header.
			if (new_block_data_address >
			    (current_block_address + 3 * ALLOCATION_HEADER_SIZE))
			{
				const size_t remaining_size_in_original_block =
				    new_block_address - current_block_address - ALLOCATION_HEADER_SIZE;

				set_block_header(current_block_address,
				                 remaining_size_in_original_block, true);

				// Take into account that the alignment offset by which this block
				// was moved back may have left some space at the end of the block
				// that is too small to hold a valid block. If so, include that
				// space in the size of the new block.
				const size_t effective_size =
				    size +
				    (alignment_offset <= ALLOCATION_HEADER_SIZE ? alignment_offset : 0);

				set_block_header(new_block_address, effective_size, false);

				// If moving the second block back to satisfy alignment left enough
				// space for a new block at the end of this block, create it.
				if (alignment_offset > ALLOCATION_HEADER_SIZE)
				{
					set_block_header(new_block_data_address + size,
					                 alignment_offset - ALLOCATION_HEADER_SIZE, true);
				}

				return new_block_data_address;
			}
			else if (((current_block_address + ALLOCATION_HEADER_SIZE) % alignment) ==
			         0)
			{
				// If the remaining space in the first block is too small to hold a
				// valid block, but the current block is already aligned, allocate
				// from the start of the current block.
				// If enough space is left over in the current block, create a new
				// free block at its end.
				const uintptr_t allocated_address =
				    current_block_address + ALLOCATION_HEADER_SIZE;

				size_t allocated_block_size = current_block->size;

				if (current_block->size > (size + 2 * ALLOCATION_HEADER_SIZE))
				{
					const size_t remaining_size =
					    current_block->size - size - ALLOCATION_HEADER_SIZE;

					allocated_block_size = size;

					set_block_header(allocated_address + size, remaining_size, true);
				}

				set_block_header(current_block_address, allocated_block_size, false);

				return allocated_address;
			}
		}

		// Move to check the next block in the heap.
		current_block_address += ALLOCATION_HEADER_SIZE + current_block->size;
	}

	// If this point is reached, there's no free block large enough to satisfy
	// the allocation request.
	return 0;
}

/**
 * Walks the heap, merging any adjacent free blocks into a single block.
 */
static void coalesce_free_blocks(Program_Heap *program_heap)
{
	uintptr_t current_block_address = program_heap->heap_start_address;

	while (is_valid_header_address(program_heap, current_block_address))
	{
		Allocation_Header *current_block =
		    (Allocation_Header *)current_block_address;

		if (!is_valid_header(current_block_address))
		{
			// The heap has been corrupted.
			return;
		}

		const uintptr_t next_block_address =
		    current_block_address + ALLOCATION_HEADER_SIZE + current_block->size;

		// If the next block lies outside the heap, we're done.
		if (!is_valid_header_address(program_heap, next_block_address))
		{
			return;
		}

		const Allocation_Header *next_block =
		    (const Allocation_Header *)next_block_address;

		if (!is_valid_header(next_block_address))
		{
			return;
		}

		if (is_block_free(current_block_address) &&
		    is_block_free(next_block_address))
		{
			// If the current block and the next block are both free, merge them
			// into a single block.
			set_block_header(current_block_address,
			                 current_block->size + ALLOCATION_HEADER_SIZE +
			                     next_block->size,
			                 true);
		}
		else
		{
			// Otherwise move to check the next block in the heap.
			current_block_address = next_block_address;
		}
	}
}

void free_memory(Program_Heap *program_heap, uintptr_t addr)
{
	if (addr == 0 || !is_allocated_address_in_heap(program_heap, addr))
	{
		return;
	}

	const uintptr_t block_address = addr - ALLOCATION_HEADER_SIZE;

	// The provided address is invalid if the block preceding it is not a valid
	// allocated block.
	if (!is_block_allocated(block_address))
	{
		return;
	}

	Allocation_Header *block = (Allocation_Header *)block_address;

	set_block_header(block_address, block->size, true);

	coalesce_free_blocks(program_heap);
}

void initialise_heap(Program_Heap *program_heap, uintptr_t heap_start_address,
                     size_t heap_size)
{
	program_heap->heap_start_address = heap_start_address;
	program_heap->heap_size = heap_size;

	if (heap_size <= ALLOCATION_HEADER_SIZE)
	{
		// The heap is too small to hold a single valid block.
		program_heap->heap_size = 0;

		return;
	}

	// Initialise the heap with a single free block spanning the whole heap.
	set_block_header(heap_start_address, heap_size - ALLOCATION_HEADER_SIZE,
	                 true);
}
