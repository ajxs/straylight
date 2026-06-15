#include "heap.h"
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

/**
 * Calculates the offset necessary to align the given address to the desired
 * alignment, while also factoring in the size of the allocation header.
 */
static size_t calculate_alignment_offset(uintptr_t address, size_t alignment)
{
	// Since a virtual address can only be 39 - bits in length, we can mask the
	// address to help avoid an arithmetic overflow.
	const uintptr_t masked_address = address & 0x7fffffffff;

	// The alignment offset needs to factor in the size of the allocation header,
	// since it's the final allocated address that needs to be aligned, not the
	// address of the allocation header.
	const size_t mod = (masked_address + sizeof(Allocation_Header)) % alignment;

	return mod == 0 ? 0 : alignment - mod;
}

static region_index_t find_unused_free_region_entry(Program_Heap *program_heap)
{
	for (region_index_t i = 0; i < MAX_FREE_REGIONS; i++)
	{
		if (!program_heap->free_region_list[i].entry_used)
		{
			return i;
		}
	}

	return NULL_REGION_INDEX;
}

static bool find_and_allocate_free_region(Program_Heap *program_heap,
                                          size_t size, size_t alignment,
                                          uintptr_t *allocated_address)
{
	size_t alignment_offset = 0;
	size_t remaining_size = 0;
	const size_t real_allocation_size = sizeof(Allocation_Header) + size;

	region_index_t current_region_index =
	    program_heap->free_region_list_head_index;
	region_index_t previous_region_index = NULL_REGION_INDEX;

	while (current_region_index != NULL_REGION_INDEX)
	{
		if (alignment != 1)
		{
			alignment_offset = calculate_alignment_offset(
			    program_heap->free_region_list[current_region_index].address,
			    alignment);
		}

		// If the current region is the same size and alignment as the desired
		// allocation size, it can be 'fully allocated' without subdividing the
		// free block.
		// Clear the current region, and 'attach' its previous node to its next
		// to 'remove' it from the list.
		if (program_heap->free_region_list[current_region_index].size ==
		        real_allocation_size &&
		    alignment_offset == 0)
		{
			program_heap->free_region_list[current_region_index].entry_used = false;

			// If there is a previous region set its next region pointer to the next
			// region pointer of the current entry.
			if (previous_region_index != NULL_REGION_INDEX)
			{
				program_heap->free_region_list[previous_region_index]
				    .next_region_index =
				    program_heap->free_region_list[current_region_index]
				        .next_region_index;
			}
			else
			{
				program_heap->free_region_list_head_index =
				    program_heap->free_region_list[current_region_index]
				        .next_region_index;
			}

			*allocated_address =
			    program_heap->free_region_list[current_region_index].address;

			return true;
		}

		// If the current region is larger than the desired allocation size, and
		// aligned, amend the current region so that its new size reflects the
		// alignment.
		// At the same time, move its start offset forward to reflect the
		// allocated region being at its start.
		if (program_heap->free_region_list[current_region_index].size >
		        real_allocation_size &&
		    alignment_offset == 0)
		{
			*allocated_address =
			    program_heap->free_region_list[current_region_index].address;

			program_heap->free_region_list[current_region_index].size -=
			    real_allocation_size;
			program_heap->free_region_list[current_region_index].address +=
			    real_allocation_size;

			return true;
		}

		// If the alignment doesn't match, but the current block is
		// large enough to fit the allocation (and the number of bytes
		// necessary to offset the start to make the block match the
		// desired alignment), then resize the current block to match the
		// amount necessary to get the desired alignment.
		// If there is any remaining size after this, insert a new region
		// with the remaining size after the allocation.
		if (program_heap->free_region_list[current_region_index].size >=
		    (real_allocation_size + alignment_offset))
		{
			remaining_size =
			    program_heap->free_region_list[current_region_index].size -
			    (real_allocation_size + alignment_offset);

			program_heap->free_region_list[current_region_index].size =
			    alignment_offset;

			// If there is any remaining size after the allocation, insert a new
			// region with the remaining size after the allocation.
			if (remaining_size > 0)
			{
				const region_index_t new_region_index =
				    find_unused_free_region_entry(program_heap);
				if (new_region_index == NULL_REGION_INDEX)
				{
					return false;
				}

				program_heap->free_region_list[new_region_index].entry_used = true;
				program_heap->free_region_list[new_region_index].address =
				    program_heap->free_region_list[current_region_index].address +
				    alignment_offset + real_allocation_size;

				program_heap->free_region_list[new_region_index].size = remaining_size;

				program_heap->free_region_list[new_region_index].next_region_index =
				    program_heap->free_region_list[current_region_index]
				        .next_region_index;

				program_heap->free_region_list[current_region_index].next_region_index =
				    new_region_index;
			}

			*allocated_address =
			    program_heap->free_region_list[current_region_index].address +
			    alignment_offset;

			return true;
		}

		previous_region_index = current_region_index;
		current_region_index =
		    program_heap->free_region_list[current_region_index].next_region_index;
	}

	// If this point is reached, the heap is too fragmented to find a free region
	// large enough to satisfy the allocation request.
	return false;
}

uintptr_t allocate_memory(Program_Heap *program_heap, size_t size,
                          size_t alignment)
{
	uintptr_t allocated_address = 0;
	if (!find_and_allocate_free_region(program_heap, size, alignment,
	                                   &allocated_address))
	{
		return 0;
	}

	Allocation_Header *allocation_header =
	    (Allocation_Header *)(allocated_address);
	allocation_header->identity = ALLOCATION_HEADER_IDENTITY;
	allocation_header->size = size;

	return allocated_address + sizeof(Allocation_Header);
}

static void coalesce_free_regions(Program_Heap *program_heap)
{
	region_index_t previous_region_index = NULL_REGION_INDEX;
	region_index_t current_region_index =
	    program_heap->free_region_list_head_index;

	while (current_region_index != NULL_REGION_INDEX)
	{
		if (previous_region_index != NULL_REGION_INDEX)
		{
			uintptr_t region_end_address =
			    program_heap->free_region_list[previous_region_index].address +
			    program_heap->free_region_list[previous_region_index].size;

			bool matched =
			    program_heap->free_region_list[current_region_index].address ==
			    region_end_address;

			if (matched)
			{
				program_heap->free_region_list[previous_region_index].size +=
				    program_heap->free_region_list[current_region_index].size;

				program_heap->free_region_list[previous_region_index]
				    .next_region_index =
				    program_heap->free_region_list[current_region_index]
				        .next_region_index;

				program_heap->free_region_list[current_region_index].entry_used = false;
			}
			else
			{
				previous_region_index = current_region_index;
			}
		}
		else
		{
			previous_region_index = current_region_index;
		}

		current_region_index =
		    program_heap->free_region_list[current_region_index].next_region_index;
	}
}

static region_index_t insert_free_region(Program_Heap *program_heap,
                                         uintptr_t new_address, size_t new_size)
{
	const region_index_t new_region_index =
	    find_unused_free_region_entry(program_heap);
	region_index_t current_region_index =
	    program_heap->free_region_list_head_index;
	region_index_t previous_region_index = NULL_REGION_INDEX;
	bool inserted_new_region = false;

	if (new_region_index == NULL_REGION_INDEX)
	{
		// No free region entries are available, so the region being freed cannot be
		// added to the free region list. This will cause it to be effectively
		// leaked, as there is no way to track it or reallocate it in the future.
		return NULL_REGION_INDEX;
	}

	program_heap->free_region_list[new_region_index].entry_used = true;
	program_heap->free_region_list[new_region_index].address = new_address;
	program_heap->free_region_list[new_region_index].size = new_size;
	program_heap->free_region_list[new_region_index].next_region_index =
	    NULL_REGION_INDEX;

	if (program_heap->free_region_list_head_index == NULL_REGION_INDEX)
	{
		program_heap->free_region_list_head_index = new_region_index;
		return new_region_index;
	}

	// Iterate through the list of free regions until one is found with a higher
	// starting offset. Add the new region _before_ this entry in the list to
	// perform an insertion sort. Ensuring all regions are sorted by offset into
	// the heap. This allows for easily coalescing adjacent entries.
	current_region_index = program_heap->free_region_list_head_index;

	while (current_region_index != NULL_REGION_INDEX)
	{
		if (program_heap->free_region_list[current_region_index].address >
		    new_address)
		{
			// The current region is after the newly inserted region, so the
			// newly inserted region should be before the current region in the
			// list. Update the next region pointer of the newly inserted region
			// to point to the current region, and update the next region pointer
			// of the previous region to point to the newly inserted region.
			program_heap->free_region_list[new_region_index].next_region_index =
			    current_region_index;

			if (previous_region_index != NULL_REGION_INDEX)
			{
				program_heap->free_region_list[previous_region_index]
				    .next_region_index = new_region_index;
			}
			else
			{
				// If the _current_ region's offset is higher than that of the new
				// entry, and there is no 'previous region', this means that the new
				// region belongs at the head of the list. Make the new region the
				// new list head. The 'current region' points to the original list
				// head.
				program_heap->free_region_list_head_index = new_region_index;
			}

			inserted_new_region = true;
			break;
		}

		previous_region_index = current_region_index;
		current_region_index =
		    program_heap->free_region_list[current_region_index].next_region_index;
	}

	if (!inserted_new_region)
	{
		// If the new region has not been inserted at this point, it means that
		// its offset is higher than all existing regions, so it belongs at the end
		// of the list. Update the next region pointer of the previous tail to point
		// to the new region, and set the next region pointer of the new region to
		// null.
		program_heap->free_region_list[previous_region_index].next_region_index =
		    new_region_index;
	}

	// After inserting the new free region 'Coalesce' all of the free region
	// entries. This will combine any adjacent regions into a single region entry.
	coalesce_free_regions(program_heap);

	return new_region_index;
}

void free_memory(Program_Heap *program_heap, uintptr_t addr)
{
	uintptr_t actual_region_address =
	    (uintptr_t)(addr - sizeof(Allocation_Header));

	Allocation_Header *allocation_header =
	    (Allocation_Header *)(actual_region_address);
	if (allocation_header->identity != ALLOCATION_HEADER_IDENTITY)
	{
		// The provided address is invalid, as the allocation header identity is
		// not correct.
		return;
	}

	allocation_header->identity = 0;

	size_t region_size = allocation_header->size + sizeof(Allocation_Header);

	insert_free_region(program_heap, actual_region_address, region_size);
}

void initialise_heap(Program_Heap *program_heap, uintptr_t heap_start_address,
                     size_t heap_size)
{
	program_heap->heap_start_address = heap_start_address;
	program_heap->free_region_list_head_index = NULL_REGION_INDEX;

	for (size_t i = 0; i < MAX_FREE_REGIONS; i++)
	{
		program_heap->free_region_list[i].entry_used = false;
		program_heap->free_region_list[i].address = 0;
		program_heap->free_region_list[i].size = 0;
		program_heap->free_region_list[i].next_region_index = NULL_REGION_INDEX;
	}

	// Initialise the free region list with a single region that encompasses the
	// entire heap.
	insert_free_region(program_heap, heap_start_address, heap_size);
}
