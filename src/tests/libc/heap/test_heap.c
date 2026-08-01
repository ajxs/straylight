#include <CUnit/Basic.h>
#include <CUnit/CUnit.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>

#include <heap.h>

#define TEST_HEAP_SIZE 4096

static uint8_t heap_backing[TEST_HEAP_SIZE] __attribute__((aligned(64)));
static Program_Heap heap;

static void reset_heap(void)
{
	memset(heap_backing, 0, TEST_HEAP_SIZE);
	initialise_heap(&heap, (uintptr_t)heap_backing, TEST_HEAP_SIZE);
}

static uintptr_t heap_start(void) { return (uintptr_t)heap_backing; }

static uintptr_t heap_end(void)
{
	return (uintptr_t)heap_backing + TEST_HEAP_SIZE;
}

static const Allocation_Header *header_of(uintptr_t ptr)
{
	return (const Allocation_Header *)(ptr - ALLOCATION_HEADER_SIZE);
}

/**
 * Walks the heap, and counts the problems it finds. Every block header must be
 * aligned and valid, and the blocks must tile the heap exactly.
 */
static int audit_heap_blocks(void)
{
	uintptr_t block_address = heap_start();
	int problems = 0;

	while (block_address < heap_end())
	{
		if ((block_address % _Alignof(Allocation_Header)) != 0)
		{
			problems++;
		}

		if (!is_block_free(block_address) && !is_block_allocated(block_address))
		{
			// The heap is corrupt, so there is no safe way to continue the walk.
			return problems + 1;
		}

		const Allocation_Header *header = (const Allocation_Header *)block_address;
		block_address += ALLOCATION_HEADER_SIZE + header->size;
	}

	if (block_address != heap_end())
	{
		problems++;
	}

	return problems;
}

// ---------------------------------------------------------------------------
// initialise_heap
// ---------------------------------------------------------------------------
static void test_init_sets_heap_start(void)
{
	reset_heap();
	CU_ASSERT_EQUAL(heap.heap_start_address, heap_start());
	CU_ASSERT_EQUAL(heap.heap_size, TEST_HEAP_SIZE);
}

static void test_init_creates_free_block(void)
{
	reset_heap();

	// The whole heap must be covered by a single free block.
	CU_ASSERT_TRUE(is_block_free(heap_start()));
	CU_ASSERT_FALSE(is_block_allocated(heap_start()));

	const Allocation_Header *header = (const Allocation_Header *)heap_start();
	CU_ASSERT_EQUAL(header->size, TEST_HEAP_SIZE - ALLOCATION_HEADER_SIZE);
}

// ---------------------------------------------------------------------------
// allocate_memory
// ---------------------------------------------------------------------------
static void test_alloc_returns_nonzero(void)
{
	reset_heap();
	uintptr_t ptr = allocate_memory(&heap, 64, 1);
	CU_ASSERT_NOT_EQUAL(ptr, 0);
}

static void test_alloc_zero_size_returns_zero(void)
{
	reset_heap();
	CU_ASSERT_EQUAL(allocate_memory(&heap, 0, 1), 0);
}

static void test_alloc_invalid_alignment_returns_zero(void)
{
	reset_heap();
	CU_ASSERT_EQUAL(allocate_memory(&heap, 64, 0), 0);
	CU_ASSERT_EQUAL(allocate_memory(&heap, 64, 24), 0);
}

static void test_alloc_splits_from_end_of_free_block(void)
{
	reset_heap();
	uintptr_t ptr = allocate_memory(&heap, 64, 1);

	// Blocks are split from the tail of the free block they are allocated
	// within, so the payload ends at the end of the heap.
	CU_ASSERT_EQUAL(ptr + 64, heap_end());
}

static void test_alloc_header_is_allocated_with_size(void)
{
	reset_heap();
	uintptr_t ptr = allocate_memory(&heap, 64, 1);
	CU_ASSERT_NOT_EQUAL(ptr, 0);

	CU_ASSERT_TRUE(is_block_allocated(ptr - ALLOCATION_HEADER_SIZE));
	CU_ASSERT_EQUAL(header_of(ptr)->size, 64);
}

static void test_alloc_preceding_block_remains_free(void)
{
	reset_heap();
	uintptr_t ptr = allocate_memory(&heap, 64, 1);
	CU_ASSERT_NOT_EQUAL(ptr, 0);

	// The remainder of the original block stays free, and its size must reflect
	// the space consumed by the allocation and its header.
	CU_ASSERT_TRUE(is_block_free(heap_start()));

	const Allocation_Header *free_header =
	    (const Allocation_Header *)heap_start();
	CU_ASSERT_EQUAL(heap_start() + ALLOCATION_HEADER_SIZE + free_header->size,
	                ptr - ALLOCATION_HEADER_SIZE);
}

static void test_alloc_sequential_nonoverlap(void)
{
	reset_heap();
	uintptr_t ptr_a = allocate_memory(&heap, 64, 1);
	uintptr_t ptr_b = allocate_memory(&heap, 64, 1);

	CU_ASSERT_NOT_EQUAL(ptr_a, 0);
	CU_ASSERT_NOT_EQUAL(ptr_b, 0);
	CU_ASSERT_NOT_EQUAL(ptr_a, ptr_b);

	// Allocations are made from the tail of the free block, so the second
	// allocation sits below the first, and must not overlap its header.
	CU_ASSERT(ptr_b + 64 <= ptr_a - ALLOCATION_HEADER_SIZE);
}

static void test_alloc_exact_fit(void)
{
	reset_heap();
	// An allocation whose total size (header + payload) equals the heap size
	// consumes the single free block entirely.
	size_t payload = TEST_HEAP_SIZE - ALLOCATION_HEADER_SIZE;
	uintptr_t ptr = allocate_memory(&heap, payload, 1);
	CU_ASSERT_NOT_EQUAL(ptr, 0);
	CU_ASSERT_EQUAL(ptr, heap_start() + ALLOCATION_HEADER_SIZE);

	// The heap is now a single allocated block, so nothing more can be
	// allocated.
	CU_ASSERT_TRUE(is_block_allocated(heap_start()));
	CU_ASSERT_EQUAL(allocate_memory(&heap, 1, 1), 0);
}

static void test_alloc_too_large_returns_zero(void)
{
	reset_heap();
	CU_ASSERT_EQUAL(allocate_memory(&heap, TEST_HEAP_SIZE, 1), 0);
}

static void test_alloc_exhaustion_returns_zero(void)
{
	reset_heap();
	// Drain the heap with small allocations until it is full.
	uintptr_t ptr;
	int count = 0;
	do
	{
		ptr = allocate_memory(&heap, 64, 1);
		count++;
	} while (ptr != 0 && count < (TEST_HEAP_SIZE / 64) + 1);

	// The last attempt must have returned 0 (heap full).
	CU_ASSERT_EQUAL(ptr, 0);
}

static void test_alloc_corrupt_header_returns_zero(void)
{
	reset_heap();

	// Corrupt the first block's header so that it identifies neither a free nor
	// an allocated block. The allocator must refuse to walk the heap.
	Allocation_Header *header = (Allocation_Header *)heap_start();
	header->checksum ^= 0xFFFFFFFF;

	CU_ASSERT_EQUAL(allocate_memory(&heap, 64, 1), 0);
}

static void test_alloc_all_blocks_remain_valid(void)
{
	reset_heap();

	for (int i = 0; i < 8; i++)
	{
		CU_ASSERT_NOT_EQUAL(allocate_memory(&heap, 40, 1), 0);
	}

	// Walk the heap, verifying that every block header is valid, and that the
	// blocks tile the heap exactly.
	uintptr_t block_address = heap_start();
	while (block_address < heap_end())
	{
		CU_ASSERT_TRUE(is_block_free(block_address) ||
		               is_block_allocated(block_address));

		const Allocation_Header *header = (const Allocation_Header *)block_address;
		block_address += ALLOCATION_HEADER_SIZE + header->size;
	}

	CU_ASSERT_EQUAL(block_address, heap_end());
}

// ---------------------------------------------------------------------------
// allocate_memory — alignment
// ---------------------------------------------------------------------------

static void test_alloc_aligned_16_returns_aligned_ptr(void)
{
	reset_heap();
	uintptr_t ptr = allocate_memory(&heap, 64, 16);
	CU_ASSERT_NOT_EQUAL(ptr, 0);
	CU_ASSERT_EQUAL(ptr % 16, 0);
}

static void test_alloc_aligned_64_returns_aligned_ptr(void)
{
	reset_heap();
	uintptr_t ptr = allocate_memory(&heap, 64, 64);
	CU_ASSERT_NOT_EQUAL(ptr, 0);
	CU_ASSERT_EQUAL(ptr % 64, 0);
}

static void test_alloc_aligned_forces_padding(void)
{
	reset_heap();
	// Consume 1 byte so that the end of the remaining free block is no longer
	// naturally aligned. The allocator must align the payload downwards to
	// satisfy the requested alignment.
	allocate_memory(&heap, 1, 1);

	uintptr_t ptr = allocate_memory(&heap, 64, 16);
	CU_ASSERT_NOT_EQUAL(ptr, 0);
	CU_ASSERT_EQUAL(ptr % 16, 0);
}

static void test_alloc_aligned_multiple_all_aligned(void)
{
	reset_heap();
	for (int i = 0; i < 8; i++)
	{
		uintptr_t ptr = allocate_memory(&heap, 64, 16);
		CU_ASSERT_NOT_EQUAL(ptr, 0);
		CU_ASSERT_EQUAL(ptr % 16, 0);
	}
}

static void test_alloc_aligned_header_is_allocated_with_size(void)
{
	reset_heap();
	uintptr_t ptr = allocate_memory(&heap, 64, 16);
	CU_ASSERT_NOT_EQUAL(ptr, 0);

	CU_ASSERT_TRUE(is_block_allocated(ptr - ALLOCATION_HEADER_SIZE));
	// The allocation absorbs any alignment padding too small to hold another
	// block header, so the recorded size may exceed the requested size.
	CU_ASSERT(header_of(ptr)->size >= 64);
}

static void test_alloc_unaligned_request_is_max_aligned(void)
{
	// malloc() requests an alignment of 1. The allocator must still return
	// memory that is aligned for an object of any type.
	// The assertion uses max_align_t, and not ALLOCATION_MINIMUM_ALIGNMENT,
	// because a change to that constant must not weaken this test.
	for (size_t size = 1; size <= 64; size++)
	{
		reset_heap();

		uintptr_t ptr = allocate_memory(&heap, size, 1);
		CU_ASSERT_NOT_EQUAL(ptr, 0);
		CU_ASSERT_EQUAL(ptr % _Alignof(max_align_t), 0);
	}
}

static void test_alloc_all_headers_are_aligned(void)
{
	reset_heap();

	// A block header is written at the end of an allocation, as well as before
	// it. A size that is not a multiple of the minimum alignment must not push
	// a subsequent header out of alignment.
	for (size_t size = 1; size <= 40; size += 3)
	{
		CU_ASSERT_NOT_EQUAL(allocate_memory(&heap, size, 1), 0);
	}

	CU_ASSERT_EQUAL(audit_heap_blocks(), 0);
}

// ---------------------------------------------------------------------------
// free_memory
// ---------------------------------------------------------------------------
static void test_free_and_reallocate(void)
{
	reset_heap();
	uintptr_t ptr_a = allocate_memory(&heap, 64, 1);
	CU_ASSERT_NOT_EQUAL(ptr_a, 0);

	free_memory(&heap, ptr_a);

	// Freeing the only allocation restores the heap to a single free block, so
	// the next allocation of the same size returns the same address.
	uintptr_t ptr_b = allocate_memory(&heap, 64, 1);
	CU_ASSERT_EQUAL(ptr_b, ptr_a);
}

static void test_free_marks_block_free(void)
{
	reset_heap();
	uintptr_t ptr = allocate_memory(&heap, 64, 1);
	CU_ASSERT_NOT_EQUAL(ptr, 0);
	CU_ASSERT_TRUE(is_block_allocated(ptr - ALLOCATION_HEADER_SIZE));

	free_memory(&heap, ptr);

	// The block is merged into the preceding free block, so the heap is a
	// single free block once more.
	CU_ASSERT_TRUE(is_block_free(heap_start()));
	CU_ASSERT_FALSE(is_block_allocated(heap_start()));

	const Allocation_Header *header = (const Allocation_Header *)heap_start();
	CU_ASSERT_EQUAL(header->size, TEST_HEAP_SIZE - ALLOCATION_HEADER_SIZE);
}

static void test_free_invalid_header_is_noop(void)
{
	reset_heap();
	uintptr_t ptr = allocate_memory(&heap, 64, 1);
	CU_ASSERT_NOT_EQUAL(ptr, 0);

	// Corrupt the header's checksum so free_memory treats it as invalid.
	Allocation_Header *header =
	    (Allocation_Header *)(ptr - ALLOCATION_HEADER_SIZE);
	header->checksum = 0xDEADBEEF;

	free_memory(&heap, ptr);

	// The block must not have been marked free.
	CU_ASSERT_FALSE(is_block_free(ptr - ALLOCATION_HEADER_SIZE));
}

static void test_free_outside_heap_is_noop(void)
{
	reset_heap();

	free_memory(&heap, 0);
	free_memory(&heap, heap_start());
	free_memory(&heap, heap_end() + 64);

	// The heap must still consist of a single free block spanning the heap.
	const Allocation_Header *header = (const Allocation_Header *)heap_start();
	CU_ASSERT_TRUE(is_block_free(heap_start()));
	CU_ASSERT_EQUAL(header->size, TEST_HEAP_SIZE - ALLOCATION_HEADER_SIZE);
}

static void test_free_double_free_is_noop(void)
{
	reset_heap();
	uintptr_t ptr = allocate_memory(&heap, 64, 1);
	CU_ASSERT_NOT_EQUAL(ptr, 0);

	free_memory(&heap, ptr);
	free_memory(&heap, ptr);

	// The heap must be intact, and fully allocatable.
	uintptr_t large =
	    allocate_memory(&heap, TEST_HEAP_SIZE - ALLOCATION_HEADER_SIZE, 1);
	CU_ASSERT_NOT_EQUAL(large, 0);
}

static void test_free_coalesces_adjacent_blocks(void)
{
	reset_heap();

	size_t alloc_size = 64;
	uintptr_t ptr_a = allocate_memory(&heap, alloc_size, 1);
	uintptr_t ptr_b = allocate_memory(&heap, alloc_size, 1);
	CU_ASSERT_NOT_EQUAL(ptr_a, 0);
	CU_ASSERT_NOT_EQUAL(ptr_b, 0);

	free_memory(&heap, ptr_a);
	free_memory(&heap, ptr_b);

	// After coalescing, the entire heap should be available again.
	size_t large = TEST_HEAP_SIZE - ALLOCATION_HEADER_SIZE;
	uintptr_t ptr_large = allocate_memory(&heap, large, 1);
	CU_ASSERT_NOT_EQUAL(ptr_large, 0);
}

static void test_free_middle_block_coalesces_both_sides(void)
{
	reset_heap();

	uintptr_t ptr_a = allocate_memory(&heap, 64, 1);
	uintptr_t ptr_b = allocate_memory(&heap, 64, 1);
	uintptr_t ptr_c = allocate_memory(&heap, 64, 1);
	CU_ASSERT_NOT_EQUAL(ptr_a, 0);
	CU_ASSERT_NOT_EQUAL(ptr_b, 0);
	CU_ASSERT_NOT_EQUAL(ptr_c, 0);

	// Free the outer two allocations, then the one between them. The three
	// freed blocks, plus the unallocated remainder of the heap, must coalesce
	// into a single free block.
	free_memory(&heap, ptr_a);
	free_memory(&heap, ptr_c);
	free_memory(&heap, ptr_b);

	const Allocation_Header *header = (const Allocation_Header *)heap_start();
	CU_ASSERT_TRUE(is_block_free(heap_start()));
	CU_ASSERT_EQUAL(header->size, TEST_HEAP_SIZE - ALLOCATION_HEADER_SIZE);
}

// ---------------------------------------------------------------------------
// churn
// ---------------------------------------------------------------------------

// The generator is deterministic, so that a failure can be reproduced.
static uint32_t random_state;

static uint32_t next_random(void)
{
	random_state = (random_state * 1103515245u) + 12345u;

	return random_state >> 16;
}

static void test_churn_preserves_heap_invariants(void)
{
	int audit_failures = 0;
	int alignment_failures = 0;
	int leak_failures = 0;

	random_state = 20260801u;

	for (int round = 0; round < 200; round++)
	{
		reset_heap();

		uintptr_t live[32];
		int live_count = 0;

		for (int op = 0; op < 40; op++)
		{
			if (live_count < 32 && (next_random() % 3) != 0)
			{
				const size_t size = 1 + (next_random() % 200);
				const size_t alignment = (size_t)1 << (next_random() % 7);

				uintptr_t ptr = allocate_memory(&heap, size, alignment);
				if (ptr != 0)
				{
					if ((ptr % alignment) != 0 || (ptr % _Alignof(max_align_t)) != 0)
					{
						alignment_failures++;
					}

					live[live_count++] = ptr;
				}
			}
			else if (live_count > 0)
			{
				const int index = (int)(next_random() % (uint32_t)live_count);

				free_memory(&heap, live[index]);
				live[index] = live[--live_count];
			}

			audit_failures += audit_heap_blocks();
		}

		// When every remaining allocation is freed, the heap must become a
		// single free block that spans the whole heap again.
		for (int i = 0; i < live_count; i++)
		{
			free_memory(&heap, live[i]);
		}

		const Allocation_Header *header = (const Allocation_Header *)heap_start();
		if (!is_block_free(heap_start()) ||
		    header->size != TEST_HEAP_SIZE - ALLOCATION_HEADER_SIZE)
		{
			leak_failures++;
		}
	}

	// The totals are asserted once, and not for each operation, to keep the
	// test summary readable.
	CU_ASSERT_EQUAL(audit_failures, 0);
	CU_ASSERT_EQUAL(alignment_failures, 0);
	CU_ASSERT_EQUAL(leak_failures, 0);
}

// ---------------------------------------------------------------------------
// main
// ---------------------------------------------------------------------------
int main(void)
{
	if (CU_initialize_registry() != CUE_SUCCESS)
	{
		return CU_get_error();
	}

	CU_pSuite s_init = CU_add_suite("initialise_heap", NULL, NULL);
	CU_add_test(s_init, "sets heap start address and size",
	            test_init_sets_heap_start);
	CU_add_test(s_init, "creates single free block covering heap",
	            test_init_creates_free_block);

	CU_pSuite s_alloc = CU_add_suite("allocate_memory", NULL, NULL);
	CU_add_test(s_alloc, "returns non-zero on success",
	            test_alloc_returns_nonzero);
	CU_add_test(s_alloc, "zero size returns zero",
	            test_alloc_zero_size_returns_zero);
	CU_add_test(s_alloc, "invalid alignment returns zero",
	            test_alloc_invalid_alignment_returns_zero);
	CU_add_test(s_alloc, "allocation is split from the end of the free block",
	            test_alloc_splits_from_end_of_free_block);
	CU_add_test(s_alloc, "header marks block allocated with correct size",
	            test_alloc_header_is_allocated_with_size);
	CU_add_test(s_alloc, "remainder of the split block stays free",
	            test_alloc_preceding_block_remains_free);
	CU_add_test(s_alloc, "sequential allocations do not overlap",
	            test_alloc_sequential_nonoverlap);
	CU_add_test(s_alloc, "exact-fit allocation consumes the heap",
	            test_alloc_exact_fit);
	CU_add_test(s_alloc, "oversized allocation returns zero",
	            test_alloc_too_large_returns_zero);
	CU_add_test(s_alloc, "returns zero when heap is exhausted",
	            test_alloc_exhaustion_returns_zero);
	CU_add_test(s_alloc, "corrupt block header returns zero",
	            test_alloc_corrupt_header_returns_zero);
	CU_add_test(s_alloc, "all blocks remain valid and tile the heap",
	            test_alloc_all_blocks_remain_valid);

	CU_pSuite s_align = CU_add_suite("allocate_memory — alignment", NULL, NULL);
	CU_add_test(s_align, "16-byte alignment satisfied",
	            test_alloc_aligned_16_returns_aligned_ptr);
	CU_add_test(s_align, "64-byte alignment satisfied",
	            test_alloc_aligned_64_returns_aligned_ptr);
	CU_add_test(s_align, "alignment satisfied after padding forced",
	            test_alloc_aligned_forces_padding);
	CU_add_test(s_align, "multiple aligned allocations all satisfy alignment",
	            test_alloc_aligned_multiple_all_aligned);
	CU_add_test(s_align, "aligned allocation header marks block allocated",
	            test_alloc_aligned_header_is_allocated_with_size);
	CU_add_test(s_align, "unaligned request is still aligned for any type",
	            test_alloc_unaligned_request_is_max_aligned);
	CU_add_test(s_align, "every block header stays aligned",
	            test_alloc_all_headers_are_aligned);

	CU_pSuite s_free = CU_add_suite("free_memory", NULL, NULL);
	CU_add_test(s_free, "freed memory can be reallocated at same address",
	            test_free_and_reallocate);
	CU_add_test(s_free, "marks the block free", test_free_marks_block_free);
	CU_add_test(s_free, "invalid header is a no-op",
	            test_free_invalid_header_is_noop);
	CU_add_test(s_free, "address outside the heap is a no-op",
	            test_free_outside_heap_is_noop);
	CU_add_test(s_free, "double free is a no-op", test_free_double_free_is_noop);
	CU_add_test(s_free, "adjacent freed blocks are coalesced",
	            test_free_coalesces_adjacent_blocks);
	CU_add_test(s_free, "freeing a middle block coalesces both neighbours",
	            test_free_middle_block_coalesces_both_sides);

	CU_pSuite s_churn = CU_add_suite("churn", NULL, NULL);
	CU_add_test(s_churn, "randomised allocation and freeing preserves invariants",
	            test_churn_preserves_heap_invariants);

	CU_basic_set_mode(CU_BRM_VERBOSE);
	CU_basic_run_tests();

	int failures = (int)CU_get_number_of_failures();
	CU_cleanup_registry();
	return failures != 0;
}
