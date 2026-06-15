#include <CUnit/Basic.h>
#include <CUnit/CUnit.h>
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

// ---------------------------------------------------------------------------
// initialise_heap
// ---------------------------------------------------------------------------
static void test_init_sets_heap_start(void)
{
	reset_heap();
	CU_ASSERT_EQUAL(heap.heap_start_address, (uintptr_t)heap_backing);
}

static void test_init_creates_free_region(void)
{
	reset_heap();
	CU_ASSERT_NOT_EQUAL(heap.free_region_list_head_index, NULL_REGION_INDEX);

	uint16_t head = heap.free_region_list_head_index;
	CU_ASSERT_EQUAL(heap.free_region_list[head].address, (uintptr_t)heap_backing);
	CU_ASSERT_EQUAL(heap.free_region_list[head].size, TEST_HEAP_SIZE);
	CU_ASSERT_TRUE(heap.free_region_list[head].entry_used);
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

static void test_alloc_pointer_past_header(void)
{
	reset_heap();
	uintptr_t ptr = allocate_memory(&heap, 64, 1);
	CU_ASSERT_EQUAL(ptr, (uintptr_t)heap_backing + sizeof(Allocation_Header));
}

static void test_alloc_header_identity_and_size(void)
{
	reset_heap();
	uintptr_t ptr = allocate_memory(&heap, 64, 1);
	CU_ASSERT_NOT_EQUAL(ptr, 0);

	Allocation_Header *hdr =
	    (Allocation_Header *)(ptr - sizeof(Allocation_Header));
	CU_ASSERT_EQUAL(hdr->identity, ALLOCATION_HEADER_IDENTITY);
	CU_ASSERT_EQUAL(hdr->size, 64);
}

static void test_alloc_sequential_nonoverlap(void)
{
	reset_heap();
	uintptr_t ptr_a = allocate_memory(&heap, 64, 1);
	uintptr_t ptr_b = allocate_memory(&heap, 64, 1);

	CU_ASSERT_NOT_EQUAL(ptr_a, 0);
	CU_ASSERT_NOT_EQUAL(ptr_b, 0);
	CU_ASSERT_NOT_EQUAL(ptr_a, ptr_b);

	// ptr_b must start at or after ptr_a + 64
	CU_ASSERT(ptr_b >= ptr_a + 64);
}

static void test_alloc_exact_fit(void)
{
	reset_heap();
	// An allocation whose total size (header + payload) equals the heap size
	// should consume the single free region entirely.
	size_t payload = TEST_HEAP_SIZE - sizeof(Allocation_Header);
	uintptr_t ptr = allocate_memory(&heap, payload, 1);
	CU_ASSERT_NOT_EQUAL(ptr, 0);

	// The free list should now be empty.
	CU_ASSERT_EQUAL(heap.free_region_list_head_index, NULL_REGION_INDEX);
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
	} while (ptr != 0 && count < MAX_FREE_REGIONS + 1);

	// The last attempt must have returned 0 (heap full).
	CU_ASSERT_EQUAL(ptr, 0);
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
	// Consume 1 byte so the free region start shifts by sizeof(Header)+1 bytes,
	// making it no longer naturally 16-byte aligned after the header. The
	// allocator must pad to satisfy the requested alignment.
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

static void test_alloc_aligned_header_identity_and_size(void)
{
	reset_heap();
	uintptr_t ptr = allocate_memory(&heap, 64, 16);
	CU_ASSERT_NOT_EQUAL(ptr, 0);

	Allocation_Header *hdr =
	    (Allocation_Header *)(ptr - sizeof(Allocation_Header));
	CU_ASSERT_EQUAL(hdr->identity, ALLOCATION_HEADER_IDENTITY);
	CU_ASSERT_EQUAL(hdr->size, 64);
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

	// The freed region is re-inserted at the front of the free list, so the
	// next allocation of the same size should return the same address.
	uintptr_t ptr_b = allocate_memory(&heap, 64, 1);
	CU_ASSERT_EQUAL(ptr_b, ptr_a);
}

static void test_free_clears_header_identity(void)
{
	reset_heap();
	uintptr_t ptr = allocate_memory(&heap, 64, 1);
	CU_ASSERT_NOT_EQUAL(ptr, 0);

	Allocation_Header *hdr =
	    (Allocation_Header *)(ptr - sizeof(Allocation_Header));
	CU_ASSERT_EQUAL(hdr->identity, ALLOCATION_HEADER_IDENTITY);

	free_memory(&heap, ptr);
	CU_ASSERT_NOT_EQUAL(hdr->identity, ALLOCATION_HEADER_IDENTITY);
}

static void test_free_invalid_identity_is_noop(void)
{
	reset_heap();
	uintptr_t ptr = allocate_memory(&heap, 64, 1);
	CU_ASSERT_NOT_EQUAL(ptr, 0);

	// Corrupt the header identity so free_memory should treat it as invalid.
	Allocation_Header *hdr =
	    (Allocation_Header *)(ptr - sizeof(Allocation_Header));
	hdr->identity = 0xDEADBEEF;

	uint16_t head_before = heap.free_region_list_head_index;
	free_memory(&heap, ptr);
	// The free list head must be unchanged — the corrupt free was a no-op.
	CU_ASSERT_EQUAL(heap.free_region_list_head_index, head_before);
}

static void test_free_coalesces_adjacent_regions(void)
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
	// Allocate nearly the full heap to verify.
	size_t large = TEST_HEAP_SIZE - sizeof(Allocation_Header);
	uintptr_t ptr_large = allocate_memory(&heap, large, 1);
	CU_ASSERT_NOT_EQUAL(ptr_large, 0);
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
	CU_add_test(s_init, "sets heap_start_address", test_init_sets_heap_start);
	CU_add_test(s_init, "creates single free region covering heap",
	            test_init_creates_free_region);

	CU_pSuite s_alloc = CU_add_suite("allocate_memory", NULL, NULL);
	CU_add_test(s_alloc, "returns non-zero on success",
	            test_alloc_returns_nonzero);
	CU_add_test(s_alloc, "pointer is past allocation header",
	            test_alloc_pointer_past_header);
	CU_add_test(s_alloc, "header has correct identity and size",
	            test_alloc_header_identity_and_size);
	CU_add_test(s_alloc, "sequential allocations do not overlap",
	            test_alloc_sequential_nonoverlap);
	CU_add_test(s_alloc, "exact-fit allocation empties free list",
	            test_alloc_exact_fit);
	CU_add_test(s_alloc, "returns zero when heap is exhausted",
	            test_alloc_exhaustion_returns_zero);

	CU_pSuite s_align = CU_add_suite("allocate_memory — alignment", NULL, NULL);
	CU_add_test(s_align, "16-byte alignment satisfied",
	            test_alloc_aligned_16_returns_aligned_ptr);
	CU_add_test(s_align, "64-byte alignment satisfied",
	            test_alloc_aligned_64_returns_aligned_ptr);
	CU_add_test(s_align, "alignment satisfied after header padding forced",
	            test_alloc_aligned_forces_padding);
	CU_add_test(s_align, "multiple aligned allocations all satisfy alignment",
	            test_alloc_aligned_multiple_all_aligned);
	CU_add_test(s_align, "aligned allocation header has correct identity and size",
	            test_alloc_aligned_header_identity_and_size);

	CU_pSuite s_free = CU_add_suite("free_memory", NULL, NULL);
	CU_add_test(s_free, "freed memory can be reallocated at same address",
	            test_free_and_reallocate);
	CU_add_test(s_free, "clears allocation header identity on free",
	            test_free_clears_header_identity);
	CU_add_test(s_free, "invalid identity is a no-op",
	            test_free_invalid_identity_is_noop);
	CU_add_test(s_free, "adjacent freed regions are coalesced",
	            test_free_coalesces_adjacent_regions);

	CU_basic_set_mode(CU_BRM_VERBOSE);
	CU_basic_run_tests();

	int failures = (int)CU_get_number_of_failures();
	CU_cleanup_registry();
	return failures != 0;
}
