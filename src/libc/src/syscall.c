#include <straylight_syscall.h>

bool is_syscall_result_error(int64_t result)
{
	return result < 0;
};
