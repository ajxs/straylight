#include <stdint.h>

#ifndef STRAYLIGHT_LIBC_H
#define STRAYLIGHT_LIBC_H 1

void exit(int status);
void log_debug(char *test_string);
void log_error(char *test_string);
void print_to_serial(char *test_string);

void straylight_graphics_fill_framebuffer(
		uintptr_t framebuffer_address,
		uint32_t width,
		uint32_t height,
		uint32_t color);

uint64_t straylight_graphics_update_framebuffer(uintptr_t framebuffer_address);

uint32_t straylight_graphics_make_colour(
		uint8_t red,
		uint8_t green,
		uint8_t blue,
		uint8_t alpha);

uint64_t allocate_memory(uint64_t size, uintptr_t *addr);

#endif
