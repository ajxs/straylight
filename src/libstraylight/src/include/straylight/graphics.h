#ifndef STRAYLIGHT_GRAPHICS_H
#define STRAYLIGHT_GRAPHICS_H 1

#include <stdint.h>

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

#endif
