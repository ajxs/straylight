#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <straylight/graphics.h>

int main()
{
	printf("Printing fractal patterns!\n");

	const uint16_t horizontal_resolution = 640;
	const uint16_t vertical_resolution = 480;

	const uint16_t window_width = horizontal_resolution / 2;
	const uint16_t window_height = vertical_resolution / 2;
	const uint16_t window_x = 0;
	const uint16_t window_y = 0;

	uintptr_t framebuffer_address =
	    (uintptr_t)malloc(horizontal_resolution * vertical_resolution * 4);
	if (framebuffer_address == 0)
	{
		fprintf(stderr, "Failed to allocate framebuffer memory\n");
		return EXIT_FAILURE;
	}

	uint8_t c = 0;
	uint32_t colour = 0;
	uint16_t x = 0;
	uint16_t y = 0;
	uint32_t pixel = 0;
	uint8_t q = 0;
	uint8_t direction = 0;

	while (1)
	{
		for (y = 0; y < window_height; y++)
		{
			for (x = 0; x < window_width; x++)
			{
				const uint16_t px = x + window_x;
				const uint16_t py = y + window_y;

				c = (px ^ py) % 256;
				colour = straylight_graphics_make_colour(255 - (c % q), c, c % q, 255);

				pixel = px + (py * horizontal_resolution);
				((uint32_t *)framebuffer_address)[pixel] = colour;
			}
		}

		uint64_t result = straylight_graphics_update_framebuffer(
		    framebuffer_address, window_x, window_y, window_width, window_height);
		if (result != 0)
		{
			fprintf(stderr, "Failed to update framebuffer\n");
			return EXIT_FAILURE;
		}

		if (direction == 0)
		{
			q++;
			if (q == 128)
			{
				direction = 1;
			}
		}
		else
		{
			q--;
			if (q == 1)
			{
				direction = 0;
			}
		}
	}

	return EXIT_SUCCESS;
}
