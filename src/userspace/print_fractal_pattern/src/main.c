#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <straylight/logging.h>
#include <straylight/graphics.h>

int main()
{
	print_to_serial("Printing fractal patterns!\n");

	const uint16_t horizontal_resolution = 640;
	const uint16_t vertical_resolution = 480;

	uintptr_t framebuffer_address = (uintptr_t)malloc(
			horizontal_resolution * vertical_resolution * 4);
	if (framebuffer_address == 0)
	{
		log_error("Failed to allocate framebuffer memory");
		return 1;
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
		for (y = 0; y < vertical_resolution; y++)
		{
			for (x = 0; x < horizontal_resolution; x++)
			{
				c = (x ^ y) % 256;
				colour = straylight_graphics_make_colour(255 - (c % q), c, c % q, 255);

				pixel = x + (y * horizontal_resolution);
				((uint32_t *)framebuffer_address)[pixel] = colour;
			}
		}

		uint64_t result = straylight_graphics_update_framebuffer(framebuffer_address);
		if (result != 0)
		{
			log_error("Failed to update framebuffer");
			exit(EXIT_FAILURE);
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

	exit(EXIT_SUCCESS);
}
