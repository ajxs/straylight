#ifndef STRAYLIGHT_STRING_H
#define STRAYLIGHT_STRING_H 1

#include <stddef.h>
#include <stdint.h>

void memcpy(
	void* dest,
	void* src,
	size_t size
);

int strlen(char* str);

#endif 
