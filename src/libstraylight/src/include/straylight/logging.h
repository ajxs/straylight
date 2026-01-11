#include <stdint.h>

#ifndef STRAYLIGHT_LOGGING_H
#define STRAYLIGHT_LOGGING_H 1

void log_debug(char *test_string);
void log_error(char *test_string);
void print_to_serial(char *test_string);

#endif
