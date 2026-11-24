#ifndef SIMPLE_SHELL_H
#define SIMPLE_SHELL_H 1

// Changes to the directory in the first argument, or to '/' without one.
void change_directory(int argument_count, char **arguments);

// Lists the directory in the first argument, or the working directory
// without one. Each entry is on its own line, and a directory's name is
// followed by a '/'.
void list_directory(int argument_count, char **arguments);

void print_working_directory(int argument_count);

#endif
