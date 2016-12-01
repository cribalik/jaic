#ifndef JAIC_UTILS
#define JAIC_UTILS

#include <stdio.h>

#define ZERO(ptr) memset(ptr, 0, sizeof(*ptr))

// Logging
#define logError(msg, ...) fprintf(stderr, BOLD "%s:%i:%i (%s:%i): " RED "error: " RESET_COLOR RESET_FORMAT msg, filename, line+1, column, __FILE__, __LINE__, ##__VA_ARGS__); printLine(stderr, filename, line, column);

#ifdef DEBUG
#define logDebugInfo(msg, ...) fprintf(stderr, BOLD "%s:%i: " GREEN "debug: " RESET_COLOR RESET_FORMAT msg, __FILE__, __LINE__, ##__VA_ARGS__)
#define logDebugError(msg, ...) fprintf(stderr, BOLD "%s:%i: " RED "error: " RESET_COLOR RESET_FORMAT msg, __FILE__, __LINE__, ##__VA_ARGS__)
#else
#define logDebugInfo(msg, ...)
#define logDebugError(msg, ...)
#endif

void printLine(FILE* out, char* filename, int line, int column) {
  // TODO: send in the file position instead so we don't need to parse the whole file? it's okay if this is slow though
  if (!filename) {
    return;
  }

  FILE* file = fopen(filename, "r");
  int l = 0;
  while (l < line) {
    l += getc(file) == '\n';
  }

  fflush(out);
  char c = getc(file);
  while (c != EOF && c != '\n') {
    putc(c, out);
    c = getc(file);
  }
  putc('\n', out);
  for (int i = 0; i < column-1; ++i) {
    putc(' ', out);
  }
  putc('^', out);
  putc('\n', out);
  fflush(out);
};

// Some macros
#define UNIMPLEMENTED logError("Unimplemented function at %s:%d\n", __FILE__, __LINE__);
#define UNREACHABLE logError("Unreachable\n"); exit(1);

// typedefs
typedef char byte;
typedef char bool;
typedef int bool32;
#define true 1
#define false 0

#endif /* JAIC_UTILS */
