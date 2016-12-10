#ifndef JAIC_UTILS
#define JAIC_UTILS

#include <stdio.h>
#include "terminal.c"


// typedefs
typedef char byte;
typedef char bool;
typedef int bool32;
#define true 1
#define false 0

#define ARRSIZE(arr) (int)sizeof(arr)/(int)sizeof(*arr)

#define ZERO(ptr) memset(ptr, 0, sizeof(*ptr))

typedef struct {
  void* data;
  int size;
} Array;

// Logging
#define logErrorAt(filepos, msg, ...) fprintf(stderr, "%s%s:%i:%i (%s:%i): %serror:%s%s " msg, BOLD, filepos.file, filepos.line, filepos.column, __FILE__, __LINE__, RED, RESET_COLOR, RESET_FORMAT, ##__VA_ARGS__); printLine(stderr, filepos.file, filepos.line, filepos.column); found_error = true;
#define logError(msg, ...) fprintf(stderr, "%s(%s:%i): %serror:%s%s " msg, BOLD, __FILE__, __LINE__, RED, RESET_COLOR, RESET_FORMAT, ##__VA_ARGS__); found_error = true;
#define logNote(msg, ...) fprintf(stderr, "%s (%s:%i): %snote:%s%s " msg, BOLD, __FILE__, __LINE__, BLUE, RESET_COLOR, RESET_FORMAT, ##__VA_ARGS__);
#define logNoteAt(filepos, msg, ...) fprintf(stderr, "%s%s:%i:%i (%s:%i): %snote:%s%s " msg, BOLD, filepos.file, filepos.line, filepos.column, __FILE__, __LINE__, BLUE, RESET_COLOR, RESET_FORMAT, ##__VA_ARGS__); printLine(stderr, filepos.file, filepos.line, filepos.column);

#ifdef DEBUG
#define logDebugInfo(msg, ...) fprintf(stderr, "%s%s:%i: %sdebug:%s%s " msg, BOLD, __FILE__, __LINE__, GREEN, RESET_COLOR, RESET_FORMAT, ##__VA_ARGS__)
#define logDebugInfoAt(filepos, msg, ...) fprintf(stderr, "%s%s:%i: %sdebug:%s%s " msg, BOLD, __FILE__, __LINE__, GREEN, RESET_COLOR, RESET_FORMAT, ##__VA_ARGS__); printLine(stderr, filepos.file, filepos.line, filepos.column);
#define logDebugError(msg, ...) fprintf(stderr, "%s%s:%i: %serror:%s%s " msg, BOLD, __FILE__, __LINE__, RED, RESET_COLOR, RESET_FORMAT, ##__VA_ARGS__)
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
  int l = 1;
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
  for (int i = 0; i < column-2; ++i) {
    putc(' ', out);
  }
  putc('^', out);
  putc('\n', out);
  fflush(out);
};

// Some macros
#define UNIMPLEMENTED logDebugError("Unimplemented function at %s:%d\n", __FILE__, __LINE__); exit(1);
#define UNREACHABLE logDebugError("Unreachable\n"); exit(1);

#endif /* JAIC_UTILS */
