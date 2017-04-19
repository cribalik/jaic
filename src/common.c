#ifndef JAIC_COMMON
#define JAIC_COMMON

#include <stdio.h>
#include <stddef.h>
#include <stdarg.h>
#include "terminal.c"
#include "array.c"
#include "memarena.c"

/* typedefs */
typedef char byte;

typedef struct {
  char* file;
  int line;
  int column;
  fpos_t fpos;
} FilePos;

#define ARRSIZE(arr) (int)sizeof(arr)/(int)sizeof(*arr)

#endif /* JAIC_COMMON */
