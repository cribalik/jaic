#ifndef JAIC_COMMON
#define JAIC_COMMON

#include <stdio.h>
#include <stddef.h>
#include <stdarg.h>
#include "terminal.c"
#include "array.c"
#include "memarena.c"

#define global static
#define local_persist static
#define internal static

/* typedefs */
typedef char byte;
typedef char bool;
typedef int bool32;
#define true 1
#define false 0

typedef struct {
  char* file;
  int line;
  int column;
  fpos_t fpos;
} FilePos;

#define ARRSIZE(arr) (int)sizeof(arr)/(int)sizeof(*arr)

#define ZERO(ptr) memset(ptr, 0, sizeof(*ptr))


#ifndef DEBUG
  #define DEBUG 0
#endif




#endif /* JAIC_COMMON */
