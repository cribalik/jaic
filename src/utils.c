#ifndef JAIC_UTILS
#define JAIC_UTILS

#include "array.c"
#include "memarena.c"

static void* pushArrayToArena(DynArray* arr, MemArena* arena) {
  int size;
  void* source;
  void* dest;
  arrayGetData(arr, &source, &size);
  dest = arenaPush(arena, size);
  memcpy(dest, source, size);
  return dest;
}

#endif /* JAIC_UTILS */
