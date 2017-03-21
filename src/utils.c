#ifndef JAIC_UTILS
#define JAIC_UTILS

#include "array.c"
#include "memarena.c"

static void* arena_push_array(DynArray* arr, MemArena* arena) {
  int size;
  void* source;
  void* dest;
  arrayGetData(arr, &source, &size);
  dest = arena_push(arena, size);
  memcpy(dest, source, size);
  return dest;
}

#endif /* JAIC_UTILS */
