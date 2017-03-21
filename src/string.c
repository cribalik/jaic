#include "array.c"
#include <assert.h>
#include <string.h>


typedef struct String {
  DynArray chars;
} String;

internal int string_append(String* a, const char* b) {
  int len;
  char* next;
  assert(a->chars.count > 0);
  len = strlen(b);
  next = array_pushN(&a->chars, len);
  --next;
  memcpy(next, b, len);
  next[len] = 0;
  return len;
}

internal String string_create(char* initial_value) {
  String result;
  result.chars = array_create(1, 1);
  *(char*) array_push(&result.chars) = 0;
  if (initial_value) {
    string_append(&result, initial_value);
  }
  return result;
}

internal int string_prepend(String* a, char* b) {
  int len;
  char* next;
  assert(a->chars.count > 0);
  len = strlen(b);
  /* move current string to end */
  next = array_pushN(&a->chars, len);
  --next;
  memmove(next, a->chars.data, a->chars.count - 1);
  *(char*)arrayLast(&a->chars) = 0;

  /* prepend new string */
  next = a->chars.data;
  while (*b) {
    *next++ = *b++;
  }
  return len;
}

internal void string_pop(String* s, int n) {
  assert(s->chars.count > n);
  arrayPopN(&s->chars, n);
  *(char*)arrayLast(&s->chars) = 0;
}

internal char* string_get(String* s) {
  return arrayBegin(&s->chars);
}

internal char* string_clear(String* s) {
  s->chars.count = 1;
  *(char*)arrayLast(&s->chars) = 0;
  return s->chars.data;
}

internal void string_free(String* s) {
  arrayFree(&s->chars);
}

internal void string_append_char(String* s, char c) {
  char* p = array_push(&s->chars);
  p[-1] = c;
  *p = 0;
}