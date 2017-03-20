#include "array.c"
#include <assert.h>
#include <string.h>


typedef struct String {
  DynArray chars;
} String;

internal int stringAppend(String* a, char* b) {
  int len;
  char* next;
  assert(a->chars.count > 0);
  len = strlen(b);
  next = arrayPushN(&a->chars, len);
  --next;
  memcpy(next, b, len);
  next[len] = 0;
  return len;
}

internal void stringInit(String* s, char* c) {
  arrayInit(&s->chars, 1, 1);
  *(char*)arrayPush(&s->chars) = 0;
  if (c) {
    stringAppend(s, c);
  }
}

internal int stringPrepend(String* a, char* b) {
  int len;
  char* next;
  assert(a->chars.count > 0);
  len = strlen(b);
  /* move current string to end */
  next = arrayPushN(&a->chars, len);
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

internal void stringPop(String* s, int n) {
  assert(s->chars.count > n);
  arrayPopN(&s->chars, n);
  *(char*)arrayLast(&s->chars) = 0;
}

internal char* stringGet(String* s) {
  return arrayBegin(&s->chars);
}

internal char* stringClear(String* s) {
  s->chars.count = 1;
  *(char*)arrayLast(&s->chars) = 0;
  return s->chars.data;
}

internal void stringFree(String* s) {
  arrayFree(&s->chars);
}

internal void stringAppendChar(String* s, char c) {
  char* p = arrayPush(&s->chars);
  p[-1] = c;
  *p = 0;
}