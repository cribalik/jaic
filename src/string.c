#include "array.c"
#include <assert.h>
#include <string.h>


typedef struct String {
  DynArray chars;
} String;

static int stringAppend(String* a, char* b);
static void stringInit(String* s, char* c) {
  arrayInit(&s->chars, 1, 1);
  *(char*)arrayPush(&s->chars) = 0;
  if (c) {
    stringAppend(s, c);
  }
}

static int stringAppend(String* a, char* b) {
  assert(a->chars.count > 0);
  int len = strlen(b);
  char* next = arrayPushN(&a->chars, len);
  --next;
  memcpy(next, b, len);
  next[len] = 0;
  return len;
}

static int stringPrepend(String* a, char* b) {
  assert(a->chars.count > 0);
  int len = strlen(b);
  /* move current string to end */
  char* next = arrayPushN(&a->chars, len);
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

static void stringPop(String* s, int n) {
  assert(s->chars.count > n);
  arrayPopN(&s->chars, n);
  *(char*)arrayLast(&s->chars) = 0;
}

static char* stringGet(String* s) {
  return arrayBegin(&s->chars);
}

static char* stringClear(String* s) {
  s->chars.count = 1;
  *(char*)arrayLast(&s->chars) = 0;
  return s->chars.data;
}

static void stringFree(String* s) {
  arrayFree(&s->chars);
}

static void stringAppendChar(String* s, char c) {
  char* p = arrayPush(&s->chars);
  p[-1] = c;
  *p = 0;
}