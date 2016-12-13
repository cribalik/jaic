#include <stdlib.h>
#include <string.h>
#include <assert.h>

typedef char byte;

typedef struct Array {
  int count;
  void* data;
} Array;

typedef struct {
  int count, capacity, item_size;
  void* data;
} DynArray;

static void arrayInit(DynArray* arr, int capacity, int item_size) {
  arr->data = 0;
  arr->count = 0;
  arr->capacity = capacity;
  arr->item_size = item_size;
  if (arr->capacity) {
    arr->data = calloc(arr->item_size, capacity);
  }
}

static void* arrayPush(DynArray* arr) {
  if (!arr->data) {
    arr->data = calloc(arr->item_size, 4);
    arr->capacity = 4;
  }
  else if (arr->count == arr->capacity - 1) {
    arr->data = realloc(arr->data, arr->item_size * arr->capacity * 2);
    arr->capacity *= 2;
  }

  ++arr->count;
  byte* result = ((byte*) arr->data) + (arr->item_size * (arr->count - 1));
  return result;
}

static void arrayPushVal(DynArray* arr, void* in) {
  void* r = arrayPush(arr);
  memcpy(r, in, arr->item_size);
}

static void* arrayBegin(DynArray* arr) {
  return arr->data;
}

static void* arrayEnd(DynArray* arr) {
  return arr->data + arr->count * arr->item_size;
}

static void* arrayGet(DynArray* arr, int i) {
  return ((byte*) arr->data) + (arr->item_size * i);
}

static void arrayPop(DynArray* arr) {
  assert(arr->count > 0);
  --arr->count;
}

static int arrayCount(DynArray* arr) {
  return arr->count;
}
#define arraySize arrayCount

static void arrayGetData(DynArray* arr, void** data, int* size) {
  *data = arr->data;
  *size = arr->count * arr->item_size;
}

static int arrayCapacity(DynArray* arr) {
  return arr->capacity;
}

static void arrayFree(DynArray* arr) {
  free(arr->data);
  arr->capacity = 0;
  arr->data = 0;
  arr->count = 0;
}
/*
static void arrayFree(DynArray* arr) {
  free(arr->data);
}
*/

static Array arrayToArray(DynArray* arr) {
  Array result = {.count = arr->count, .data = arr->data};
  return result;
}

#ifdef DEBUG
static void arrayTest() {

  {
    DynArray a;
    arrayInit(&a, 30, sizeof(int));
    assert(arrayCount(&a) == 0);
    assert(arrayCapacity(&a) == 30);
    *(int*)arrayPush(&a) = 100;
    assert(arrayCount(&a) == 1);
    assert(arrayCapacity(&a) == 30);
    assert(*(int*)arrayGet(&a, 0) == 100);
    *(int*)arrayPush(&a) = 202;
    assert(arrayCount(&a) == 2);
    assert(arrayCapacity(&a) == 30);
    assert(*(int*)arrayGet(&a, 0) == 100);
    assert(*(int*)arrayGet(&a, 1) == 202);
    *(int*)arrayGet(&a, 0) = 300;
    assert(*(int*)arrayGet(&a, 0) == 300);
    assert(*(int*)arrayGet(&a, 1) == 202);
    arrayFree(&a);
  }

  {
    DynArray a;
    arrayInit(&a, 0, sizeof(int));
    assert(arrayCount(&a) == 0);
    assert(arrayCapacity(&a) == 0);
    arrayPush(&a);
    assert(arrayCount(&a) == 1);
    assert(arrayCapacity(&a) == 4);
    arrayPush(&a);
    assert(arrayCount(&a) == 2);
    assert(arrayCapacity(&a) == 4);
    arrayPush(&a);
    assert(arrayCount(&a) == 3);
    assert(arrayCapacity(&a) == 4);
    arrayPush(&a);
    assert(arrayCount(&a) == 4);
    assert(arrayCapacity(&a) == 8);
    arrayFree(&a);
  }

}
#endif