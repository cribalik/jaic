#include <stdlib.h>
#include <string.h>
#include <assert.h>

typedef char byte;

typedef struct {
  int count, capacity, item_size;
  void* data;
} Array;

static void arrayInit(Array* arr, int capacity, int item_size) {
  arr->data = 0;
  arr->count = 0;
  arr->capacity = capacity;
  arr->item_size = item_size;
  if (arr->capacity) {
    arr->data = calloc(arr->item_size, capacity);
  }
}

static void* arrayPush(Array* arr) {
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

static void arrayPushVal(Array* arr, void* in) {
  void* r = arrayPush(arr);
  memcpy(r, in, arr->item_size);
}

static void* arrayBegin(Array* arr) {
  return arr->data;
}

static void* arrayGet(Array* arr, int i) {
  return ((byte*) arr->data) + (arr->item_size * i);
}

static int arrayCount(Array* arr) {
  return arr->count;
}
#define arraySize arrayCount

static void arrayGetData(Array* arr, void** data, int* size) {
  *data = arr->data;
  *size = arr->count * arr->item_size;
}

static int arrayCapacity(Array* arr) {
  return arr->capacity;
}

static void arrayFree(Array* arr) {
  free(arr->data);
  arr->capacity = 0;
  arr->data = 0;
  arr->count = 0;
}
/*
static void arrayFree(Array* arr) {
  free(arr->data);
}
*/

#ifdef DEBUG
static void arrayTest() {

  {
    Array a;
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
  }

  {
    Array a;
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
  }

}
#endif