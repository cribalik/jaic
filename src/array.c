#ifndef JAIC_ARRAY
#define JAIC_ARRAY

#include <stdlib.h>
#include <string.h>
#include <assert.h>

#define internal static

typedef struct {
  int count;
  void* data;
} Array;

typedef struct {
  int count, capacity, item_size;
  void* data;
} DynArray;

internal void arrayInit(DynArray* arr, int capacity, int item_size) {
  arr->data = 0;
  arr->count = 0;
  arr->capacity = capacity;
  arr->item_size = item_size;
  if (arr->capacity) {
    arr->data = calloc(arr->item_size, capacity);
  }
}

internal void* arrayPush(DynArray* arr) {
  if (!arr->data) {
    arr->data = calloc(arr->item_size, 4);
    arr->capacity = 4;
  }
  else if (arr->count == arr->capacity - 1) {
    arr->data = realloc(arr->data, arr->item_size * arr->capacity * 2);
    arr->capacity *= 2;
  }

  ++arr->count;
  return ((char*) arr->data) + (arr->item_size * (arr->count - 1));
}

internal void* arrayPushN(DynArray* arr, int n) {
  if (!arr->data) {
    int size = n > 4 ? n : 4;
    arr->data = calloc(arr->item_size, size);
    arr->capacity = size;
  } else if (arr->count + n >= arr->capacity) {
    int new_capacity = n > arr->capacity * 2 ? n * 2 : arr->capacity * 2;
    arr->data = realloc(arr->data, arr->item_size * new_capacity);
    arr->capacity = new_capacity;
  }

  arr->count += n;
  return ((char*) arr->data) + (arr->item_size * (arr->count - n));
}

internal void* arrayLast(DynArray* arr) {
  return ((char*) arr->data) + (arr->count - 1) * arr->item_size;
}

internal void* arrayPushVal(DynArray* arr, void* in) {
  void* r = arrayPush(arr);
  memcpy(r, in, arr->item_size);
  return arrayLast(arr);
}

internal void* arrayBegin(DynArray* arr) {
  return arr->data;
}

internal void* arrayEnd(DynArray* arr) {
  return ((char*) arr->data) + (arr->count * arr->item_size);
}

internal void* arrayGet(DynArray* arr, int i) {
  return ((char*) arr->data) + (arr->item_size * i);
}

internal void arrayPop(DynArray* arr) {
  assert(arr->count > 0);
  --arr->count;
}

internal void arrayPopN(DynArray* arr, int n) {
  assert(arr->count >= n);
  arr->count -= n;
}

internal int arrayCount(DynArray* arr) {
  return arr->count;
}
#define arraySize arrayCount

internal void arrayGetData(DynArray* arr, void** data, int* size) {
  *data = arr->data;
  *size = arr->count * arr->item_size;
}

internal int arrayCapacity(DynArray* arr) {
  return arr->capacity;
}

internal void arrayFree(DynArray* arr) {
  free(arr->data);
  arr->capacity = 0;
  arr->data = 0;
  arr->count = 0;
}
/*
internal void arrayFree(DynArray* arr) {
  free(arr->data);
}
*/

internal Array arrayToArray(DynArray* arr) {
  Array result;
  result.count = arr->count;
  result.data = arr->data;
  return result;
}

#ifdef DEBUG
internal void arrayTest() {

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

#endif /* JAIC_ARRAY */