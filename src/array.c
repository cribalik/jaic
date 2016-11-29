#include <stdlib.h>
#include <string.h>
#include <assert.h>

typedef char byte;

typedef struct {
  int count, capacity;
  void* data;
} Array;

static void _arrayInit(Array* arr, int item_size, int capacity) {
  arr->data = 0;
  arr->count = 0;
  arr->capacity = capacity;
  if (arr->capacity) {
    arr->data = malloc(item_size * capacity);
  }
}
#define arrayInit(type, arr, capacity) _arrayInit(arr, sizeof(type), capacity)

static void* _arrayPush(Array* arr, int item_size) {
  if (!arr->data) {
    arr->data = malloc(item_size * 4);
    arr->capacity = 4;
  }
  else if (arr->count == arr->capacity - 1) {
    void* newData = malloc(item_size * arr->capacity * 2);
    memcpy(newData, arr->data, item_size * arr->count);
    free(arr->data);
    arr->data = newData;
    arr->capacity *= 2;
  }

  ++arr->count;
  return ((byte*) arr->data) + (item_size * (arr->count - 1));
}
#define arrayPush(type, arr) ((type*) _arrayPush(arr, sizeof(type)))

static void* _arrayGet(Array* arr, int item_size, int i) {
  return ((byte*) arr->data) + (item_size * i);
}
#define arrayGet(type, arr, i) ((type*) _arrayGet(arr, sizeof(type), i))

static int arrayCount(Array* arr) {
  return arr->count;
}
#define arraySize arrayCount

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
    arrayInit(int, &a, 30);
    assert(arrayCount(&a) == 0);
    assert(arrayCapacity(&a) == 30);
    *arrayPush(int, &a) = 100;
    assert(arrayCount(&a) == 1);
    assert(arrayCapacity(&a) == 30);
    assert(*arrayGet(int, &a, 0) == 100);
    *arrayPush(int, &a) = 202;
    assert(arrayCount(&a) == 2);
    assert(arrayCapacity(&a) == 30);
    assert(*arrayGet(int, &a, 0) == 100);
    assert(*arrayGet(int, &a, 1) == 202);
    *arrayGet(int, &a, 0) = 300;
    assert(*arrayGet(int, &a, 0) == 300);
    assert(*arrayGet(int, &a, 1) == 202);
  }

  {
    Array a;
    arrayInit(int, &a, 0);
    assert(arrayCount(&a) == 0);
    assert(arrayCapacity(&a) == 0);
    arrayPush(int, &a);
    assert(arrayCount(&a) == 1);
    assert(arrayCapacity(&a) == 4);
    arrayPush(int, &a);
    assert(arrayCount(&a) == 2);
    assert(arrayCapacity(&a) == 4);
    arrayPush(int, &a);
    assert(arrayCount(&a) == 3);
    assert(arrayCapacity(&a) == 4);
    arrayPush(int, &a);
    assert(arrayCount(&a) == 4);
    assert(arrayCapacity(&a) == 8);
  }

}
#endif