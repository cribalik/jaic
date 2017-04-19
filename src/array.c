#ifndef JAIC_ARRAY
#define JAIC_ARRAY

#include <stdlib.h>
#include <string.h>
#include <assert.h>

typedef struct {
  int count;
  void* data;
} Array;

typedef struct {
  int count, capacity, item_size;
  void* data;
} DynArray;

#define ARRAY_FOREACH(arr, it, end) for (it = array_begin(arr), end = array_end(end); it != end; ++it)

static DynArray array_create(int capacity, int item_size) {
  DynArray result;
  result.data = 0;
  result.count = 0;
  result.capacity = capacity;
  result.item_size = item_size;
  if (capacity) {
    result.data = calloc(item_size, capacity);
  }
  return result;
}

static void* array_push(DynArray* arr) {
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

static void* array_pushN(DynArray* arr, int n) {
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

static void* array_last(DynArray* arr) {
  return ((char*) arr->data) + (arr->count - 1) * arr->item_size;
}

static void* array_push_val(DynArray* arr, void* in) {
  void* r = array_push(arr);
  memcpy(r, in, arr->item_size);
  return array_last(arr);
}

static void* array_begin(DynArray* arr) {
  return arr->data;
}

static void* array_end(DynArray* arr) {
  return ((char*) arr->data) + (arr->count * arr->item_size);
}

static void* array_get(DynArray* arr, int i) {
  return ((char*) arr->data) + (arr->item_size * i);
}

static void array_pop(DynArray* arr) {
  assert(arr->count > 0);
  --arr->count;
}

static void array_pop_N(DynArray* arr, int n) {
  assert(arr->count >= n);
  arr->count -= n;
}

static int array_count(DynArray* arr) {
  return arr->count;
}
#define arraySize array_count

static void array_free(DynArray* arr) {
  free(arr->data);
  arr->data = 0;
  arr->capacity = 0;
  arr->count = 0;
}

static Array array_to_array(DynArray* arr) {
  Array result;
  result.count = arr->count;
  result.data = arr->data;
  return result;
}

#ifdef DEBUG
static void array_test() {

  {
    DynArray a;
    a = array_create(30, sizeof(int));
    assert(array_count(&a) == 0);
    assert(a.capacity == 30);
    *(int*)array_push(&a) = 100;
    assert(array_count(&a) == 1);
    assert(a.capacity == 30);
    assert(*(int*)array_get(&a, 0) == 100);
    *(int*)array_push(&a) = 202;
    assert(array_count(&a) == 2);
    assert(a.capacity == 30);
    assert(*(int*)array_get(&a, 0) == 100);
    assert(*(int*)array_get(&a, 1) == 202);
    *(int*)array_get(&a, 0) = 300;
    assert(*(int*)array_get(&a, 0) == 300);
    assert(*(int*)array_get(&a, 1) == 202);
    array_free(&a);
  }

  {
    DynArray a;
    a = array_create(0, sizeof(int));
    assert(array_count(&a) == 0);
    assert(a.capacity == 0);
    array_push(&a);
    assert(array_count(&a) == 1);
    assert(a.capacity == 4);
    array_push(&a);
    assert(array_count(&a) == 2);
    assert(a.capacity == 4);
    array_push(&a);
    assert(array_count(&a) == 3);
    assert(a.capacity == 4);
    array_push(&a);
    assert(array_count(&a) == 4);
    assert(a.capacity == 8);
    array_free(&a);
  }

}
#endif

#endif /* JAIC_ARRAY */