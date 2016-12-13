// TODO: alignment!!
#include "utils.c"
#include <stddef.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>
typedef char byte;
#ifndef ARENA_BLOCK_SIZE
#define ARENA_BLOCK_SIZE 32*1024*1024
#endif

typedef struct SArenaBlock {
  byte data[ARENA_BLOCK_SIZE];
  int size;
  struct SArenaBlock* next;
  struct SArenaBlock* prev;
} ArenaBlock;

typedef ArenaBlock* MemArena;

void arenaInit(MemArena* arena) {
  *arena = calloc(1, sizeof(ArenaBlock));
  (*arena)->size = 0;
  (*arena)->prev = 0;
}

static void* arenaPush(MemArena* arena, int size) {
  assert(size <= ARENA_BLOCK_SIZE);
  if ((*arena)->size + size > ARENA_BLOCK_SIZE) {
    // guess we get some internal fragmentation by doing this but oh well
    ArenaBlock* new_block = malloc(sizeof(ArenaBlock));
    (*arena)->next = new_block;
    new_block->prev = *arena;
    *arena = new_block;
    (*arena)->size = 0;
  }
  byte* result = (*arena)->data + (*arena)->size;
  memset(result, 0, size);
  (*arena)->size += size;
  return result;
}

#define arenaGetCurrent(arena) ((void*) (arena->data + arena->size))

static void* arenaPop(MemArena* arena, int size) {
  while (1) {
    if (size <= (*arena)->size) {
      (*arena)->size -= size;
      return (*arena)->data + (*arena)->size;
    }
    ArenaBlock* tmp = *arena;
    size -= (*arena)->size;
    *arena = (*arena)->prev;
    free(tmp);
  }
}

static void arenaPopTo(MemArena* arena, void* _to) {
  byte* to = (byte*) _to;
  while (1) {
    if ((*arena)->data <= to && (*arena)->data + (*arena)->size >= to) {
      (*arena)->size = to-(*arena)->data;
      return;
    }
    ArenaBlock* tmp = *arena;
    *arena = (*arena)->prev;
    free(tmp);
  }
}