#ifndef JAIC_ARENA
#define JAIC_ARENA

/* TODO: alignment!! */
#include "common.c"
#include <stddef.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>

#ifndef ARENA_BLOCK_SIZE
#define ARENA_BLOCK_SIZE 32*1024*1024
#endif

typedef struct ArenaBlock {
  char data[ARENA_BLOCK_SIZE];
  int size;
  struct ArenaBlock* next;
  struct ArenaBlock* prev;
} ArenaBlock;

typedef ArenaBlock* MemArena;

static MemArena arena_create() {
  MemArena result;
  result = calloc(1, sizeof(ArenaBlock));
  return result;
}

static void* arena_push(MemArena* arena, int size) {
  char* result;
  assert(size <= ARENA_BLOCK_SIZE);
  if ((*arena)->size + size > ARENA_BLOCK_SIZE) {
    ArenaBlock* new_block = malloc(sizeof(ArenaBlock));
    (*arena)->next = new_block;
    new_block->prev = *arena;
    *arena = new_block;
    (*arena)->size = 0;
  }
  result = (*arena)->data + (*arena)->size;
  memset(result, 0, size);
  (*arena)->size += size;
  return result;
}

#define arena_pos(arena) ((void*) (arena->data + arena->size))

static void* arena_pop(MemArena* arena, int size) {
  while (1) {
    if (size <= (*arena)->size) {
      (*arena)->size -= size;
      return (*arena)->data + (*arena)->size;
    }
    else {
      ArenaBlock* tmp = *arena;
      size -= (*arena)->size;
      *arena = (*arena)->prev;
      free(tmp);
    }
  }
}

static void arena_pop_to(MemArena* arena, void* to) {
  char* dest = (char*) to;
  while (1) {
    if ((*arena)->data <= dest && (*arena)->data + (*arena)->size >= dest) {
      (*arena)->size = dest-(*arena)->data;
      return;
    }
    else {
      ArenaBlock* tmp = *arena;
      *arena = (*arena)->prev;
      free(tmp);
    }
  }
}

static void arena_reset(MemArena* arena) {
  while ((*arena)->prev) {
    ArenaBlock* next = *arena;
    free(*arena);
    *arena = next;
  }
  (*arena)->size = 0;
}

static char* arena_push_string(MemArena* arena, char* str) {
  int len = strlen(str) + 1;
  char* result = arena_push(arena, len);
  memcpy(result, str, len);
  return result;
}

#endif /* JAIC_ARENA */