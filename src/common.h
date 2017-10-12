#ifndef COMMON_H
#define COMMON_H

#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <assert.h>

#ifdef LINUX
  #include <unistd.h>
#endif

static void die(const char* fmt, ...) {
  va_list args;
  va_start(args, fmt);
  vfprintf(stderr, fmt, args);
  va_end(args);
  fflush(stderr);
  #ifdef DEBUG_BUILD
    abort();
  #else
    exit(1);
  #endif
}

/* Terminal textstyles */

static char* RED = "";
static char* GREEN = "";
static char* YELLOW = "";
static char* BLUE = "";
static char* BOLD = "";
static char* UNBOLD = "";
static char* RESET_FORMAT = "";
static char* RESET_COLOR = "";
static char* RESET = "";

static void init_formatting() {
  #ifdef LINUX
  if (isatty(1)) {
    RED = "\x1B[31m";
    GREEN = "\x1B[32m";
    YELLOW = "\x1B[33m";
    BLUE = "\x1B[34m";
    BOLD = "\x1B[1m";
    UNBOLD = "\x1B[21m";
    RESET_FORMAT = "\x1B[0m";
    RESET_COLOR = "\x1B[39m";
    RESET = "\x1B[0m\x1B[39m";
  }
  #endif /* LINUX */
}

typedef struct FileCache FileCache;
typedef struct File File;
typedef struct FilePos FilePos;

struct FileCache {
  /* TODO: implement */
  File *file;
};

static void filecache_init(FileCache *cache) {
  (void)cache;
}

struct File {
  char const *name;
  char *data, *end;
  int refcount;
};

struct FilePos {
  const char *name;
  int offset;
};

static File file_open(const char *filename);
static File* file_get(FileCache *cache, const char *filename);
static void file_put(FileCache *cache, const char *filename);

static File* file_get(FileCache *cache, const char *filename) {
  if (!cache->file) {
    cache->file = malloc(sizeof(*cache->file));
    *cache->file = file_open(filename);
  }
  ++cache->file->refcount;
  return cache->file;
}

static void file_put(FileCache *cache, const char *filename) {
  (void)filename;
  --cache->file->refcount;
  assert(cache->file->refcount >= 0);
}

static void filepos_print(FileCache *cache, FilePos file_pos, const char *prefix, const char *color, const char *fmt, va_list args) {
  int line, col;
  char *c, *pos, *last_line;
  File *file;

  file = file_get(cache, file_pos.name);

  line = 1;
  last_line = file->data;
  pos = file->data + file_pos.offset;
  for (c = file->data; *c && c < pos; ++c) {
    if (*c == '\n') {
      last_line = c+1;
      ++line;
    }
  }
  col = pos - last_line;
  while (*c && *c != '\n')
    ++c;

  fprintf(stderr, "%s:%i:%i: ", file->name, line, col+1);

  fprintf(stderr, "%s%s:%s ", color, prefix, RESET);

  vfprintf(stderr, fmt, args);

  fprintf(stderr, "%.*s\n", (int)(c - last_line), last_line);

  while (col--)
    fputc(' ', stderr);
  fprintf(stderr, "^\n");
}


static File file_open(const char *filename) {
  File result = {0};
  FILE *f;
  char *data;
  long size, num_read;

  f = fopen(filename, "rb");
  if (!f)
    die("Failed to open file %s: %s\n", filename, strerror(errno));

  /* calculate size */
  fseek(f, 0, SEEK_END);
  size = ftell(f);
  fseek(f, 0, SEEK_SET);

  /* read data */
  data = malloc(size+1);
  data[size] = 0;
  num_read = fread(data, 1, size, f);
  if (num_read != size)
    die("Failed to read file %s: %s\n", filename, strerror(errno));

  fclose(f);

  result.name = filename;
  result.data = data;
  result.end = result.data + size;
  /* TODO: ids */
  result.refcount = 0;

  return result;
}

#endif /* COMMON_H */