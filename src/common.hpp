#ifndef COMMON_H
#define COMMON_H

#ifdef _MSC_VER
  #define OS_WINDOWS 1
#else
  #define OS_LINUX 1
#endif

#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <assert.h>
#include <inttypes.h>

#ifdef OS_LINUX
  #include <unistd.h>
#endif

#define STATIC_ASSERT(expr, name) typedef char static_assert_##name[expr?1:-1]
#define ARRAY_LEN(a) (int)(sizeof(a)/sizeof(*(a)))

#ifdef _MSC_VER
  typedef __int8 i8;
  typedef __int16 i16;
  typedef __int32 i32;
  typedef __int64 i64;
  typedef unsigned __int8 u8;
  typedef unsigned __int16 u16;
  typedef unsigned __int32 u32;
  typedef unsigned __int64 u64;
#else
  /* let's hope stdint has us covered */
  #include <stdint.h>
  typedef int8_t i8;
  typedef int16_t i16;
  typedef int32_t i32;
  typedef int64_t i64;
  typedef uint8_t u8;
  typedef uint16_t u16;
  typedef uint32_t u32;
  typedef uint64_t u64;
#endif

STATIC_ASSERT(sizeof(i8) == 1, i8_is_1_byte);
STATIC_ASSERT(sizeof(i16) == 2, i16_is_2_bytes);
STATIC_ASSERT(sizeof(i32) == 4, i32_is_4_bytes);
STATIC_ASSERT(sizeof(i64) == 8, i64_is_8_bytes);
STATIC_ASSERT(sizeof(u8) == 1, u8_is_1_byte);
STATIC_ASSERT(sizeof(u16) == 2, u16_is_2_bytes);
STATIC_ASSERT(sizeof(u32) == 4, u32_is_4_bytes);
STATIC_ASSERT(sizeof(u64) == 8, u64_is_8_bytes);

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

static const char* RED = "";
static const char* GREEN = "";
static const char* YELLOW = "";
static const char* BLUE = "";
static const char* BOLD = "";
static const char* UNBOLD = "";
static const char* RESET_FORMAT = "";
static const char* RESET_COLOR = "";
static const char* RESET = "";

static void init_formatting() {
  #ifdef OS_LINUX
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
  #endif /* OS_LINUX */
}


struct FilePos {
  const char *name;
  int offset;
};

struct File;
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

static File file_open(const char *filename);

static File* file_get(FileCache *cache, const char *filename) {
  if (!cache->file) {
    cache->file = (File*)malloc(sizeof(*cache->file));
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

typedef struct String {
  const char *str;
  int len;
} String;

static int streq(String a, String b) {
  return a.len == b.len && !memcmp(a.str, b.str, a.len);
}

static int streq(String a, const char *b) {
  return a.str && b && (int)strlen(b) == a.len && !memcmp(a.str, b, a.len);
}

enum DataType {
  DATATYPE_NULL = 0,

  DATATYPE_LITERAL_I64,
  DATATYPE_LITERAL_F64,
  DATATYPE_STACK,

  DATATYPE_NUM
};

typedef u8 DataType_t;

static const char *datatype_names[] = {
  0,
  "i64",
  "f64",
  0
};
STATIC_ASSERT(ARRAY_LEN(datatype_names) == DATATYPE_NUM, all_datatypes_named);

enum Instruction {
  INSTR_NULL = 0,

  INSTR_MV,
  INSTR_SUBI,
  INSTR_SUBF,
  INSTR_ADDI,
  INSTR_ADDF,
  INSTR_MULI,
  INSTR_MULF,
  INSTR_DIVI,
  INSTR_DIVF,
  INSTR_PUSH,
  INSTR_PUSHN,
  INSTR_POP,
  INSTR_CALL,
  INSTR_ECALL,
  INSTR_RET,
  INSTR_EXIT,

  INSTR_NUM
};

typedef u8 Instruction_t;

static const char *instruction_names[] = {
  0,
  "mv",
  "subi",
  "subf",
  "addi",
  "addf",
  "muli",
  "mulf",
  "divi",
  "divf",
  "push",
  "pushn",
  "pop",
  "call",
  "ecall",
  "ret",
  "exit"
};
STATIC_ASSERT(ARRAY_LEN(instruction_names) == INSTR_NUM, all_instructions_named);

union Word {
  i8  int8;
  i16 int16;
  i32 int32;
  i64 int64;
  u8  uint8;
  u16 uint16;
  u32 uint32;
  u64 uint64;
  float f32;
  double f64;
  u64 ptr;
};

typedef enum VMFun {
  VMFUN_NULL,

  VMFUN_PRINT,

  VMFUN_NUM
} VMFun;

static const char *vmfun_names[] = {
  0,
  "print"
};
STATIC_ASSERT(ARRAY_LEN(vmfun_names) == VMFUN_NUM, all_vmfuns_named);

static bool _file_open(FILE **f, const char *filename, const char *mode) {
  #ifdef OS_WINDOWS
  return !fopen_s(f, filename, mode);
  #else
  *f = fopen(filename, mode);
  return *f != NULL;
  #endif
}

static bool file_write(const char *filename, u8 *data, int size) {
  FILE *f;
  if (!_file_open(&f, filename, "wb"))
    return false;

  if (fwrite(data, size, 1, f) != 1) {
    fclose(f);
    return false;
  }

  fclose(f);
  return true;
}

static const char* get_error() {
  #ifdef OS_WINDOWS
  static char buf[512];
  strerror_s(buf, sizeof(buf), errno);
  return buf;
  #else
  return strerror(errno);
  #endif
}

static File file_open(const char *filename) {
  File result = {};
  FILE *f;
  char *data;
  long size, num_read;

  if (!_file_open(&f, filename, "rb"))
    die("Failed to open file %s: %s\n", filename, get_error());

  /* calculate size */
  fseek(f, 0, SEEK_END);
  size = ftell(f);
  fseek(f, 0, SEEK_SET);

  /* read data */
  data = (char*)malloc(size+1);
  data[size] = 0;
  num_read = fread(data, 1, size, f);
  if (num_read != size)
    die("Failed to read file %s: %s\n", filename, get_error());

  fclose(f);

  result.name = filename;
  result.data = data;
  result.end = result.data + size;
  /* TODO: ids */
  result.refcount = 0;

  return result;
}

#endif /* COMMON_H */