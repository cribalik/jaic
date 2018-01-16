#include "parser.h"
#include <stdlib.h>
#include <assert.h>

typedef struct CompilerState {
  FileCache file_cache;
} CompilerState;
static CompilerState compiler;

static void compiler_init() {
  filecache_init(&compiler.file_cache);
}

static void compile_error_print(Node *where, const char *fmt, ...) {
  va_list args;

  va_start(args, fmt);
  filepos_print(&compiler.file_cache, where->head.file_pos, "error", RED, fmt, args);
  va_end(args);
}

static void compile_error_nopos(const char *fmt, ...) {
  va_list args;

  fprintf(stderr, "%serror:%s ", RED, RESET);
  va_start(args, fmt);
  vfprintf(stderr, fmt, args);
  va_end(args);
}

static void compile_error(Node *where, const char *fmt, ...) {
  va_list args;

  va_start(args, fmt);
  filepos_print(&compiler.file_cache, where->head.file_pos, "error", RED, fmt, args);
  va_end(args);
  exit(1);
}

static void print_usage() {
  fprintf(stderr, "Usage: zenc FILE\n");
}

static int get_arguments(Node *fn, Node *result[], int n) {
  Node **out;

  if (!fn || fn->head.type != NODE_TYPE_FN)
    return -1;

  out = result;
  for (fn = fn->function.parameters; fn; fn = fn->head.next) {
    if (out - result < n)
      *out = fn;
    ++out;
  }

  return out - result;
}

static void do_compile() {
  Node *n, *root;

  /* find main */
  root = 0;
  for (n = parser.root->compound.statements; n; n = n->head.next) {
    if (n->head.type == NODE_TYPE_FN && strcmp(n->function.name, "main") == 0) {
      if (root) {
        compile_error_print(root, "Multiple definitions of main\n");
        compile_error_print(n, "Definition here\n");
        exit(1);
      }

      root = n;
    }
  }
  if (!root)
    compile_error_nopos("main not found\n");

  /* check main args */
  if (root->function.parameters)
    compile_error(root, "main must not have any arguments\n");
}

int main(int argc, const char **argv) {
  --argc, ++argv;


  if (argc < 1)
    print_usage(), exit(1);

  compiler_init();

  init_formatting();

  parse(&compiler.file_cache, file_get(&compiler.file_cache, argv[0]));

  do_compile();

  return 0;
}