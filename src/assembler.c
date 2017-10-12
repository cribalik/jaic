#include "common.h"
#include <ctype.h>

typedef enum {
  TOKEN_FN,
  TOKEN_ENDLINE,
  TOKEN_LBRACE,
  TOKEN_RBRACE,
  TOKEN_EOF,
  TOKEN_INT,
  TOKEN_FLOAT,
  TOKEN_OUT,
  TOKEN_VAR,
  TOKEN_MV,
  TOKEN_PUSH,
  TOKEN_CALL,
  TOKEN_IDENTIFIER
} Token;

const char *token_names[] = {
  "fn",
  "<eol>",
  "{",
  "}",
  "<eof>",
  "<int>",
  "<float>",
  "out",
  "var",
  "mv",
  "push",
  "call",
  "<identifier>"
};

typedef struct AssemblerState AssemblerState;

struct AssemblerState {
  FileCache file_cache;
  File file;

  char *tok_cursor, *tok_start;
  Token token;
  char *tok_identifier;
  int tok_identifier_len;
  long tok_integer;
  double tok_float;
};
static AssemblerState assembler;

static FilePos get_filepos() {
  FilePos p;
  p.offset = (int)(assembler.tok_start - assembler.file.data);
  p.name = assembler.file.name;
  return p;
}

static void parse_error(const char *fmt, ...) {
  va_list args;

  va_start(args, fmt);
  filepos_print(&assembler.file_cache, get_filepos(), "error", RED, fmt, args);
  va_end(args);

  #ifdef DEBUG_BUILD
    abort();
  #else
    exit(1);
  #endif
}

static void assemble_error(const char *fmt, ...) {
  va_list args;

  va_start(args, fmt);
  filepos_print(&assembler.file_cache, get_filepos(), "error", RED, fmt, args);
  va_end(args);

  exit(1);
}

static int token_identifier_eq(const char *str) {
  if ((int)strlen(str) != assembler.tok_identifier_len)
    return 0;
  return !memcmp(assembler.tok_identifier, str, assembler.tok_identifier_len);
}

static void token_next() {
  char *c;

  c = assembler.tok_cursor;
  while (isspace(*c))
    ++c;

  if (*c == ';') {
    while (*c && *c != '\n')
      ++c;
    if (!*c) {
      assembler.token = TOKEN_EOF;
      goto done;
    }
  }

  assembler.tok_start = c;

  if (*c == '\n') {
    ++c;
    assembler.token = TOKEN_ENDLINE;
  }

  else if (*c == '{') {
    ++c;
    assembler.token = TOKEN_LBRACE;
  }

  else if (*c == '}') {
    ++c;
    assembler.token = TOKEN_RBRACE;
  }

  /* identifier */
  else if (isalpha(*c) || *c == '_') {
    while (isalpha(*c) || isdigit(*c) || *c=='_')
      ++c;

    assembler.tok_identifier = assembler.tok_start;
    assembler.tok_identifier_len = c - assembler.tok_start;
    if (token_identifier_eq("fn"))
      assembler.token = TOKEN_FN;
    else if (token_identifier_eq("out"))
      assembler.token = TOKEN_OUT;
    else if (token_identifier_eq("mv"))
      assembler.token = TOKEN_MV;
    else if (token_identifier_eq("var"))
      assembler.token = TOKEN_VAR;
    else if (token_identifier_eq("push"))
      assembler.token = TOKEN_PUSH;
    else if (token_identifier_eq("call"))
      assembler.token = TOKEN_CALL;
    else
      assembler.token = TOKEN_IDENTIFIER;
  }

  /* number literal */
  else if (isdigit(*c)) {
    char tmp;

    while (isdigit(*c))
      ++c;

    /* integer */
    if (*c != '.') {
      tmp = *c;
      *c = '\0';
      assembler.tok_integer = atoi(assembler.tok_start);
      *c = tmp;

      /* TODO: check for suffixes here e.g. 10u32 */
      assembler.token = TOKEN_INT;
      goto done;
    }

    /* float */
    ++c;
    if (!isdigit(*c))
      parse_error("Expected digit after '.'\n");

    while (isdigit(*c))
      ++c;

    /* TODO: check for suffixes here e.g. 30.0f */
    tmp = *c;
    *c = '\0';
    assembler.tok_float = strtod(assembler.tok_start, 0);
    *c = tmp;

    assembler.token = TOKEN_FLOAT;
  }

  else if (!*c)
    assembler.token = TOKEN_EOF;

  else
    assemble_error("Unknown token\n");

  done:
  assembler.tok_cursor = c;
}

static void assemble() {

  token_next();

  while (assembler.token != TOKEN_EOF) {
    token_next();
    printf("%s\n", token_names[assembler.token]);
    #if 0
    switch (assembler.token) {
      case TOKEN_FN:
        break;
      case TOKEN_ENDLINE:
        break;
      case TOKEN_LBRACE:
        break;
      case TOKEN_RBRACE:
        break;
    }
    #endif
  }
}

static void assembler_init(const char *filename) {
  filecache_init(&assembler.file_cache);
  assembler.file = *file_get(&assembler.file_cache, filename);
  assembler.tok_cursor = assembler.tok_start = assembler.file.data;
}

int main(int argc, const char **argv) {
  --argc, ++argv;

  if (argc < 1)
    fprintf(stderr, "Usage: zasm FILE\n"), exit(1);

  assembler_init(argv[0]);

  assemble();

  return 0;
}