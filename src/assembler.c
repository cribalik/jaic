#include "common.h"
#include "array.h"
#define MEM_IMPLEMENTATION
#include "mem.h"
#include <ctype.h>

typedef union Register Register;
union Register {
  i8  i8;
  i16 i16;
  i32 i32;
  i64 i64;
  u8  u8;
  u16 u16;
  u32 u32;
  u64 u64;
  float f32;
  double f64;
  u64 ptr;
};

typedef char Instruction;

typedef enum RegisterType {
  TYPE_I64,
  TYPE_F64,
  TYPE_STACK
} RegisterType;

typedef enum {
  INSTR_MV = 1,
  INSTR_SUB,
  INSTR_PUSH,
  INSTR_CALL,
  TOKEN_IDENTIFIER = 5,
  TOKEN_INT = 6,
  TOKEN_FLOAT = 7,
  TOKEN_FN,
  TOKEN_EOL,
  TOKEN_ARG,
  TOKEN_LBRACE,
  TOKEN_RBRACE,
  TOKEN_EOF,
  TOKEN_OUT,
  TOKEN_VAR
} Token;

const char *token_names[] = {
  "<unknown>",
  "mv",
  "sub",
  "push",
  "call",
  "<identifier>"
  "<int>",
  "<float>",
  "fn",
  "\n",
  "param",
  "{",
  "}",
  "<eof>",
  "out",
  "var",
};

typedef struct AssemblerState AssemblerState;

typedef struct Symbol {
  char *name;
  int name_len;
  int address;
} Symbol;

typedef struct Variable {
  Token type;
  char *name;
  int name_len;
} Variable;

struct AssemblerState {
  FileCache file_cache;
  File file;

  Array(int) unresolved;
  Array(Symbol) symbols;
  Array(char) program;

  LStack identifier_mem;

  Array(Variable) variables;

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

static void assemble_error(const char *fmt, ...) {
  va_list args;

  va_start(args, fmt);
  filepos_print(&assembler.file_cache, get_filepos(), "error", RED, fmt, args);
  va_end(args);

  abort();
}

static char* identifier_alloc() {
  char *s;

  if (assembler.tok_identifier_len > assembler.identifier_mem.block_size)
    assemble_error("Identifier length (%i) exeeded limit (%i)\n", assembler.tok_identifier_len, assembler.identifier_mem.block_size);

  s = lstack_push_ex(&assembler.identifier_mem, assembler.tok_identifier_len+1, 1);
  memcpy(s, assembler.tok_identifier, assembler.tok_identifier_len);
  s[assembler.tok_identifier_len] = 0;
  return s;
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

static int token_identifier_eq(const char *str) {
  if ((int)strlen(str) != assembler.tok_identifier_len)
    return 0;
  return !memcmp(assembler.tok_identifier, str, assembler.tok_identifier_len);
}

static void token_next() {
  char *c;

  c = assembler.tok_cursor;
    while (*c != '\n' && isspace(*c))
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
    assembler.token = TOKEN_EOL;
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
      assembler.token = INSTR_MV;
    else if (token_identifier_eq("var"))
      assembler.token = TOKEN_VAR;
    else if (token_identifier_eq("push"))
      assembler.token = INSTR_PUSH;
    else if (token_identifier_eq("sub"))
      assembler.token = INSTR_SUB;
    else if (token_identifier_eq("param"))
      assembler.token = TOKEN_ARG;
    else if (token_identifier_eq("call"))
      assembler.token = INSTR_CALL;
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
      if (isalpha(*c) || *c == '_')
        parse_error("Invalid token\n");
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
    if (isalpha(*c) || *c == '_')
      parse_error("Invalid token\n");

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

static int address() {
  return array_len(assembler.program);
}

static Symbol* find_symbol(char *name, int name_len) {
  Symbol *s;

  array_find(assembler.symbols, s, 
    name_len == s->name_len && memcmp(name, s->name, s->name_len) == 0);
  return s;
}

static Variable* find_variable(char *name, int name_len) {
  Variable *v;

  array_find(assembler.variables, v,
    name_len == v->name_len && memcmp(name, v->name, v->name_len) == 0);
  return v;
}

static void save_symbol() {
  Symbol *sp;
  Symbol s;

  /* check if symbol already exists */
  sp = find_symbol(assembler.tok_identifier, assembler.tok_identifier_len);
  if (sp) {
    sp->address = address();
    return;
  }

  s.name = assembler.tok_identifier;
  s.name_len = assembler.tok_identifier_len;
  s.address = address();
  array_push(assembler.symbols, s);
}

static int symbol_address() {
  Symbol *s;

  s = find_symbol(assembler.tok_identifier, assembler.tok_identifier_len);
  /* if symbol doesn't exist, create a new symbol with negative address */
  if (!s) {
    static int invalid_address;
    Symbol new_sym;

    new_sym.name = assembler.tok_identifier;
    new_sym.name_len = assembler.tok_identifier_len;
    new_sym.address = --invalid_address;

    array_push(assembler.symbols, new_sym);
    s = array_last(assembler.symbols);
  }
  return s->address;
}

static const char* token_name() {
  return token_names[assembler.token];
}

static void newline() {
  if (assembler.token != TOKEN_EOL)
    assemble_error("Expected EOL, but got %s\n", token_name());
  while (assembler.token == TOKEN_EOL)
    token_next();
}

static void add_variable() {
  Variable var;

  var.type = assembler.token;

  token_next();
  if (assembler.token != TOKEN_IDENTIFIER)
    assemble_error("Expected output name, but got %s\n", token_name());

  /* add variable */
  /* check that variable doesn't already exists */
  if (find_variable(assembler.tok_identifier, assembler.tok_identifier_len))
    assemble_error("Multiple definition of %.*s\n", assembler.tok_identifier, assembler.tok_identifier_len);
  var.name = assembler.tok_identifier;
  var.name_len = assembler.tok_identifier_len;
  array_push(assembler.variables, var);
  token_next();

  newline();
}

static void add_instruction() {
  array_push(assembler.program, assembler.token);
  token_next();
}

#define add_val(type, val) (((Register*)array_push_n(assembler.program, (int)sizeof(Register)))->type = val)

static int get_variable_offset(char *name, int name_len) {
  Variable *var;
  int offset;

  var = find_variable(name, name_len);
  if (!var)
    assemble_error("Unknown variable %.*s\n", name_len, name);

  offset = array_len(assembler.variables) - (var - assembler.variables);
  if (var->type != TOKEN_VAR)
    ++offset;

  return offset;
}

static void add_symbol() {
  if (assembler.token != TOKEN_IDENTIFIER)
    assemble_error("Expected identifier for symbol, but got %s\n", token_name());

  add_val(i64, symbol_address());
  token_next();
}

static void add_location() {
  if (assembler.token == TOKEN_IDENTIFIER) {
    array_push(assembler.program, TYPE_STACK);
    add_val(i64, get_variable_offset(assembler.tok_identifier, assembler.tok_identifier_len));
  }
  else if (assembler.token == TOKEN_INT) {
    array_push(assembler.program, TYPE_I64);
    add_val(i64, assembler.tok_integer);
  }
  else if (assembler.token == TOKEN_FLOAT) {
    array_push(assembler.program, TYPE_F64);
    add_val(f64, assembler.tok_float);
  }
  token_next();
}

static void assemble() {
  token_next();
  while (1) {
    /* resolve tags */
    switch (assembler.token) {
    case TOKEN_EOF:
      goto assemble_done;

    case TOKEN_FN: {
      token_next();
      if (assembler.token != TOKEN_IDENTIFIER)
        assemble_error("Expected identifier after fn, but got %s\n", token_name());
      save_symbol();
      token_next();

      newline();

      if (assembler.token != TOKEN_LBRACE)
        assemble_error("Expected '{' after function declaration, but got %s\n", token_name());
      token_next();
      newline();

      array_resize(assembler.variables, 0);
      while (assembler.token == TOKEN_OUT)
        add_variable();
      while (assembler.token == TOKEN_ARG)
        add_variable();
      while (assembler.token == TOKEN_VAR)
        add_variable();

      while (1) {
        switch (assembler.token) {
        case INSTR_MV:
          add_instruction();
          add_location();
          add_location();
          break;

        case INSTR_SUB:
          add_instruction();
          add_location();
          add_location();
          break;

        case INSTR_PUSH:
          add_instruction();
          add_location();
          break;

        case INSTR_CALL:
          add_instruction();
          add_symbol();
          break;

        case TOKEN_RBRACE:
          token_next();
          goto fn_done;

        default:
          assemble_error("Invalid token %s\n", token_name());
        }
        newline();
      }

      fn_done:
      break;
    }
    default:
      assemble_error("Unexepected token %s\n", token_names[assembler.token]);
    }
    newline();
  }

  assemble_done:
  {
    FILE *f;
    f = fopen("output.zbin", "wb");
    if (!f)
      assemble_error("Failed to open output file %s: %s\n", "output.zbin", strerror(errno));
    if (fwrite(assembler.program, array_len(assembler.program), 1, f) != 1)
      assemble_error("Failed to write to output: %s\n", strerror(errno));
    fclose(f);
  }
  /* TODO: resolve symbols */
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