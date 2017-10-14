#include "common.h"
#include "array.h"
#define MEM_IMPLEMENTATION
#include "mem.h"
#include <ctype.h>

typedef enum {
  TOKEN_NULL = 0,
  TOKEN_MV = INSTR_MV,
  TOKEN_SUBI = INSTR_SUBI,
  TOKEN_SUBF = INSTR_SUBF,
  TOKEN_PUSH = INSTR_PUSH,
  TOKEN_CALL = INSTR_CALL,
  TOKEN_ECALL = INSTR_ECALL,
  TOKEN_IDENTIFIER,
  TOKEN_INT,
  TOKEN_FLOAT,
  TOKEN_FN,
  TOKEN_EOL,
  TOKEN_ARG,
  TOKEN_LBRACE,
  TOKEN_RBRACE,
  TOKEN_EOF,
  TOKEN_OUT,
  TOKEN_VAR,
  TOKEN_I64,
  TOKEN_F64,

  TOKEN_COUNT
} Token;

const char *token_names[TOKEN_COUNT];
typedef struct AssemblerState AssemblerState;

typedef struct Symbol {
  String name;
  int address;
} Symbol;

typedef struct Variable {
  Token vartype;
  RegisterType type;
  String name;
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
  String tok_identifier;
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

  if (assembler.tok_identifier.len > assembler.identifier_mem.block_size)
    assemble_error("Identifier length (%i) exeeded limit (%i)\n", assembler.tok_identifier.len, assembler.identifier_mem.block_size);

  s = lstack_push_ex(&assembler.identifier_mem, assembler.tok_identifier.len+1, 1);
  memcpy(s, assembler.tok_identifier.str, assembler.tok_identifier.len);
  s[assembler.tok_identifier.len] = 0;
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
  return streqc(assembler.tok_identifier, str);
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

    assembler.tok_identifier.str = assembler.tok_start;
    assembler.tok_identifier.len = c - assembler.tok_start;
    if (token_identifier_eq("fn"))
      assembler.token = TOKEN_FN;
    else if (token_identifier_eq("out"))
      assembler.token = TOKEN_OUT;
    else if (token_identifier_eq("mv"))
      assembler.token = INSTR_MV;
    else if (token_identifier_eq("var"))
      assembler.token = TOKEN_VAR;
    else if (token_identifier_eq("i64"))
      assembler.token = TOKEN_I64;
    else if (token_identifier_eq("f64"))
      assembler.token = TOKEN_F64;
    else if (token_identifier_eq("push"))
      assembler.token = INSTR_PUSH;
    else if (token_identifier_eq("subi"))
      assembler.token = INSTR_SUBI;
    else if (token_identifier_eq("subf"))
      assembler.token = INSTR_SUBF;
    else if (token_identifier_eq("param"))
      assembler.token = TOKEN_ARG;
    else if (token_identifier_eq("call"))
      assembler.token = INSTR_CALL;
    else if (token_identifier_eq("ecall"))
      assembler.token = INSTR_ECALL;
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

static Symbol* find_symbol(String name) {
  Symbol *s;

  array_find(assembler.symbols, s, streq(s->name, name));
  return s;
}

static Variable* find_variable(String name) {
  Variable *v;

  array_find(assembler.variables, v, streq(v->name, name));
  return v;
}

static void save_symbol() {
  Symbol *sp;
  Symbol s;

  /* check if symbol already exists */
  sp = find_symbol(assembler.tok_identifier);
  if (sp) {
    int i;
    int old_addr;
    /* resolve the places we used it */

    if (sp->address >= 0)
      assemble_error("%.*s defined multiple times\n", assembler.tok_identifier.len, assembler.tok_identifier.str);
    old_addr = sp->address;
    sp->address = address();

    for (i = 0; i < array_len(assembler.unresolved); ++i) {
      if (assembler.unresolved[i] != old_addr)
        continue;
      ((Register*)&assembler.program[assembler.unresolved[i]])->i64 = sp->address;
      array_remove_fast(assembler.unresolved, i);
      --i;
    }
    return;
  }

  s.name = assembler.tok_identifier;
  s.address = address();
  array_push(assembler.symbols, s);
}

static int symbol_address() {
  Symbol *s;

  s = find_symbol(assembler.tok_identifier);
  /* if symbol doesn't exist, create a new symbol with negative address */
  if (!s) {
    static int invalid_address;
    Symbol new_sym;

    new_sym.name = assembler.tok_identifier;
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

static int is_type() {
  return assembler.token == TOKEN_I64 || assembler.token == TOKEN_F64;
}

static RegisterType get_type() {
  switch (assembler.token) {
    case TOKEN_I64: return TYPE_I64;
    case TOKEN_F64: return TYPE_F64;
    default: return 0;
  }
}

static void add_variable() {
  Variable var;

  var.vartype = assembler.token;

  /* type */
  token_next();
  var.type = get_type();
  if (!var.type)
    assemble_error("Expected type, but got %s\n", token_name());

  /* name */
  token_next();
  if (assembler.token != TOKEN_IDENTIFIER)
    assemble_error("Expected output name, but got %s\n", token_name());

  /* add variable */
  /* check that variable doesn't already exists */
  if (find_variable(assembler.tok_identifier))
    assemble_error("Multiple definition of %.*s\n", assembler.tok_identifier.len, assembler.tok_identifier.str);
  var.name = assembler.tok_identifier;
  array_push(assembler.variables, var);
  token_next();

  newline();
}

static void add_instruction(Instruction instr) {
  array_push(assembler.program, instr);
  token_next();
}

#define add_val(type, val) (((Register*)array_push_n(assembler.program, (int)sizeof(Register)))->type = val)

static int get_variable_offset(String name) {
  Variable *var;
  int offset;

  var = find_variable(name);
  if (!var)
    assemble_error("Unknown variable %.*s\n", name.len, name.str);

  offset = array_len(assembler.variables) - (var - assembler.variables);
  if (var->vartype != TOKEN_VAR)
    ++offset;

  return offset;
}

static void add_symbol() {
  if (assembler.token != TOKEN_IDENTIFIER)
    assemble_error("Expected identifier for symbol, but got %s\n", token_name());

  add_val(i64, symbol_address());
  token_next();
}

static void add_location(int dest) {
  if (assembler.token == TOKEN_IDENTIFIER) {
    array_push(assembler.program, TYPE_STACK);
    add_val(i64, get_variable_offset(assembler.tok_identifier));
    goto done;
  }
  if (dest)
    assemble_error("Only integers can be used as destination\n");

  if (assembler.token == TOKEN_INT) {
    array_push(assembler.program, TYPE_I64);
    add_val(i64, assembler.tok_integer);
  }
  else if (assembler.token == TOKEN_FLOAT) {
    array_push(assembler.program, TYPE_F64);
    add_val(f64, assembler.tok_float);
  }

  done:
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
        case TOKEN_MV:
          add_instruction(INSTR_MV);
          add_location(1);
          add_location(0);
          break;

        case TOKEN_SUBI:
          add_instruction(INSTR_SUBI);
          add_location(1);
          add_location(0);
          break;

        case TOKEN_SUBF:
          add_instruction(INSTR_SUBF);
          add_location(1);
          add_location(0);
          break;

        case TOKEN_PUSH:
          add_instruction(INSTR_PUSH);
          add_location(0);
          break;

        case TOKEN_CALL:
          add_instruction(INSTR_CALL);
          add_symbol();
          break;

        case TOKEN_ECALL: {
          int i;

          add_instruction(INSTR_ECALL);

          for (i = 0; i < ARRAY_LEN(host_api); ++i) {
            if (streq(host_api[i].name, assembler.tok_identifier))
              break;
          }
          if (i == ARRAY_LEN(host_api))
            assemble_error("Unknown host api function %.*s\n", assembler.tok_identifier.len, assembler.tok_identifier.str);
          add_val(u64, host_api[i].id);
          token_next();

          break;
        }

        case TOKEN_RBRACE:
          token_next();
          goto fn_done;

        case TOKEN_IDENTIFIER:
          assemble_error("Unrecognized command %.*s\n", assembler.tok_identifier.len, assembler.tok_identifier.str);

        default:
          assemble_error("Expected command, but got %s\n", token_name());
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
    Symbol *s;
    array_foreach(assembler.symbols, s)
      if (s->address < 0)
        assemble_error("%.*s not defined\n", s->name.len, s->name.str);
  }
  {
    FILE *f;
    f = fopen("output.zbin", "wb");
    if (!f)
      assemble_error("Failed to open output file %s: %s\n", "output.zbin", strerror(errno));
    if (fwrite(assembler.program, array_len(assembler.program), 1, f) != 1)
      assemble_error("Failed to write to output: %s\n", strerror(errno));
    fclose(f);
  }
}

static void assembler_init(const char *filename) {
  int i;

  filecache_init(&assembler.file_cache);
  assembler.file = *file_get(&assembler.file_cache, filename);
  assembler.tok_cursor = assembler.tok_start = assembler.file.data;

  token_names[TOKEN_NULL]       = "<unknown>";
  token_names[TOKEN_MV]         = "mv";
  token_names[TOKEN_SUBI]       = "subi";
  token_names[TOKEN_SUBF]       = "subf";
  token_names[TOKEN_PUSH]       = "push";
  token_names[TOKEN_CALL]       = "call";
  token_names[TOKEN_ECALL]      = "ecall";
  token_names[TOKEN_IDENTIFIER] = "<identifier>";
  token_names[TOKEN_INT]        = "<int>";
  token_names[TOKEN_FLOAT]      = "<float>";
  token_names[TOKEN_FN]         = "fn";
  token_names[TOKEN_EOL]        = "\n";
  token_names[TOKEN_ARG]        = "param";
  token_names[TOKEN_LBRACE]     = "{";
  token_names[TOKEN_RBRACE]     = "}";
  token_names[TOKEN_EOF]        = "<eof>";
  token_names[TOKEN_OUT]        = "out";
  token_names[TOKEN_VAR]        = "var";
  token_names[TOKEN_I64]        = "i64";
  token_names[TOKEN_F64]        = "f64";
  for (i = 0; i < TOKEN_COUNT; ++i)
    if (!token_names[i])
      assemble_error("Token name %i not defined\n", i);
}

int main(int argc, const char **argv) {
  --argc, ++argv;

  if (argc < 1)
    fprintf(stderr, "Usage: zasm FILE\n"), exit(1);

  assembler_init(argv[0]);

  assemble();

  return 0;
}