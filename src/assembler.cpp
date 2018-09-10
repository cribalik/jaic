#include "common.hpp"
#include "array.hpp"
#include "mem.hpp"
#include <ctype.h>

enum Token {
  TOKEN_NULL = 0,

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

  TOKEN_COUNT
};

const char *token_names[TOKEN_COUNT];

struct Symbol {
  String name;
  int address;
};

struct Variable {
  Token vartype;
  DataType type;
  String name;
};

struct UnresolvedSymbol {
  int id;
  int location;
};

struct AssemblerState {
  FileCache file_cache;
  File file;

  Array<UnresolvedSymbol> unresolved;
  Array<Symbol> symbols;
  Array<u8> program;

  StackAllocator<4096> identifier_mem;

  Array<Variable> variables;

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
  char *s = assembler.identifier_mem.push_array<char>(assembler.tok_identifier.len+1);
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
  return streq(assembler.tok_identifier, str);
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
    else if (token_identifier_eq("var"))
      assembler.token = TOKEN_VAR;
    else if (token_identifier_eq("param"))
      assembler.token = TOKEN_ARG;
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
  return assembler.program.size;
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
  /* check if we have already encountered the symbol */
  Symbol *sp = find_symbol(assembler.tok_identifier);
  if (sp) {
    /* resolve the places we used it */
    if (sp->address >= 0)
      assemble_error("%.*s defined multiple times\n", assembler.tok_identifier.len, assembler.tok_identifier.str);
    int id = sp->address;
    sp->address = address();

    for (int i = 0; i < assembler.unresolved.size; ++i) {
      if (assembler.unresolved[i].id != id)
        continue;
      ((Word*)&assembler.program[assembler.unresolved[i].location])->int64 = sp->address;
      array_remove(assembler.unresolved, i);
      --i;
    }
    printf("Symbol %.*s found at offset %i \n", sp->name.len, sp->name.str, (int)sp->address);
    return;
  }

  array_push(assembler.symbols, Symbol{assembler.tok_identifier, address()});
  printf("Symbol %.*s found at offset %i \n", assembler.tok_identifier.len, assembler.tok_identifier.str, address());
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

static DataType get_type() {
  if (assembler.token != TOKEN_IDENTIFIER)
    assemble_error("Expected type, but got %s\n", token_name());

  for (int i = 1; i < ARRAY_LEN(datatype_names); ++i)
    if (streq(assembler.tok_identifier, datatype_names[i]))
      return (DataType)i;

  assemble_error("Unknown type %.*s\n", assembler.tok_identifier.len, assembler.tok_identifier.str);
  return DATATYPE_NULL;
}

static VMFun get_vmfun() {
  if (assembler.token != TOKEN_IDENTIFIER)
    assemble_error("Expected vm function, but got %s\n", token_name());

  for (int i = 1; i < ARRAY_LEN(vmfun_names); ++i)
    if (streq(assembler.tok_identifier, vmfun_names[i]))
      return (VMFun)i;

  assemble_error("Uknown host function %.*s\n", assembler.tok_identifier.len, assembler.tok_identifier.str);
  return VMFUN_NULL;
}

static void add_variable() {
  Variable var;

  var.vartype = assembler.token;

  /* type */
  token_next();
  var.type = get_type();
  if (var.type == DATATYPE_STACK)
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
  array_push(assembler.program, (Instruction_t)instr);
  token_next();
}

#define add_val(type, val) (((Word*)array_pushn(assembler.program, (int)sizeof(Word)))->type = val)

static int get_variable_offset(String name) {
  Variable *var;
  int offset;

  var = find_variable(name);
  if (!var)
    assemble_error("Unknown variable %.*s\n", name.len, name.str);

  offset = assembler.variables.size - (var - assembler.variables.items);
  if (var->vartype != TOKEN_VAR)
    ++offset;

  return offset;
}

static void add_symbol() {
  if (assembler.token != TOKEN_IDENTIFIER)
    assemble_error("Expected identifier for symbol, but got %s\n", token_name());

  Symbol *s = find_symbol(assembler.tok_identifier);

  /* if symbol doesn't exist, create a new symbol with negative address */
  if (!s) {
    static int invalid_address;
    Symbol new_sym;
    printf("Creating temporary symbol %.*s\n", assembler.tok_identifier.len, assembler.tok_identifier.str);

    new_sym.name = assembler.tok_identifier;
    new_sym.address = --invalid_address;

    array_push(assembler.symbols, new_sym);
    s = array_last(assembler.symbols);
  }

  // remember this location so we can resolve it later
  if (s->address < 0)
    array_push(assembler.unresolved, UnresolvedSymbol{s->address, address()});

  add_val(int64, s->address);
  token_next();
}

static Instruction get_instruction() {
  if (assembler.token != TOKEN_IDENTIFIER)
    assemble_error("Expected instruction, but got %s\n", token_name());
  for (int i = 0; i < ARRAY_LEN(instruction_names); ++i)
    if (streq(assembler.tok_identifier, instruction_names[i]))
      return (Instruction)i;
  return INSTR_NULL;
}

static void add_location(bool dest) {
  bool address_of = false;
  // if (assembler.token == TOKEN_AMPERSAND) {
  //   address_of = true;
  //   token_next();
  // }

  if (assembler.token == TOKEN_IDENTIFIER) {
    array_push(assembler.program, (DataType_t)DATATYPE_STACK);
    add_val(int64, get_variable_offset(assembler.tok_identifier));
    goto done;
  }
  if (dest)
    assemble_error("Only integers can be used as destination\n");
  if (address_of)
    assemble_error("Expected identifier after ampersand, but got %s", token_name);

  if (assembler.token == TOKEN_INT) {
    array_push(assembler.program, (DataType_t)DATATYPE_LITERAL_I64);
    add_val(int64, assembler.tok_integer);
  }
  else if (assembler.token == TOKEN_FLOAT) {
    array_push(assembler.program, (DataType_t)DATATYPE_LITERAL_F64);
    add_val(f64, assembler.tok_float);
  }

  done:
  token_next();
}

static void assemble() {
  token_next();

  while (1) {
    switch (assembler.token) {
    case TOKEN_EOF:
      goto assemble_done;

    case TOKEN_FN: {
      bool is_main = false;
      int num_variables = 0;

      token_next();
      if (assembler.token != TOKEN_IDENTIFIER)
        assemble_error("Expected function name after fn, but got %s\n", token_name());

      /* main? */
      if (streq(assembler.tok_identifier, "Main")) {
        if (((Word*)assembler.program.items)->uint64)
          assemble_error("Multiple declarations of Main\n");
        ((Word*)assembler.program.items)->uint64 = assembler.program.size;
        is_main = true;
      }

      save_symbol();

      token_next();

      newline();

      if (assembler.token != TOKEN_LBRACE)
        assemble_error("Expected '{' after function declaration, but got %s\n", token_name());
      token_next();
      newline();

      assembler.variables.size = 0;
      while (assembler.token == TOKEN_OUT)
        add_variable();
      while (assembler.token == TOKEN_ARG)
        add_variable();
      while (assembler.token == TOKEN_VAR)
        add_variable(), ++num_variables;

      if (num_variables) {
        array_push(assembler.program, (Instruction_t)INSTR_PUSHN);
        array_push(assembler.program, (DataType_t)DATATYPE_LITERAL_I64);
        add_val(uint64, (u64)num_variables);
      }

      while (1) {
        switch (assembler.token) {

          case TOKEN_IDENTIFIER: {
            switch (get_instruction()) {
              case INSTR_NUM:
              case INSTR_NULL:
                assemble_error("Unrecognized command %.*s\n", assembler.tok_identifier.len, assembler.tok_identifier.str);

              case INSTR_MV:
                add_instruction(INSTR_MV);
                add_location(true);
                add_location(false);
                break;

              case INSTR_SUBI:
                add_instruction(INSTR_SUBI);
                add_location(true);
                add_location(false);
                break;

              case INSTR_SUBF:
                add_instruction(INSTR_SUBF);
                add_location(true);
                add_location(false);
                break;

              case INSTR_ADDI:
                add_instruction(INSTR_ADDI);
                add_location(true);
                add_location(false);
                break;

              case INSTR_ADDF:
                add_instruction(INSTR_ADDF);
                add_location(true);
                add_location(false);
                break;

              case INSTR_MULI:
                add_instruction(INSTR_MULI);
                add_location(true);
                add_location(false);
                break;

              case INSTR_MULF:
                add_instruction(INSTR_MULF);
                add_location(true);
                add_location(false);
                break;

              case INSTR_DIVI:
                add_instruction(INSTR_DIVI);
                add_location(true);
                add_location(false);
                break;

              case INSTR_DIVF:
                add_instruction(INSTR_DIVF);
                add_location(true);
                add_location(false);
                break;

              case INSTR_PUSH:
                add_instruction(INSTR_PUSH);
                add_location(false);
                break;

              case INSTR_PUSHN:
                add_instruction(INSTR_PUSHN);
                add_location(false);
                break;

              case INSTR_POP:
                add_instruction(INSTR_POP);
                add_location(false);
                break;

              case INSTR_CALL:
                add_instruction(INSTR_CALL);
                add_symbol();
                break;

              case INSTR_ECALL: {
                VMFun f;

                add_instruction(INSTR_ECALL);
                f = get_vmfun();
                add_val(uint64, f);
                token_next();
                break;
              }

              case INSTR_RET:
                add_instruction(INSTR_RET);
                add_val(uint64, num_variables);
                break;

              case INSTR_EXIT:
                add_instruction(INSTR_EXIT);
                break;

            }
            break;
          }

          case TOKEN_RBRACE:
            if (is_main) {
              add_instruction(INSTR_EXIT);
            }
            else {
              add_instruction(INSTR_RET);
              add_val(uint64, num_variables);
              printf("adding return instruction %i\n", num_variables);
            }
            goto fn_done;

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

  // check if all symbols are defined
  for (Symbol &s : assembler.symbols)
    if (s.address < 0)
      assemble_error("%.*s not defined\n", s.name.len, s.name.str);

  if (!file_write("output.zbin", assembler.program.items, assembler.program.size))
    assemble_error("Failed to write to output.zbin: %s", get_error());
}

static void assembler_init(const char *filename) {
  filecache_init(&assembler.file_cache);
  assembler.file = *file_get(&assembler.file_cache, filename);
  assembler.tok_cursor = assembler.tok_start = assembler.file.data;

  token_names[TOKEN_NULL]       = "<unknown>";
  token_names[TOKEN_IDENTIFIER] = "<identifier>";
  token_names[TOKEN_INT]        = "<int>";
  token_names[TOKEN_FLOAT]      = "<float>";
  token_names[TOKEN_FN]         = "fn";
  token_names[TOKEN_EOL]        = "<eol>";
  token_names[TOKEN_ARG]        = "param";
  token_names[TOKEN_LBRACE]     = "{";
  token_names[TOKEN_RBRACE]     = "}";
  token_names[TOKEN_EOF]        = "<eof>";
  token_names[TOKEN_OUT]        = "out";
  token_names[TOKEN_VAR]        = "var";
  for (int i = 0; i < TOKEN_COUNT; ++i)
    if (!token_names[i])
      assemble_error("Token name %i not defined\n", i);

  add_val(uint64, 0);
}

int main(int argc, const char **argv) {
  --argc, ++argv;

  if (argc < 1)
    fprintf(stderr, "Usage: zasm FILE\n"), exit(1);

  assembler_init(argv[0]);

  assemble();

  return 0;
}