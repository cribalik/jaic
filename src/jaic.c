#define DEBUG 1

#include <stdio.h>
#include "common.c"
internal void print(FILE* file, char* fmt, ...);
internal void vprint(FILE* file, char* fmt, va_list args);
#include "terminal.c"
#include "memarena.c"
#include "array.c"
#include "utils.c"
#include "string.c"
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <stdint.h>

#ifdef LINUX
  #include <unistd.h>
#endif

/* Logging */

void printLine(FILE* out, char* filename, int line, int column) {
  FILE* file;
  int i = 1;
  char c;

  /* TODO: send in the file position instead so we don't need to parse the whole file? it's okay if this is slow though */
  if (!filename) {
    return;
  }

  file = fopen(filename, "r");
  for (i = 1; i < line;) {
    i += getc(file) == '\n';
  }

  fflush(out);
  c = getc(file);
  while (c != EOF && c != '\n') {
    putc(c, out);
    c = getc(file);
  }
  putc('\n', out);
  for (i = 0; i < column-1; ++i) {
    putc(' ', out);
  }
  putc('^', out);
  putc('\n', out);
  fflush(out);
}

/* Some macros */

#if defined(_MSC_VER)
  extern void __cdecl __debugbreak(void);
  #define trigger_breakpoint() __debugbreak()
#else
  #include <signal.h>
  #define trigger_breakpoint() raise(SIGTRAP)
#endif

#define UNIMPLEMENTED do {fprintf(stderr, "Unimplemented function at %s:%d\n", __FILE__, __LINE__); trigger_breakpoint(); exit(1);} while(0)
#define UNREACHABLE do {fprintf(stderr, "Unreachable\n"); trigger_breakpoint(); exit(1);} while(0)
#define LOG_AND_DIE(msg) do {fputs(msg, stderr); trigger_breakpoint(); exit(1);} while(0)

global bool found_error = false;

/* Example: 
 * jai_log_at(ERROR, "value was %i", 3);
 * jai_log_at(DEBUG_INFO, "value was %i, that makes me %s", 5, happy ? "happy" : "sad");
 */
#define ERROR _LOG_USER_ERROR,__FILE__,__LINE__
#define NOTE _LOG_USER_INFO,__FILE__,__LINE__
#define DEBUG_INFO _LOG_DEBUG_INFO,__FILE__,__LINE__
#define DEBUG_ERROR _LOG_DEBUG_ERROR,__FILE__,__LINE__
typedef enum {
  _LOG_USER_ERROR,
  _LOG_USER_INFO,
  _LOG_DEBUG_INFO,
  _LOG_DEBUG_ERROR
} _LogType;
internal void jai_log_at(_LogType type, char* file, int line, FilePos filepos, char* fmt, ...) {
  va_list args;
  va_start(args, fmt);
  switch (type) {
    case _LOG_USER_ERROR: {
      fprintf(stderr, "%s%s:%i:%i (%s:%i): %serror:%s%s ", BOLD, filepos.file, filepos.line, filepos.column, file, line, RED, RESET_COLOR, RESET_FORMAT);
      vprint(stderr, fmt, args);
      printLine(stderr, filepos.file, filepos.line, filepos.column);
      found_error = true;
    } break;
    case _LOG_USER_INFO: {
      fprintf(stderr, "%s%s:%i:%i (%s:%i): %snote:%s%s ", BOLD, filepos.file, filepos.line, filepos.column, file, line, GREEN, RESET_COLOR, RESET_FORMAT);
      vprint(stderr, fmt, args);
      printLine(stderr, filepos.file, filepos.line, filepos.column);
    } break;
    case _LOG_DEBUG_INFO: {
      if (DEBUG) {
        fprintf(stderr, "%s%s:%i:%i (%s:%i): %sdebug:%s%s ", BOLD, filepos.file, filepos.line, filepos.column, file, line, GREEN, RESET_COLOR, RESET_FORMAT);
        printLine(stderr, filepos.file, filepos.line, filepos.column);
        vprint(stderr, fmt, args);
      }
    } break;
    case _LOG_DEBUG_ERROR: {
      if (DEBUG) {
        fprintf(stderr, "%s%s:%i:%i (%s:%i): %serror:%s%s ", BOLD, filepos.file, filepos.line, filepos.column, file, line, RED, RESET_COLOR, RESET_FORMAT);
        vprint(stderr, fmt, args);
        printLine(stderr, filepos.file, filepos.line, filepos.column);
      }
    } break;
  }
  va_end(args);
}
internal void jai_log(_LogType type, char* file, int line, char* fmt, ...) {
  va_list args;
  va_start(args, fmt);
  switch (type) {
    case _LOG_DEBUG_INFO: {
      if (DEBUG) {
        fprintf(stderr, "%s%s:%i: %sdebug:%s%s ", BOLD, file, line, GREEN, RESET_COLOR, RESET_FORMAT);
        vprint(stderr, fmt, args);
      }
    } break;
    case _LOG_DEBUG_ERROR: {
      if (DEBUG) {
        fprintf(stderr, "%s%s:%i: %serror:%s%s ", BOLD, file, line, RED, RESET_COLOR, RESET_FORMAT);
        vprint(stderr, fmt, args);
      }
    } break;
    case _LOG_USER_INFO: {
      fprintf(stderr, "%s%snote:%s%s ", BOLD, GREEN, RESET_COLOR, RESET_FORMAT);
      vprint(stderr, fmt, args);
    } break;
    case _LOG_USER_ERROR: {
      fprintf(stderr, "%s%serror:%s%s ", BOLD, RED, RESET_COLOR, RESET_FORMAT);
      vprint(stderr, fmt, args);
    } break;
    default: UNREACHABLE;
  }
  va_end(args);
}

typedef enum {
  TOK_EOF = -1,
  TOK_IDENTIFIER = -3,
  TOK_FLOAT = -4,
  TOK_INT = -5,
  TOK_UNKNOWN = -6,
  TOK_ARROW = -7,
  TOK_STRUCT = -8,
  TOK_LOOP = -9,
  TOK_DOUBLEDOT = -10,
  TOK_RETURN = -11,
  TOK_FN = -12,
  TOK_VAR = -13
} Token;

/* values set by the tokenizer */
global char* identifierStr;
global double floatVal;
global int intVal;

/* the input source file */
global FILE* file;

#define MAIN_MEMORY_SIZE 64*1024*1024

global MemArena perm_arena;

#define pushPosition \
  FilePos _position = prev_pos; \
  fgetpos(file, &_position.fpos); \
  int _num_chars_last_read = num_chars_last_read; \
  jai_log(DEBUG_INFO, "Pushing %s:%i:%i %c\n", _position.file, _position.line, _position.column-1, curr_char);

#define popPosition \
  jai_log(DEBUG_INFO, "Jumping back to %s:%i:%i %c\n", prev_pos.file, prev_pos.line, prev_pos.column-1, curr_char); \
  fsetpos(file, &_position.fpos); \
  fseek(file, -_num_chars_last_read - 1, SEEK_CUR); \
  next_pos = _position; \
  curr_char = ' '; \
  eatToken();

#define pushState \
  void* _arena_current = arena_pos(perm_arena);
#define popState \
  arena_pop_to(&perm_arena, _arena_current);

global FilePos prev_pos;
global FilePos next_pos;

/** Tokenizer stuff **/

global int num_chars_read = 0;
internal char getChar(FILE* file) {
  char c = getc(file);
  /* jai_log(DEBUG_INFO, "%i: %c\n", num_chars_read, c); */
  ++num_chars_read;
  ++next_pos.column;
  if (c == '\n') {
    ++next_pos.line;
    next_pos.column = 0;
  }
  return c;
}

global int curr_char = ' ';
internal void eatWhitespace() {
  while (isspace(curr_char)) curr_char = getChar(file);
}
internal int gettok() {
  #define BUFFER_SIZE 512
  static char buf[BUFFER_SIZE];

  while (isspace(curr_char)) {
    curr_char = getChar(file);
  }

  /* check for comments */
  while (curr_char == '/') {
    curr_char = getChar(file);
    if (curr_char == '/') {
      do {
        curr_char = getChar(file);
      } while (curr_char != EOF && curr_char != '\n' && curr_char != '\r');
      if (curr_char == EOF) {
        return TOK_EOF;
      }
      goto endofcomment;
    }
    else if (curr_char == '*') {
      int depth = 1;
      curr_char = getChar(file);
      while (1) {
        if (curr_char == '*') {
          curr_char = getChar(file);
          if (curr_char == '/') {
            --depth;
            if (depth == 0) {
              curr_char = getChar(file);
              goto endofcomment;
            }
          }
        }
        else if (curr_char == '/') {
          curr_char = getChar(file);
          if (curr_char == '*') {
            ++depth;
          }
        } else if (curr_char == EOF) {
          printf("File ended while still in block comment\n");
          return TOK_UNKNOWN;
        }
        curr_char = getChar(file);
      }
    }
    else {
      return '/';
    }
    endofcomment:
    while (isspace(curr_char)) {curr_char = getChar(file);};
  }

  prev_pos = next_pos;

  /* identifier */
  if (isalpha(curr_char)) {
    int i = 0;
    for (i = 0; i < BUFFER_SIZE && (isalpha(curr_char) || isdigit(curr_char) || curr_char=='_'); ++i) {
      buf[i] = curr_char;
      curr_char = getChar(file);
    }
    if (i == BUFFER_SIZE) {
      printf("Reached identifier size limit\n");
      return TOK_UNKNOWN;
    }
    else {
      buf[i] = '\0';
      identifierStr = buf;
      if (strcmp(identifierStr, "type") == 0) {
        return TOK_STRUCT;
      } else if (strcmp(identifierStr, "loop") == 0) {
        return TOK_LOOP;
      } else if (strcmp(identifierStr, "for") == 0) {
        return TOK_LOOP;
      } else if (strcmp(identifierStr, "return") == 0) {
        return TOK_RETURN;
      } else if (strcmp(identifierStr, "fn") == 0) {
        return TOK_FN;
      } else if (strcmp(identifierStr, "var") == 0) {
        return TOK_VAR;
      } else {
        return TOK_IDENTIFIER;
      }
    }
  }

  /* number literal */
  else if (isdigit(curr_char)) {
    int i;
    for (i = 0; i < BUFFER_SIZE && isdigit(curr_char); ++i) {
      buf[i] = curr_char;
      curr_char = getChar(file);
    }

    if (i == BUFFER_SIZE) {
      jai_log_at(ERROR, prev_pos, "Reached max size for int\n");
    }
    else if (curr_char == '.') {
      buf[i++] = '.';
      curr_char = getChar(file);
      if (isdigit(curr_char)) {
        for (; i < BUFFER_SIZE && isdigit(curr_char); ++i) {
          buf[i] = curr_char;
          curr_char = getChar(file);
        }

        if (i == BUFFER_SIZE) {
          jai_log_at(ERROR, prev_pos, "Reached max size for float number\n");
        }
        /* TODO: check for suffixes here e.g. 30.0f */
        else {
          buf[i] = '\0';
          floatVal = strtod(buf, NULL);
          return TOK_FLOAT;
        }
      } else {
        jai_log_at(ERROR, prev_pos, "Unexpected character '%c', expected floating point number or '..'\n", curr_char);
      }
    }
    /* TODO: check for suffixes here e.g. 10u32 */
    else {
      buf[i] = '\0';
      intVal = atoi(buf);
      return TOK_INT;
    }
  }

  /* eof */
  else if (curr_char == EOF) {
    return TOK_EOF;
  }

  /* arrow */
  else if (curr_char == '-') {
    curr_char = getChar(file);
    if (curr_char == '>') {
      curr_char = getChar(file);
      return TOK_ARROW;
    } else {
      return '-';
    }
  }

  /* double dots */
  else if (curr_char == '.') {
    curr_char = getChar(file);
    if (curr_char == '.') {
      curr_char = getChar(file);
      return TOK_DOUBLEDOT;
    } else {
      return '.';
    }
  }

  else {
    int r = curr_char;
    curr_char = getChar(file);
    return r;
  }

  UNREACHABLE;
  #undef BUFFER_SIZE
}

global int token;
internal char* print_token() {
  static char buffer[2];
  switch (token) {
    case TOK_FLOAT:
      return "<float>";
    case TOK_INT:
      return "<int>";
    case TOK_UNKNOWN:
      return "<unknown>";
    case TOK_IDENTIFIER:
      return identifierStr;
    case TOK_EOF:
      return "<eof>";
    case TOK_ARROW:
      return "->";
    default:
      buffer[0] = token;
      return buffer;
  }
  UNREACHABLE;
}

global int num_chars_last_read = 0;
internal void eatToken() {
  /* jai_log(DEBUG_INFO, "before gettok: line: %i column: %i\n", prev_pos.line, prev_pos.column); */
  int i = num_chars_read;
  token = gettok();
  eatWhitespace();
  num_chars_last_read = num_chars_read - i;
  /* jai_log(DEBUG_INFO, "last read: %i token: %s\n", num_chars_last_read, print_token()); */
}

internal void goto_matching_brace() {
  FilePos pos = prev_pos;
  int d = 0;
  while (d) {
    eatToken();
    if (token == '{') ++d;
    else if (token == '}') --d;
    else if (token == EOF) {
      jai_log_at(ERROR, pos, "No matching brace found\n");
      return;
    }
  }
  eatToken();
}

/** AST Definitions **/

struct ExpressionAST;

/* Statements */

typedef enum StatementType {
  UNKNOWN_STMT,
  STRUCT_DECLARATION_STMT,
  VARIABLE_DECLARATION_STMT,
  FUNCTION_DECLARATION_STMT,
  ASSIGNMENT_STMT,
  EXPRESSION_STMT,
  COMPOUND_STMT, /* new scope */
  LOOP_STMT,
  RETURN_STMT
} StatementType;

typedef struct StatementAST {
  StatementType type;
  FilePos pos;
} StatementAST;

typedef struct CompoundStatementAST {
  StatementAST stmt;
  struct CompoundStatementAST* parent_scope;
  int num_statements;
  StatementAST** statements;
  /* TODO: optimize variable lookups */
} CompoundStatementAST;

typedef struct ReturnAST {
  StatementAST stmt;
  struct ExpressionAST* value;
} ReturnAST;

/* TypeClass */

typedef enum TypeClass {
  UNKNOWN_TYPE,
  /* builtin types */
  VOID_TYPE,
  INT_TYPE,
  FLOAT_TYPE,

  F32_TYPE,
  F64_TYPE,

  U8_TYPE,
  U16_TYPE,
  U32_TYPE,
  U64_TYPE,

  I8_TYPE,
  I16_TYPE,
  I32_TYPE,
  I64_TYPE,

  USIZE_TYPE, /* size_t */

  STRUCT_TYPE,

  ARRAY_TYPE,
  STATIC_ARRAY_TYPE,

  POINTER_TYPE,

  FUNCTION_TYPE
} TypeClass;

typedef struct Type {
  TypeClass type;
} Type;

/* Type syntax tree (this does not represent the actual type, but rather how the parser sees a type) */

typedef enum TypeASTClass {
  ARRAY_TYPE_AST,
  STATIC_ARRAY_TYPE_AST,
  POINTER_TYPE_AST
} TypeASTClass;
typedef struct PartialTypeAST {
  TypeASTClass class;
  union {
    struct ExpressionAST* size; /* if static size array */
  } static_array;
} PartialTypeAST;
typedef struct TypeAST {
  int num_partial_types;
  PartialTypeAST* partial_types;
  char* name;
} TypeAST;

global TypeAST void_type_ast;

global char* builtin_typenames[] = {
  "",
  "void",
  "int",
  "float",
  "f32",
  "f64",
  "u8",
  "u16",
  "u32",
  "u64",
  "i8",
  "i16",
  "i32",
  "i64",
  "usize",
};

global Type builtin_types[19];

/* some dummy types, like telling evaluateTypeOfExpression that we want an array, but don't know the type of the elements */
global Type array_no_internal_type;
global DynArray /* Type* */ generated_types; /* types that are generated as we go, like pointer and array types */

typedef struct MemberAST {
  char* name;
  TypeAST type_ast;
  Type* type;
  FilePos pos;
} MemberAST;
typedef struct StructType {
  Type type;
  char* name;
  int num_members;
  MemberAST* members;
  bool has_initializer;
} StructType;

typedef struct ArrayType {
  Type type;
  Type* base_type;
} ArrayType;

typedef struct StaticArrayType {
  ArrayType arr;
  int size;
} StaticArrayType;

typedef struct PointerType {
  Type type;
  Type* base_type;
} PointerType;

/* Expressions */

typedef enum ExpressionType {
  UNKNOWN_EXPR,
  LITERAL_EXPR,
  CALL_EXPR,
  VARIABLE_GET_EXPR,
  MEMBER_ACCESS_EXPR,
  STRUCT_INIT_EXPR,
  BINOP_EXPR,
  ARRAY_SUBSCRIPT_EXPR
} ExpressionType;

typedef struct ExpressionAST {
  StatementAST stmt;
  ExpressionType expr_type;
  Type* type;
} ExpressionAST;


typedef struct Constant {
  ExpressionAST expr;
  union {
    float float_val;
    int32_t int_val;
    float f32;
    double f64;
    uint8_t u8;
    uint16_t u16;
    uint32_t u32;
    uint64_t u64;
    int8_t i8;
    int16_t i16;
    int32_t i32;
    int64_t i64;
  } value;
} Constant;

typedef struct MemberAccessAST {
  ExpressionAST expr;
  ExpressionAST* base; /* in  a.b.x, this is a.b */
  char* name; /* in a.b.x, this is x */
} MemberAccessAST;

typedef struct StructDeclarationAST {
  StatementAST stmt;
  StructType str;
} StructDeclarationAST;

typedef struct MemberInitializationAST {
  char* name;
  ExpressionAST* value;
  MemberAST* member;
} MemberInitializationAST;
typedef struct StructInitializationAST {
  ExpressionAST expr;
  int num_members;
  MemberInitializationAST* members;
} StructInitializationAST;

typedef struct VariableGetAST {
  ExpressionAST expr;
  char* name;
} VariableGetAST;

struct FunctionDeclarationAST;
typedef struct CallAST {
  ExpressionAST expr;
  ExpressionAST* base;
  int num_args;
  struct FunctionDeclarationAST* fun;
  ExpressionAST** args;
} CallAST;

typedef struct ArraySubscriptAST {
  ExpressionAST expr;
  ExpressionAST* base;
  ExpressionAST* subscript;
} ArraySubscriptAST;

typedef struct BinaryExpressionAST {
  ExpressionAST expr;
  char operator;
  ExpressionAST* lhs;
  ExpressionAST* rhs;
} BinaryExpressionAST;

typedef struct VariableDeclarationAST {
  StatementAST stmt;
  char* name;
  TypeAST type_ast;
  Type* type;
  ExpressionAST* value;
} VariableDeclarationAST;

typedef struct LoopAST {
  CompoundStatementAST body;
  VariableDeclarationAST iter;
  VariableDeclarationAST index;
  ExpressionAST* from;
  ExpressionAST* to;
} LoopAST;

typedef VariableDeclarationAST ArgumentAST;
typedef struct FunctionDeclarationAST {
  CompoundStatementAST body;
  int id;
  char* name;
  int num_args;
  ArgumentAST* args;
  TypeAST return_type_ast;
  Type* return_type;
  FilePos pos;
  bool is_foreign;
} FunctionDeclarationAST;

typedef struct AssignmentAST {
  StatementAST stmt;
  ExpressionAST* lhs;
  ExpressionAST* rhs;
} AssignmentAST;

internal char* print_type_ast_name(TypeAST type) {
  String res;
  int i;
  if (!type.name) return 0;
  res = string_create(0);
  for (i = 0; i < type.num_partial_types; ++i) {
    switch (type.partial_types[i].class) {
      case POINTER_TYPE_AST: string_append_char(&res, '^'); break;
      case ARRAY_TYPE_AST: string_append(&res, "[]"); break;
      case STATIC_ARRAY_TYPE_AST: string_append(&res, "[N]"); break;
    }
  }
  string_append(&res, type.name);
  return string_get(&res);
}

internal int test() {
  arrayTest();
  return 0;
}

typedef enum IdentifierType {
  IdentifierType_UNKNOWN = 0,
  IdentifierType_FUNCTION,
  IdentifierType_VARIABLE,
  IdentifierType_TYPE
} IdentifierType;


/** 1st pass: Parsing **/

internal StatementAST* parseStatement();
internal bool parseType(TypeAST*);

internal int parse_block(CompoundStatementAST* result, DynArray* prefix) {
  DynArray statements;
  DynArray* array;
  if (token != '{') {
    return 1;
  }
  eatToken();
  if (prefix) {
    array = prefix;
  } else {
    array = &statements;
    *array = array_create(32, sizeof(StatementAST*));
  }
  while (1) {
    StatementAST* stmt;
    if (token == '}') {
      eatToken();
      break;
    }
    stmt = parseStatement();
    if (stmt) {
      array_pushVal(array, &stmt);
    } else {
      jai_log_at(ERROR, prev_pos, "Failed to parse statement\n");
      arrayFree(array);
      return 1;
    }
  }
  result->num_statements = arrayCount(array);
  result->statements = arena_push_array(array, &perm_arena);
  result->stmt.type = COMPOUND_STMT;
  arrayFree(array);
  return 0;
}

internal ExpressionAST* parseExpression();
internal ExpressionAST* parsePrimitive() {
  switch (token) {
    case TOK_FLOAT: {
      Constant* f = arena_push(&perm_arena, sizeof(Constant));
      f->expr.expr_type = LITERAL_EXPR;
      f->expr.stmt.pos = prev_pos;
      f->value.float_val = floatVal;
      f->expr.type = &builtin_types[FLOAT_TYPE];
      eatToken();
      jai_log(DEBUG_INFO, "Found a float literal with value %f\n", f->value.f32);
      return &f->expr;
    } break;

    case TOK_INT: {
      Constant* i = arena_push(&perm_arena, sizeof(Constant));
      i->expr.expr_type = LITERAL_EXPR;
      i->expr.stmt.pos = prev_pos;
      i->value.int_val = intVal;
      i->expr.type = &builtin_types[INT_TYPE];
      eatToken();
      jai_log(DEBUG_INFO, "Found an int literal with value %i\n", i->value.int_val);
      return &i->expr;
    } break;

    case TOK_IDENTIFIER: {
      VariableGetAST* var;
      ExpressionAST* result;

      var = arena_push(&perm_arena, sizeof(VariableGetAST));
      var->expr.stmt.pos = prev_pos;
      var->expr.expr_type = VARIABLE_GET_EXPR;
      var->name = arena_push_string(&perm_arena, identifierStr);
      result = &var->expr;
      eatToken();

      while (1) {

        /* function call? */
        if (token == '(') {
          CallAST* call;
          DynArray args;
          call = arena_push(&perm_arena, sizeof(CallAST));
          call->expr.expr_type = CALL_EXPR;
          call->expr.stmt.pos = prev_pos;
          call->base = result;
          args = array_create(0, sizeof(ExpressionAST*));
          eatToken();

          /* get args */
          while (1) {
            ExpressionAST* expr = 0;
            if (token == ')') {
              eatToken();
              break;
            }

            expr = parseExpression();
            if (!expr) {
              jai_log_at(ERROR, prev_pos, "Invalid expression inside function call\n");
              arrayFree(&args);
              return 0;
            }

            array_pushVal(&args, &expr);

            if (token == ')') {
              eatToken();
              break;
            }
            if (token == ',') {
              eatToken();
            } else {
              jai_log_at(ERROR, prev_pos, "Expected ',' between function input parameters\n");
            }
          }

          call->num_args = arrayCount(&args);
          if (call->num_args > 0) {
            call->args = arena_push_array(&args, &perm_arena);
          }
          arrayFree(&args);

          jai_log(DEBUG_INFO, "Found function call with %i inputs\n", call->num_args);
          result = &call->expr;
        }
        /* array element? */
        else if (token == '[') {
          ArraySubscriptAST* as;
          as = arena_push(&perm_arena, sizeof(ArraySubscriptAST));
          as->expr.expr_type = ARRAY_SUBSCRIPT_EXPR;
          as->expr.stmt.pos = prev_pos;
          as->base = result;
          eatToken();

          as->subscript = parseExpression();
          if (as->subscript) {
            if (token == ']') {
              eatToken();
              result = &as->expr;
            } else {
              jai_log_at(ERROR, prev_pos, "Found no matching ']'\n");
              return 0;
            }
          } else {
            jai_log_at(ERROR, prev_pos, "Failed to parse array subscript index\n");
            return 0;
          }
        }
        /* member access? */
        else if (token == '.') {
          eatToken();
          if (token == TOK_IDENTIFIER) {
            MemberAccessAST* ast = arena_push(&perm_arena, sizeof(MemberAccessAST));
            ast->expr.expr_type = MEMBER_ACCESS_EXPR;
            ast->expr.stmt.pos = prev_pos;
            ast->base = result;
            ast->name = arena_push_string(&perm_arena, identifierStr);
            result = &ast->expr;
            eatToken();
          } else {
            jai_log_at(ERROR, prev_pos, "Identifier expected after '.', instead got %s\n", print_token());
            return 0;
          }
        }
        else {
          break;
        }

      }
      return result;
    } break;

    case '(': {
      ExpressionAST* result;
      eatToken();
      result = parseExpression();
      if (!result || token != ')') {
        return 0;
      }
      eatToken();
      return result;
    } break;

    case '{': {
      StructInitializationAST* ast;
      eatToken();
      ast = arena_push(&perm_arena, sizeof(StructInitializationAST));
      ast->expr.expr_type = STRUCT_INIT_EXPR;
      ast->expr.stmt.pos = prev_pos;
      if (token != '}') {
        DynArray members = array_create(4, sizeof(MemberInitializationAST));
        while (1) {
          MemberInitializationAST* member = array_push(&members);
          bool success = false;
          if (token == TOK_IDENTIFIER) {
            member->name = arena_push_string(&perm_arena, identifierStr);
            eatToken();
            if (token == '=') {
              eatToken();
              member->value = parseExpression();
              if (member->value) {
                if (token == ',') {
                  eatToken();
                  success = true;
                } else if (token == '}') {
                  eatToken();
                  break;
                } else {
                  jai_log_at(ERROR, prev_pos, "Expected ',' or '}', but found %s\n", print_token());
                }
              } else {jai_log_at(ERROR, prev_pos, "Failed to parse expression\n");}
            } else {jai_log_at(ERROR, prev_pos, "Expected '=' after member name, found %s\n", print_token());}
          } else {jai_log_at(ERROR, prev_pos, "Expected identifier, instead found %s\n", print_token());}

          if (!success) {
            while (token != '}') {
              eatToken();
            }
            eatToken();
            ast = 0;
            break;
          }
          /*we failed, eat everything to matching brace */
        }
        if (ast) {
          ast->members = arena_push_array(&members, &perm_arena);
          ast->num_members = arrayCount(&members);
        }
        arrayFree(&members);
      } else {
        eatToken();
      }
      if (ast) {
        return &ast->expr;
      } else {
        return 0;
      }
    } break;

    default: {
      jai_log_at(ERROR, prev_pos, "Failed to parse expression\n");
      return 0;
    } break;
  }
}

internal int binopPrecedence(char op) {
  switch (op) {
    case '-': return 10;
    case '+': return 10;
    case '*': return 20;
    case '/': return 20;
    case '%': return 20;
    case '<': return 5;
    case '>': return 5;
    default: return 0;
  }
}

internal ExpressionAST* parseExpression() {
  ExpressionAST* expr = parsePrimitive();
  if (expr) {
    if (binopPrecedence(token)) {
      BinaryExpressionAST* bin = arena_push(&perm_arena, sizeof(BinaryExpressionAST));
      bin->expr.expr_type = BINOP_EXPR;
      bin->expr.stmt.pos = prev_pos;
      bin->operator = token;
      bin->lhs = expr;
      jai_log_at(DEBUG_INFO, prev_pos, "Found binary expression with operator %s\n", print_token());
      eatToken();
      bin->rhs = parseExpression();
      return &bin->expr;
      /* TODO: operator precedence */
    }
  }
  return expr;
}

internal bool parseType(TypeAST* result) {
  bool partial_success = false;

  DynArray partial_types = array_create(4, sizeof(PartialTypeAST));

  while (1) {
    if (token == '[') {
      partial_success = true;
      eatToken();
      if (token == ']') {
        PartialTypeAST* type = array_push(&partial_types);
        type->class = ARRAY_TYPE_AST;
        eatToken();
        continue;
      } else {
        ExpressionAST* expr = parseExpression();
        if (expr) {
          if (token == ']') {
            PartialTypeAST* type = array_push(&partial_types);
            type->class = STATIC_ARRAY_TYPE_AST;
            type->static_array.size = expr;
            eatToken();
            continue;
          } else {jai_log_at(ERROR, prev_pos, "Expected matching ']' in array type\n");}
        } else {jai_log_at(ERROR, prev_pos, "Expected ']' or constant after '[' (array type)\n");}
      }
    }
    else if (token == '^') {
      PartialTypeAST* type;
      partial_success = true;
      eatToken();
      type = array_push(&partial_types);
      type->class = POINTER_TYPE_AST;
      continue;
    } else {
      break;
    }

    arrayFree(&partial_types);
    return false;
  }

  if (token == TOK_IDENTIFIER) {
    result->name = arena_push_string(&perm_arena, identifierStr);
    eatToken();
  } else {
    if (partial_success) {
      jai_log_at(ERROR, prev_pos, "Expected type name\n");
    }
    return false;
  }

  if (arrayCount(&partial_types)) {
    result->num_partial_types = arrayCount(&partial_types);
    result->partial_types = arena_push_array(&partial_types, &perm_arena);
  }
  arrayFree(&partial_types);

  return true;
}

internal StatementAST* _parseStatement() {
  FilePos pos_of_identifier = prev_pos;

  if (token == TOK_LOOP) {
    LoopAST* loop = arena_push(&perm_arena, sizeof(LoopAST));
    loop->body.stmt.type = LOOP_STMT;
    loop->body.stmt.pos = prev_pos;
    /* TODO: size_t */
    loop->index.type = &builtin_types[INT_TYPE];
    eatToken();

    if (token == TOK_IDENTIFIER || token == '_') {
      if (token == TOK_IDENTIFIER) {
        loop->iter.name = arena_push_string(&perm_arena, identifierStr);
        loop->iter.stmt.pos = prev_pos;
      }
      eatToken();

      if (token == ',') {
        eatToken();
        if (token == TOK_IDENTIFIER) {
          loop->index.name = arena_push_string(&perm_arena, identifierStr);
          loop->index.stmt.pos = prev_pos;
          eatToken();
        } else {
          jai_log_at(ERROR, prev_pos, "Expected index name\n");
          return 0;
        }
      }

      if (token == ':') {
        ExpressionAST* from;
        eatToken();

        from = parseExpression();
        if (from) {
          loop->from = from;

          /* range loop? */
          if (token == TOK_ARROW) {
            eatToken();
            loop->to = parseExpression();
            if (!loop->to) {
              jai_log_at(ERROR, prev_pos, "Could not parse expression at end of loop\n");
              return 0;
            }
          }

          /* parse block */
          if (token == '{') {
            DynArray statements = array_create(16, sizeof(StatementAST*));
            eatToken();
            while (1) {
              StatementAST* stmt;
              if (token == '}') {
                eatToken();
                break;
              }
              stmt = parseStatement();
              if (stmt) {
                array_pushVal(&statements, &stmt);
              } else {
                jai_log_at(ERROR, prev_pos, "Failed to parse statement in loop body\n");
                break;
              }
            }
            loop->body.num_statements = arrayCount(&statements);
            loop->body.statements = arena_push_array(&statements, &perm_arena);
            arrayFree(&statements);
            return &loop->body.stmt;
          } else {
            jai_log_at(ERROR, prev_pos, "Expected block after loop. Did you forget '{'?\n");
          }
          return 0;
        } else {jai_log_at(ERROR, prev_pos, "Expected expression\n");}
      } else {jai_log_at(ERROR, prev_pos, "Expected ':'\n");}
    } else {jai_log_at(ERROR, prev_pos, "Expected a name for element in list.. For example 'for x : list {...}'\n");}
    return 0;
  }

  else if (token == TOK_RETURN) {
    ReturnAST* ast;
    eatToken();
    ast = arena_push(&perm_arena, sizeof(ReturnAST));
    ast->stmt.type = RETURN_STMT;
    ast->stmt.pos = prev_pos;
    ast->value = parseExpression();
    if (ast->value) {
      return &ast->stmt;
    } else {jai_log_at(ERROR, ast->stmt.pos, "Failed to parse return statement\n");}
  }

  else if (token == TOK_FN) {
    local_persist int functionID = 0;
    FunctionDeclarationAST* fun = arena_push(&perm_arena, sizeof(FunctionDeclarationAST));
    fun->body.stmt.type = FUNCTION_DECLARATION_STMT;
    fun->id = functionID++;
    fun->pos = prev_pos;
    eatToken();

    if (token == TOK_IDENTIFIER) {
      fun->name = arena_push_string(&perm_arena, identifierStr);
      eatToken();

      /* arguments */
      if (token == '(') {
        DynArray args = array_create(0, sizeof(ArgumentAST));
        eatToken();
        

        while (1) {
          ArgumentAST* arg;
          if (token == ')') {
            eatToken();
            break;
          }
          arg = array_push(&args);
          arg->stmt.pos = prev_pos;
          if (token == TOK_IDENTIFIER) {
            arg->name = arena_push_string(&perm_arena, identifierStr);
            eatToken();
            jai_log(DEBUG_INFO, "Found parameter %s\n", arg->name);

            if (token == ':') {
              eatToken();

              if (parseType(&arg->type_ast)) {
                jai_log(DEBUG_INFO, " - with type %s\n", print_type_ast_name(arg->type_ast));
                /* TODO: default parameters */

                if (token == ',') {
                  eatToken();
                  continue;
                }
                else if (token == ')') {
                  eatToken();
                  break;
                } else {jai_log_at(ERROR, prev_pos, "Expected ',' or ')' after parameter\n");}
              } else {jai_log_at(ERROR, prev_pos, "Type expected after ':'\n");}
            } else {jai_log_at(ERROR, prev_pos, "No type found after parameter name (did you forget a ':'?\n");}
          } else {jai_log_at(ERROR, prev_pos, "Identifier expected in parameter list, found %s\n", print_token());}
          arrayFree(&args);
          return 0;
        }
        fun->num_args = arrayCount(&args);
        fun->args = arena_push_array(&args, &perm_arena);
        arrayFree(&args);

        /* return value */
        if (token == ':') {
          eatToken();
          if (!parseType(&fun->return_type_ast)) {
            jai_log_at(ERROR, prev_pos, "Expected type after ':'");
          };
        } else {
          fun->return_type_ast = void_type_ast;
        }

        if (fun->return_type_ast.name) {
          /* Function body */
          fun->body.stmt.pos = prev_pos;

          if (token == '#') {
            eatToken();
            if (token == TOK_IDENTIFIER && strcmp(identifierStr, "foreign") == 0) {
              eatToken();
              fun->is_foreign = true;
            } else {
              jai_log_at(ERROR, prev_pos, "Invalid compiler directive, did you mean to use 'foreign' ?\n");
              return 0;
            }
          }

          else if (token == '{') {
            DynArray statements = array_create(32, sizeof(StatementAST*));
            eatToken();
            while (1) {
              if (token == '}') {
                eatToken();
                break;
              } else {
                StatementAST* stmt = parseStatement();
                if (stmt) {
                  array_pushVal(&statements, &stmt);
                  continue;
                } else {jai_log_at(ERROR, prev_pos, "Failed to parse statement in %s\n", fun->name);}
              }
              goto_matching_brace();
              arrayFree(&statements);
              return 0;
            }
            fun->body.num_statements = arrayCount(&statements);
            fun->body.statements = arena_push_array(&statements, &perm_arena);
            arrayFree(&statements);

          } else {jai_log_at(ERROR, prev_pos, "No '{' or '#foreign' found after function parameter list\n");}

          jai_log(DEBUG_INFO, "Found a function definition with name %s, and %i arguments\n", fun->name, fun->num_args);
          return &fun->body.stmt;
        } else {jai_log_at(ERROR, prev_pos, "Failed to parse return type\n");}
      } else {jai_log_at(ERROR, prev_pos, "token %s not start of a function prototype\n", print_token(token));}
    } else {jai_log_at(ERROR, prev_pos, "Expected function name, found %s\n", print_token());}
    return 0;
  }

  /* struct? */
  else if (token == TOK_STRUCT) {
    StructDeclarationAST* decl = arena_push(&perm_arena, sizeof(StructDeclarationAST));
    decl->stmt.type = STRUCT_DECLARATION_STMT;
    decl->stmt.pos = pos_of_identifier;
    decl->str.type.type = STRUCT_TYPE;
    eatToken();

    if (token == TOK_IDENTIFIER) {
      decl->str.name = arena_push_string(&perm_arena, identifierStr);
      eatToken();

      if (token == '{') {
        DynArray members = array_create(4, sizeof(MemberAST));
        eatToken();

        /* members */
        while (1) {
          MemberAST* member = array_push(&members);
          if (token == TOK_IDENTIFIER) {
            member->name = arena_push_string(&perm_arena, identifierStr);
            member->pos = prev_pos;
            eatToken();
            if (token == ':') {
              eatToken();
              /* TODO: parse default value */
              if (parseType(&member->type_ast)) {
                if (token == '}') {
                  eatToken();
                  break;
                } else {
                  continue;
                }
              } else { jai_log_at(ERROR, prev_pos, "No type after member\n"); }
            } else { jai_log_at(ERROR, prev_pos, "No type after member. Did you forget a ':'?\n"); }
          } else { jai_log_at(ERROR, prev_pos, "Expected member name, instead found %s\n", print_token()); }
          while (token != '}') {
            eatToken();
          }
          eatToken();
          arrayFree(&members);
          return 0;
        }

        decl->str.num_members = arrayCount(&members);
        decl->str.members = arena_push_array(&members, &perm_arena);
        arrayFree(&members);
        return &decl->stmt;
      } else {jai_log_at(ERROR, pos_of_identifier, "Expected '{' after struct keyword\n");}
    } else {jai_log_at(ERROR, prev_pos, "Expected struct name, got %s", print_token());}
    return 0;
  }

  /* Variable declaration? */
  else if (token == TOK_VAR) {
    eatToken();
    if (token == TOK_IDENTIFIER) {
      VariableDeclarationAST* var;

      var = arena_push(&perm_arena, sizeof(VariableDeclarationAST));
      var->stmt.type = VARIABLE_DECLARATION_STMT;
      var->stmt.pos = prev_pos;
      var->name = arena_push_string(&perm_arena, identifierStr);
      eatToken();

      /* type? */
      if (token == ':') {
        eatToken();
        if (!parseType(&var->type_ast)) {
          jai_log_at(ERROR, prev_pos, "Expected type after ':'\n");
          return 0;
        }
      }

      /* assignment? */
      if (token == '=') {
        eatToken();
        var->value = parseExpression();
      }

      /* if no assignment, was there at least a type declaration? */
      if (!var->value && !var->type_ast.name) {
        jai_log_at(ERROR, prev_pos, "Can't infer type of variable. Need type declaration or initial value");
        return 0;
      }

      return &var->stmt;

    } else {jai_log_at(ERROR, prev_pos, "Expected variable name, got token %s", print_token());}
  }

  else {
    ExpressionAST* expr = parseExpression();
    if (expr) {
      /* assignment? */
      if (token == '=') {
        AssignmentAST* ass;
        eatToken();
        ass = arena_push(&perm_arena, sizeof(AssignmentAST));
        ass->stmt.type = ASSIGNMENT_STMT;
        ass->stmt.pos = pos_of_identifier;
        ass->lhs = expr;
        ass->rhs = parseExpression();
        if (ass->rhs) {
          return &ass->stmt;
        }
        else {
          jai_log_at(ERROR, prev_pos, "Failed to parse right hand side of assignment, expected expression\n");
          return 0;
        }
      }

      /* just an expression */
      else {
        expr->stmt.type = EXPRESSION_STMT;
        return &expr->stmt;
      }

      return 0;
    } else {jai_log_at(ERROR, pos_of_identifier, "Failed to parse statement\n"); }
  }
  return 0;
}
internal StatementAST* parseStatement() {
  StatementAST* result;
  while (1) {
    if (token == EOF) {
      return 0;
    } if (token != ';') {
      break;
    }
    eatToken();
  }
  result = _parseStatement();
  while (1) {
    if (token == EOF || token != ';') {
      break;
    }
    eatToken();
  }
  return result;
}


/** 2nd pass: type inference and scope checks **/

internal Constant* getConstantFromExpression(ExpressionAST* val) {
  switch (val->expr_type) {
    case LITERAL_EXPR: {
      return (Constant*) val;
    } break;

    case BINOP_EXPR: {
      BinaryExpressionAST* bin = (BinaryExpressionAST*) val;
      Constant* l = getConstantFromExpression(bin->lhs);
      if (l) {
        Constant* r = getConstantFromExpression(bin->rhs);
        if (r && r->expr.type == l->expr.type) {

          Constant* result = arena_push(&perm_arena, sizeof(Constant));
          result->expr.expr_type = LITERAL_EXPR;
          result->expr.type = r->expr.type;

          switch (r->expr.type->type) {

            #define DO_BIN_INT_OPERATOR(field) \
              switch (bin->operator) { \
                case '+': result->value.field = l->value.field + r->value.field; break; \
                case '-': result->value.field = l->value.field - r->value.field; break; \
                case '*': result->value.field = l->value.field * r->value.field; break; \
                case '/': result->value.field = l->value.field / r->value.field; break; \
                case '%': result->value.field = l->value.field % r->value.field; break; \
                case '|': result->value.field = l->value.field | r->value.field; break; \
                case '&': result->value.field = l->value.field & r->value.field; break; \
                case '^': result->value.field = l->value.field & r->value.field; break; \
              }
            #define DO_BIN_FLOAT_OPERATOR(field) \
              switch (bin->operator) { \
                case '+': result->value.field = l->value.field + r->value.field; break; \
                case '-': result->value.field = l->value.field - r->value.field; break; \
                case '*': result->value.field = l->value.field * r->value.field; break; \
                case '/': result->value.field = l->value.field / r->value.field; break; \
              }

            case INT_TYPE:   {DO_BIN_INT_OPERATOR(int_val);}
            case FLOAT_TYPE: {DO_BIN_FLOAT_OPERATOR(float_val);}
            case I8_TYPE:    {DO_BIN_INT_OPERATOR(i8);}
            case I16_TYPE:   {DO_BIN_INT_OPERATOR(i16);}
            case I32_TYPE:   {DO_BIN_INT_OPERATOR(i32);}
            case I64_TYPE:   {DO_BIN_INT_OPERATOR(i64);}
            case U8_TYPE:    {DO_BIN_INT_OPERATOR(u8);}
            case U16_TYPE:   {DO_BIN_INT_OPERATOR(u16);}
            case U32_TYPE:   {DO_BIN_INT_OPERATOR(u32);}
            case U64_TYPE:   {DO_BIN_INT_OPERATOR(u64);}
            case F32_TYPE:   {DO_BIN_FLOAT_OPERATOR(f32);}
            case F64_TYPE:   {DO_BIN_FLOAT_OPERATOR(f64);}

            default: assert(false);

            #undef DO_BIN_FLOAT_OPERATOR
            #undef DO_BIN_INT_OPERATOR
          }

          return result;

        }
      }
    } break;

    default: {
      jai_log_at(ERROR, val->stmt.pos, "Value of expression could not be determined at compile time\n");
    } break;
  }
  return 0;
}

internal void evaluateTypeOfStruct(StructType* str, CompoundStatementAST* scope);

internal Type* getTypeDeclaration(TypeAST type_ast, CompoundStatementAST* scope, bool recurse_into_structs) {

  if (type_ast.num_partial_types == 0) {
    int i;
    /* builtin? */
    for (i = 0; i < ARRSIZE(builtin_typenames); ++i) {
      if (strcmp(builtin_typenames[i], type_ast.name) == 0) {
        return &builtin_types[i];
      }
    }

    /* user defined type? */
    while (scope) {
      for (i = 0; i < scope->num_statements; ++i) {
        StatementAST* stmt = scope->statements[i];
        if (stmt->type == STRUCT_DECLARATION_STMT) {
          StructType* str = &((StructDeclarationAST*) stmt)->str;
          if (strcmp(str->name, type_ast.name) == 0) {
            if (recurse_into_structs) {
              evaluateTypeOfStruct(str, scope);
            }
            return &str->type;
          }
        }
      }
      scope = scope->parent_scope;
    }
  }

  else {

    switch (type_ast.partial_types->class) {
      case STATIC_ARRAY_TYPE_AST: {
        TypeAST subtype_ast = {0};
        Type* base_type;
        subtype_ast.num_partial_types = type_ast.num_partial_types-1;
        subtype_ast.partial_types = type_ast.partial_types+1;
        subtype_ast.name = type_ast.name;
        base_type = getTypeDeclaration(subtype_ast, scope, true);
        if (base_type) {
          StaticArrayType* result = 0;
          Constant* constant = getConstantFromExpression(type_ast.partial_types->static_array.size);
          if (constant) {
            assert(constant->expr.expr_type == LITERAL_EXPR);
            if (constant->expr.type->type == INT_TYPE) {
              Type **it, **end;
              for (it = arrayBegin(&generated_types), end = arrayEnd(&generated_types); it < end; ++it) {
                StaticArrayType* sa = (StaticArrayType*) *it;
                /* TODO: not int, size_t */ 
                if ((*it)->type == STATIC_ARRAY_TYPE && sa->arr.base_type == base_type && sa->size == constant->value.int_val) {
                  result = (StaticArrayType*) it;
                  break;
                }
              }

              /* create new type if needed */
              if (!result) {
                result = arena_push(&perm_arena, sizeof(StaticArrayType));
                result->arr.type.type = STATIC_ARRAY_TYPE;
                result->arr.base_type = base_type;
                result->size = constant->value.int_val;
                array_pushVal(&generated_types, &result);
              }

              return (Type*) result;

            } else {jai_log_at(ERROR, type_ast.partial_types->static_array.size->stmt.pos, "Array size must be integer, but got '$t'");}
          } else {jai_log_at(ERROR, type_ast.partial_types->static_array.size->stmt.pos, "Array size must be a compile time constant\n");}
        } else {jai_log_at(ERROR, type_ast.partial_types->static_array.size->stmt.pos, "Could not find type %s\n", print_type_ast_name(subtype_ast));}
      } break;

      case ARRAY_TYPE_AST: {
        Type* base_type;
        TypeAST subtype_ast;
        subtype_ast.num_partial_types = type_ast.num_partial_types-1;
        subtype_ast.partial_types = type_ast.partial_types+1;
        subtype_ast.name = type_ast.name;
        base_type = getTypeDeclaration(subtype_ast, scope, true);
        if (base_type) {
          ArrayType* result = 0;
          Type **it, **end;
          for (it = arrayBegin(&generated_types), end = arrayEnd(&generated_types); it < end; ++it) {
            if ((*it)->type == ARRAY_TYPE && ((ArrayType*) *it)->base_type == base_type) {
              result = (ArrayType*) *it;
              break;
            }
          }

          /* create new type if needed */
          if (!result) {
            result = arena_push(&perm_arena, sizeof(ArrayType));
            result->type.type = ARRAY_TYPE;
            result->base_type = base_type;
            array_pushVal(&generated_types, &result);
          }

          return &result->type;

        } else {jai_log_at(ERROR, type_ast.partial_types->static_array.size->stmt.pos, "Could not find type %s\n", print_type_ast_name(subtype_ast));}
      } break;

      case POINTER_TYPE_AST: {
        Type* base_type;
        TypeAST subtype_ast;
        subtype_ast.num_partial_types = type_ast.num_partial_types-1;
        subtype_ast.partial_types = type_ast.partial_types+1;
        subtype_ast.name = type_ast.name;
        base_type = getTypeDeclaration(subtype_ast, scope, true);
        if (base_type) {
          PointerType* result = 0;
          Type **it, **end;
          for (it = arrayBegin(&generated_types), end = arrayEnd(&generated_types); it < end; ++it) {
            if ((*it)->type == POINTER_TYPE && ((PointerType*) *it)->base_type == base_type) {
              result = (PointerType*) *it;
              break;
            }
          }

          if (!result) {
            result = arena_push(&perm_arena, sizeof(PointerType));
            result->type.type = POINTER_TYPE;
            result->base_type = base_type;
            array_pushVal(&generated_types, &result);
          }

          return &result->type;
        } else {jai_log_at(ERROR, type_ast.partial_types->static_array.size->stmt.pos, "Could not find type %s\n", print_type_ast_name(subtype_ast));}

        /* create new type if needed */
      } break;

    }
  }
  return 0;
}

internal DynArray getFunctionDeclarations(char* name, CompoundStatementAST* scope) {
  /* TODO: builtin functions? */
  DynArray funs = array_create(2, sizeof(FunctionDeclarationAST*));
  while (scope) {
    int i;
    for (i = 0; i < scope->num_statements; ++i) {
      StatementAST* stmt = scope->statements[i];
      if (stmt->type == FUNCTION_DECLARATION_STMT) {
        FunctionDeclarationAST* fun = (FunctionDeclarationAST*) stmt;
        if (strcmp(fun->name, name) == 0) {
          array_pushVal(&funs, &fun);
        }
      }
    }
    scope = scope->parent_scope;
  }
  return funs;
}

internal void evaluateTypeOfVariableDeclaration(VariableDeclarationAST* var, CompoundStatementAST* scope);

/* TODO: not int, but size_t */
internal VariableDeclarationAST* getVariableDeclaration(char* name, CompoundStatementAST* scope) {
  while (scope) {
    int i;
    /* check loop implicit variables */
    if (scope->stmt.type == LOOP_STMT) {
      LoopAST* loop = (LoopAST*) scope;
      if (loop->iter.name && strcmp(name, loop->iter.name) == 0) {
        return &loop->iter;
      } else if (loop->index.name && strcmp(name, loop->index.name) == 0) {
        return &loop->index;
      }
    }

    /* TODO: check only statements before this statement */
    /* check local variables */
    for (i = 0; i < scope->num_statements; ++i) {
      StatementAST* stmt = scope->statements[i];
      if (stmt->type == VARIABLE_DECLARATION_STMT) {
        VariableDeclarationAST* var = (VariableDeclarationAST*) stmt;
        if (strcmp(var->name, name) == 0) {
          evaluateTypeOfVariableDeclaration(var, scope);
          return var;
        }
      }
    }

    /* check function args */
    if (scope->stmt.type == FUNCTION_DECLARATION_STMT) {
      FunctionDeclarationAST* fun = (FunctionDeclarationAST*) scope;
      for (i = 0; i < fun->num_args; ++i) {
        ArgumentAST* arg = fun->args + i;
        if (strcmp(arg->name, name) == 0) {
          return arg;
        }
      }
    }
    scope = scope->parent_scope;
  }
  return 0;
}

internal void _evaluateTypeOfStruct(StructType* str, DynArray* parents, CompoundStatementAST* scope) {
  int i;
  for (i = 0; i < str->num_members; ++i) {
    if (!str->members[i].type) {
      str->members[i].type = getTypeDeclaration(str->members[i].type_ast, scope, false);
      if (!str->members[i].type) {
        jai_log_at(ERROR, str->members[i].pos, "Could not find type %s\n", print_type_ast_name(str->members[i].type_ast));
        return;
      }
      if (str->members[i].type->type == STRUCT_TYPE && str->members[i].type_ast.num_partial_types == 0) {
        char **parent, **end;
        for (parent = arrayBegin(parents), end = arrayEnd(parents); parent != end; ++parent) {
          if (strcmp(*parent, str->members[i].type_ast.name) == 0) {
            jai_log_at(ERROR, str->members[i].pos, "TypeClass %s cannot contain itself. Did you mean to use a pointer?\n", *parent);
            exit(1);
          }
        }
        array_pushVal(parents, &str->members[i].type_ast.name);
        _evaluateTypeOfStruct((StructType*) str->members[i].type, parents, scope);
        arrayPop(parents);
      }
    }
  }
}
internal void evaluateTypeOfStruct(StructType* str, CompoundStatementAST* scope) {
  DynArray parents = array_create(4, sizeof(char*));
  array_pushVal(&parents, &str->name);
  _evaluateTypeOfStruct(str, &parents, scope);
  arrayFree(&parents);
}

internal bool hasImplicitConversion(Type* from, Type* to) {
  return from->type == STATIC_ARRAY_TYPE && to->type == ARRAY_TYPE;
}

internal void evaluateTypeOfExpression(ExpressionAST* expr, Type* evidence, CompoundStatementAST* scope);
internal void evaluateTypeOfVariableDeclaration(VariableDeclarationAST* var, CompoundStatementAST* scope) {
  Type* declared_type = 0;
  if (!var || var->type) {
    return;
  }

  if (var->type_ast.name) {
    declared_type = getTypeDeclaration(var->type_ast, scope, true);
    if (!declared_type) {
      jai_log_at(ERROR, var->stmt.pos, "Declared type '%s' for '%s' doesn't exist\n", print_type_ast_name(var->type_ast), var->name);
      var->type = 0;
      return;
    }
    var->type = declared_type;
  }

  if (var->value) {
    evaluateTypeOfExpression(var->value, var->type, scope);
    if (!var->value->type) {
      jai_log_at(ERROR, var->stmt.pos, "Couldn't infer type for %s\n", var->name);
      var->type = 0;
      return;
    }
    if (!var->type) {
      var->type = var->value->type;
    }
  }


  /* check that infered expression type and stated type match */
  if (var->value && declared_type && var->value->type != declared_type && !hasImplicitConversion(var->value->type, declared_type)) {
    jai_log_at(ERROR, var->value->stmt.pos, "Inferred type of '%s': '$t' differs from declared type '$t'\n", var->name, var->value->type, declared_type);
    var->type = 0;
    return;
  }

  return;
}

internal void evaluateTypeOfFunction(FunctionDeclarationAST* fun, CompoundStatementAST* scope) {
  int i;
  if (!fun || fun->return_type) {
    return;
  }
  assert(fun->return_type_ast.name); /* return_type should have been set to void */

  for (i = 0; i < fun->num_args; ++i) {
    fun->args[i].type = getTypeDeclaration(fun->args[i].type_ast, scope, true);
    if (!fun->args[i].type) {
      jai_log_at(ERROR, fun->args[i].stmt.pos, "Could not find type '%s'\n", print_type_ast_name(fun->args[i].type_ast));
    }
  }

  fun->return_type = getTypeDeclaration(fun->return_type_ast, scope, true);
  if (!fun->return_type) {
    jai_log_at(ERROR, fun->pos, "Could not find definition of return type '%s' of '%s'\n", print_type_ast_name(fun->return_type_ast), fun->name);
  }
}

internal void evaluateTypeOfExpression(ExpressionAST* expr, Type* evidence, CompoundStatementAST* scope) {
  if (expr->type) {
    return;
  }

  switch (expr->expr_type) {
    case UNKNOWN_EXPR: assert(false); break;

    case LITERAL_EXPR: {
      /* type should have been created by the parser already */
      if (!expr->type) {LOG_AND_DIE("Something went wrong in the compiler. Please report the bug!\n");}
    } break;

    case ARRAY_SUBSCRIPT_EXPR: {
      ArraySubscriptAST* as = (ArraySubscriptAST*) expr;
      evaluateTypeOfExpression(as->base, 0, scope);
      if (as->base) {
        Type* base_type = 0;

        if (as->base->type->type == ARRAY_TYPE || as->base->type->type == STATIC_ARRAY_TYPE) {
          base_type = ((ArrayType*) as->base->type)->base_type;
        } else {
          jai_log_at(ERROR, as->base->stmt.pos, "Type must be of array type, but found type $t", as->base->type);
        }

        if (base_type) {
          evaluateTypeOfExpression(as->subscript, 0, scope);
          if (as->subscript->type) {
            /* TODO: size_t instead */
            if (as->subscript->type->type == INT_TYPE) {
              as->expr.type = base_type;
            } else {jai_log_at(ERROR, as->subscript->stmt.pos, "Array subscript must be integer, found $t\n", as->subscript->type);}
          }
        }
      }
    } break;

    case CALL_EXPR: {
      CallAST* call = (CallAST*) expr;
      if (call->base->expr_type == VARIABLE_GET_EXPR) {
        VariableGetAST* base = (VariableGetAST*) call->base;
        DynArray funs = getFunctionDeclarations(base->name, scope);
        if (arrayCount(&funs)) {
          /* eval functions and find best match */
          int matches = 0;
          FunctionDeclarationAST **it, **end;
          for (it = arrayBegin(&funs), end = arrayEnd(&funs); it < end; ++it) {
            FunctionDeclarationAST* fun = *it;
            bool match = true;
            if (!fun->return_type && fun->return_type_ast.name) {
              evaluateTypeOfFunction(fun, scope);
            }
            /* TODO: implicit type conversions? */
            if (call->num_args != fun->num_args) {
              match = false;
            } else {
              int i;
              for (i = 0; i < fun->num_args; ++i) {
                evaluateTypeOfExpression(call->args[i], fun->args[i].type, scope);
                if (call->args[i]->type != fun->args[i].type) {
                  match = false;
                  break;
                }
              }
            }

            if (match) {
              ++matches;
              call->fun = *it;
              expr->type = (*it)->return_type;
            }
          }
          if (matches > 1) {
            /* TODO: print out actual matches, and not all overload */
            jai_log_at(ERROR, expr->stmt.pos, "Multiple matching overloads for %s.\n", base->name);
            jai_log(NOTE, "Overloads are:\n");
            for (it = arrayBegin(&funs), end = arrayEnd(&funs); it < end; ++it) {
              jai_log_at(NOTE, (*it)->pos, "\n");
            }
          } else if (matches == 0) {
            jai_log_at(ERROR, expr->stmt.pos, "Could not find matching function overload for %s.\n", base->name);
            jai_log(NOTE, "Alternatives are:\n");
            for (it = arrayBegin(&funs), end = arrayEnd(&funs); it < end; ++it) {
              jai_log_at(NOTE, (*it)->pos, "\n");
            }
          }

        } else {jai_log_at(ERROR, call->expr.stmt.pos, "Unknown function '%s'\n", base->name); }

        arrayFree(&funs);

      } else {jai_log_at(ERROR, call->expr.stmt.pos, "Functions pointers not yet supported\n");}
    } break;

    case VARIABLE_GET_EXPR: {
      VariableGetAST* var_ref = (VariableGetAST*) expr;
      VariableDeclarationAST* var = getVariableDeclaration(var_ref->name, scope);
      if (var) {
        expr->type = var->type;
      } else {jai_log_at(ERROR, var_ref->expr.stmt.pos, "Use of undeclared variable '%s'\n", var_ref->name);}
    } break;

    case MEMBER_ACCESS_EXPR: {
      MemberAccessAST* access = (MemberAccessAST*) expr;
      evaluateTypeOfExpression(access->base, 0, scope);
      if (access->base->type) {
        if (access->base->type->type == STRUCT_TYPE) {
          /* find member */
          StructType* str = (StructType*) access->base->type;
          MemberAST* match = 0;
          int i;
          for (i = 0; i < str->num_members; ++i) {
            MemberAST* member = str->members + i;
            if (strcmp(member->name, access->name) == 0) {
              match = member;
              break;
            }
          }

          if (match) {
            assert(match->type);
            access->expr.type = match->type;
          } else {jai_log_at(ERROR, access->expr.stmt.pos, "struct %s has no member %s\n", str->name, access->name);}
        } else if (access->base->type->type == ARRAY_TYPE) {

          if (!strcmp(access->name, "count") || !strcmp(access->name, "size") || !strcmp(access->name, "length")) {
            /* TODO: size_t */
            access->expr.type = &builtin_types[INT_TYPE];
          }
          else if (!strcmp(access->name, "data")) {
            jai_log_at(ERROR, access->expr.stmt.pos, "Pointers not yet implemented\n");
            UNIMPLEMENTED;
          }

        } else {jai_log_at(ERROR, access->base->stmt.pos, "Can only access members of structs or arrays, '$t' is neither\n", access->base->type);}
      }
    } break;

    case STRUCT_INIT_EXPR: {
      StructInitializationAST* ast = (StructInitializationAST*) expr;
      if (evidence) {
        if (evidence->type == STRUCT_TYPE) {
          StructType* target = (StructType*) evidence;
          bool all_match = true;
          int i;
          for (i = 0; i < ast->num_members; ++i) {
            int j;
            /* TODO: make sure each member is only mentioned once */
            bool name_match = false;
            for (j = 0; j < target->num_members; ++j) {
              if (strcmp(ast->members[i].name, target->members[j].name) == 0) {
                name_match = true;
                ast->members[i].member = target->members + j;
              }
            }
            if (!name_match) {
              jai_log_at(ERROR, expr->stmt.pos, "%s has no member %s\n", target->name, ast->members[i].name);
              all_match = false;
              break;
            }
            assert(ast->members[i].member->type);
            evaluateTypeOfExpression(ast->members[i].value, ast->members[i].member->type, scope);
            if (ast->members[i].value->type != ast->members[i].member->type) {
              jai_log_at(ERROR, expr->stmt.pos, "The member '%s' in '%s' has type '$t', but the expression has type '$t'\n", ast->members[i].member->name, target->name, ast->members[i].member->type, ast->members[i].value->type);
              all_match = false;
              break;
            }
          }
          if (all_match) {
            expr->type = evidence;
          }
        } else {jai_log_at(ERROR, expr->stmt.pos, "Cannot use struct initialization for non-struct type $t\n", evidence);}
      } else {jai_log_at(ERROR, expr->stmt.pos, "Not enough type information top evaluate type of struct initialization\n");}
    } break;

    case BINOP_EXPR: {
      BinaryExpressionAST* ast = (BinaryExpressionAST*) expr;
      evaluateTypeOfExpression(ast->lhs, 0, scope);
      evaluateTypeOfExpression(ast->rhs, ast->lhs->type, scope);
      /* TODO: check if operator overload exists */
      if (ast->lhs->type == ast->rhs->type) {
        ast->expr.type = ast->lhs->type;
      } else {
        jai_log_at(ERROR, ast->lhs->stmt.pos, "Type of left hand side '$t' does not match type of right hand side '$t'\n", ast->lhs->type, ast->rhs->type);
      }
    } break;
  }

  if (evidence && expr->type) {
    /* some implicit conversions.. */
    if (expr->type->type == STATIC_ARRAY_TYPE && evidence->type == ARRAY_TYPE) {
      return;
    }

    if (evidence == &array_no_internal_type) {
      if (expr->type->type != ARRAY_TYPE && expr->type->type != STATIC_ARRAY_TYPE) {
        jai_log_at(ERROR, expr->stmt.pos, "Context demands array, but expression was of type $t\n", expr->type);
        expr->type = 0;
      }
    }
    else if (evidence != expr->type) {
      jai_log_at(ERROR, expr->stmt.pos, "Expected type $t, but expression was type $t\n", evidence, expr->type);
      expr->type = 0;
    }
  }
}

internal void doTypeInferenceForScope(CompoundStatementAST* scope) {
  /* TODO: check for duplicates declarations within same scope */

  int i;
  for (i = 0; i < scope->num_statements; ++i) {
    StatementAST* stmt = scope->statements[i];
    switch (stmt->type) {

      case STRUCT_DECLARATION_STMT: {
        StructDeclarationAST* decl = (StructDeclarationAST*) stmt;
        evaluateTypeOfStruct(&decl->str, scope);
        jai_log(DEBUG_INFO, "Evaluated type of struct '%s'\n", decl->str.name);
      } break;

      case VARIABLE_DECLARATION_STMT: {
        VariableDeclarationAST* var = (VariableDeclarationAST*) stmt;
        evaluateTypeOfVariableDeclaration(var, scope);
        if (var->type) {
          jai_log_at(DEBUG_INFO, var->stmt.pos, "Evaluated type of '%s' to '$t'\n", var->name, var->type);
        } else {
          jai_log_at(ERROR, var->stmt.pos, "Failed to evaluate type of '%s'\n", var->name);
        }
      } break;

      case FUNCTION_DECLARATION_STMT: {
        FunctionDeclarationAST* fun = (FunctionDeclarationAST*) stmt;
        fun->body.parent_scope = scope;
        evaluateTypeOfFunction(fun, scope);
        if (fun->body.num_statements) {
          doTypeInferenceForScope(&fun->body);
        }
        jai_log_at(DEBUG_INFO, fun->pos, "Evaluated body of '%s'\n", fun->name);
      } break;

      case EXPRESSION_STMT: {
        ExpressionAST* expr = (ExpressionAST*) stmt;
        evaluateTypeOfExpression(expr, 0, scope);
        if (expr->type) {
          jai_log_at(DEBUG_INFO, expr->stmt.pos, "Evaluated type of expression to $t\n", expr->type);
        } else {
          jai_log_at(ERROR, expr->stmt.pos, "Failed to infer type of expression\n");
        }
      } break;

      case COMPOUND_STMT: {
        CompoundStatementAST* subscope = (CompoundStatementAST*) stmt;
        subscope->parent_scope = scope;
        doTypeInferenceForScope(subscope);
      } break;

      case LOOP_STMT: {
        LoopAST* loop = (LoopAST*) stmt;
        loop->body.parent_scope = scope;
        /* TODO: not int, but size_t */
        if (loop->to) {
          evaluateTypeOfExpression(loop->from, &builtin_types[INT_TYPE], scope);
          evaluateTypeOfExpression(loop->to, &builtin_types[INT_TYPE], scope);
          if (!loop->from->type || !loop->to->type) {
            jai_log_at(ERROR, loop->from->stmt.pos, "Loop indexes are not integers\n");
          }
          if (loop->from->type != loop->to->type) {
            jai_log_at(ERROR, loop->from->stmt.pos, "Types of start index and stop index differ\n");
          }
          loop->iter.type = loop->from->type;
        } else {
          evaluateTypeOfExpression(loop->from, &array_no_internal_type, scope);
          assert(!loop->to);
          if (loop->from->type) {
            ArrayType* arr = (ArrayType*) loop->from->type;
            loop->iter.type = arr->base_type;
          }
          else {jai_log_at(ERROR, loop->from->stmt.pos, "Loop iteratable must be of array type\n"); }
        }

        doTypeInferenceForScope(&loop->body);
      } break;

      case ASSIGNMENT_STMT: {
        AssignmentAST* ass = (AssignmentAST*) stmt;
        evaluateTypeOfExpression(ass->lhs, 0, scope);
        evaluateTypeOfExpression(ass->rhs, ass->lhs->type, scope);
        if (!ass->lhs->type) {
          jai_log_at(ERROR, stmt->pos, "Could not infer type of left hand side of assignment\n");
        }
        else if (!ass->rhs->type) {
          jai_log_at(ERROR, stmt->pos, "Could not infer type of right hand side of assignment\n");
        }
        else if (ass->lhs->type != ass->rhs->type && !hasImplicitConversion(ass->rhs->type, ass->lhs->type)) {
          jai_log_at(ERROR, stmt->pos, "Left hand side has type $t, but right hand side has type $t\n", ass->lhs->type, ass->rhs->type);
        }
      } break;

      case RETURN_STMT: {
        ReturnAST* ret = (ReturnAST*) stmt;
        CompoundStatementAST* scp = scope;
        while (scp) {
          if (scp->stmt.type == FUNCTION_DECLARATION_STMT) {
            break;
          }
          scp = scp->parent_scope;
        }

        if (scp) {
          FunctionDeclarationAST* fun = (FunctionDeclarationAST*) scp;
          evaluateTypeOfExpression(ret->value, fun->return_type, scope);
          if (ret->value->type) {
            if (ret->value->type != fun->return_type) {
              jai_log_at(ERROR, ret->stmt.pos, "Return type $t does not match function return type $t\n", ret->value->type, fun->return_type);
            }
          } else {jai_log_at(ERROR, ret->stmt.pos, "Could not infer type of return value\n");};
        } else {jai_log_at(ERROR, ret->stmt.pos, "Can only use return inside function\n");}
      } break;

      case UNKNOWN_STMT: {
        jai_log(DEBUG_ERROR, "Unknown statement\n");
      } break;

    }

  }
}

global String _print_buf;
internal void vprint(FILE* file, char* fmt, va_list args) {
  char *p, *str;
  string_clear(&_print_buf);
  string_append(&_print_buf, fmt);
  p = str = string_get(&_print_buf);

  while (*p) {
    if (*p == '$') {
      bool match = true;
      *p = 0;
      vfprintf(file, str, args);
      str = p + 1;
      ++p;

      switch (*p) {

        case 't': {
          Type* type = va_arg(args, Type*);
          if (type) {
            while (1) {
              Type* next = 0;
              switch (type->type) {
                case UNKNOWN_TYPE:
                case VOID_TYPE: fprintf(file, "void"); break;
                case INT_TYPE: fprintf(file, "int"); break;
                case FLOAT_TYPE: fprintf(file, "float"); break;
                case F32_TYPE: fprintf(file, "f32"); break;
                case F64_TYPE: fprintf(file, "f64"); break;
                case U8_TYPE: fprintf(file, "u8"); break;
                case U16_TYPE: fprintf(file, "u16"); break;
                case U32_TYPE:  fprintf(file, "u32");break;
                case U64_TYPE:  fprintf(file, "u64");break;
                case I8_TYPE: fprintf(file, "i8"); break;
                case I16_TYPE: fprintf(file, "i16"); break;
                case I32_TYPE:  fprintf(file, "i32");break;
                case I64_TYPE:  fprintf(file, "i64");break;
                case USIZE_TYPE: fprintf(file, "usize"); break;
                case STRUCT_TYPE: fprintf(file, "%s", ((StructType*) type)->name); break;
                case ARRAY_TYPE: {
                  fprintf(file, "[]");
                  next = ((ArrayType*) type)->base_type;
                }; break;
                case STATIC_ARRAY_TYPE: {
                  fprintf(file, "[%i]", ((StaticArrayType*) type)->size);
                  next = ((StaticArrayType*) type)->arr.base_type;
                }; break;
                case POINTER_TYPE: {
                  fprintf(file, "^");
                  next = ((PointerType*) type)->base_type;
                }; break;
                case FUNCTION_TYPE: fprintf(file, "<function pointer>"); break;
              }
              if (next) {
                type = next;
              } else {
                break;
              }
            }
          } else {
            fprintf(file, "<unknown>");
          }
        }; break;

        case 'T': {
          Type* type = va_arg(args, Type*);
          /* ^[20][16]^[8]int -> int *(*(*s)[20][16])[8] */
          if (type->type == POINTER_TYPE) {
            fputs("T_void*", file);
          } else if (type->type == ARRAY_TYPE) {
            fputs("Slice", file);
          } else if (type->type == STATIC_ARRAY_TYPE) {
            print(file, "$t*", ((ArrayType*) type)->base_type);
          } else if (type->type == STRUCT_TYPE) {
            fprintf(file, "T_%s", ((StructType*) type)->name);
          } else {
            int i = type - builtin_types;
            assert(i > 0 && i < ARRSIZE(builtin_types));
            fprintf(file, "T_%s", builtin_typenames[i]);
          }
        }; break;

        case 'f': {
          FunctionDeclarationAST* fun = va_arg(args, FunctionDeclarationAST*);
          fprintf(file, "%s_%i", fun->name, fun->id);
        }; break;

        default: {
          match = false;
        } break;
      }

      if (match) {
        ++p;
        ++str;
      } else {
        fputc('$', file);
      }
    } else {
      ++p;
    }
  }
  vfprintf(file, str, args);
}
internal void print(FILE* file, char* fmt, ...) {
  va_list args;
  va_start(args, fmt);
  vprint(file, fmt, args);
  va_end(args);
}

/** Compile AST to C **/

internal void compile_variable_decl_to_c(Type* type, char* name, bool prefix, FILE* file) {
  /* We handle the types, C only needs to know the size of the static allocation */
  /* ^^[32][8]int      ->   void* name; (pointer size) */
  /* [32][8]^[64]^int  ->   void* name[32][8] (32*8*pointer size) */
  /* [32][8]int        ->   int name[32][8] */
  /* [][32]int         ->   Slice name; */
  char* var_prefix = prefix ? "v_" : "";

  if (type->type == POINTER_TYPE) {
    fprintf(file, "\nT_void* %s%s", var_prefix, name);
  } else {
    int num_items = 1;
    while (type->type == STATIC_ARRAY_TYPE) {
      StaticArrayType* sa = (StaticArrayType*) type;
      num_items *= sa->size;
      type = sa->arr.base_type;
    }

    print(file, "\n$T %s%s", type, var_prefix, name);

    if (num_items > 1) {
      fprintf(file, "[%i]", num_items);
    }
  }
}

internal void compile_type_to_c(FILE* header, FILE* body, StructType* str, DynArray* done) {
  StructType **s, **end;
  int i;
  for (s = arrayBegin(done), end = arrayEnd(done); s != end; ++s) {
    if (*s == str) {
      return;
    }
  }
  array_pushVal(done, &str);

  /* check if we need to compile the members types first */
  for (i = 0; i < str->num_members; ++i) {
    MemberAST* member = str->members + i;
    if (member->type->type == STRUCT_TYPE) {
      compile_type_to_c(header, body, (StructType*) member->type, done);
    }
  }

  jai_log(DEBUG_INFO, "Compiling struct %s to C\n", str->name);

  /* Compile struct definition */
  fprintf(header, "typedef struct T_%s T_%s;\n", str->name, str->name);
  fprintf(body, "typedef struct T_%s {", str->name);
  for (i = 0; i < str->num_members; ++i) {
    MemberAST* member = str->members + i;
    compile_variable_decl_to_c(member->type, member->name, false, body);
    fprintf(body, ";");
  }
  fprintf(body, "\n} T_%s;\n\n", str->name);
  /* TODO: check for default values */
  if (str->has_initializer) {
    UNIMPLEMENTED;
  }
}

internal void compile_expression_to_c(ExpressionAST* expr, FILE* body) {
  switch (expr->expr_type) {
    case UNKNOWN_EXPR: assert(false); break;

    case LITERAL_EXPR: {
      switch (expr->type->type) {
        case FLOAT_TYPE: fprintf(body, "%f", ((Constant*) expr)->value.float_val); break;
        case INT_TYPE: fprintf(body, "%i", ((Constant*) expr)->value.int_val); break;
        case I8_TYPE: fprintf(body, "%i", ((Constant*) expr)->value.i8); break;
        case I16_TYPE: fprintf(body, "%i", ((Constant*) expr)->value.i16); break;
        case I32_TYPE: fprintf(body, "%i", ((Constant*) expr)->value.i32); break;
        case I64_TYPE: fprintf(body, "%li", ((Constant*) expr)->value.i64); break;
        case U8_TYPE: fprintf(body, "%u", ((Constant*) expr)->value.u8); break;
        case U16_TYPE: fprintf(body, "%u", ((Constant*) expr)->value.u16); break;
        case U32_TYPE: fprintf(body, "%u", ((Constant*) expr)->value.u32); break;
        case U64_TYPE: fprintf(body, "%lu", ((Constant*) expr)->value.u64); break;
        case F32_TYPE: fprintf(body, "%f", ((Constant*) expr)->value.f32); break;
        case F64_TYPE: fprintf(body, "%f", ((Constant*) expr)->value.f64); break;
        default: UNREACHABLE; break;
      }
    } break;

    case ARRAY_SUBSCRIPT_EXPR: {
      /* CONTINUE - We collapse consecutive static array sizes. Calculate the resulting offset of an array subscript */
      ArraySubscriptAST* as = (ArraySubscriptAST*) expr;
      print(body, "(", as->base->type);
      compile_expression_to_c(as->base, body);
      fprintf(body, ")[");
      compile_expression_to_c(as->subscript, body);
      fprintf(body, "]");
    } break;

    case VARIABLE_GET_EXPR: {
      VariableGetAST* var = (VariableGetAST*) expr;
      fprintf(body, "v_%s", var->name);
    } break;

    case MEMBER_ACCESS_EXPR: {
      MemberAccessAST* ast = (MemberAccessAST*) expr;
      compile_expression_to_c(ast->base, body);
      fprintf(body, ".%s", ast->name);
    } break;

    case CALL_EXPR: {
      CallAST* call = (CallAST*) expr;
      if (call->base->expr_type == VARIABLE_GET_EXPR) {
        int i;
        VariableGetAST* base = (VariableGetAST*) call->base;
        if (call->fun->is_foreign) {
          fprintf(body, "%s(", base->name);
        }
        else {
          print(body, "$f(", call->fun);
        }
        for (i = 0; i < call->num_args; ++i) {
          compile_expression_to_c(call->args[i], body);
          if (i != call->num_args - 1) {
            fprintf(body, ", ");
          }
        }
        fprintf(body, ")");
      } else {jai_log_at(ERROR, call->base->stmt.pos, "Function pointers not yet implemented");}
    } break;

    case BINOP_EXPR: {
      BinaryExpressionAST* bin = (BinaryExpressionAST*) expr;
      fprintf(body, "(");
      compile_expression_to_c(bin->lhs, body);
      fprintf(body, "%c", bin->operator);
      compile_expression_to_c(bin->rhs, body);
      fprintf(body, ")");
    } break;

    case STRUCT_INIT_EXPR: {
      jai_log_at(ERROR, expr->stmt.pos, "<internal>: Cannot directly compile a struct initialization\n");
    } break;
  }
}

internal void _get_member_access_chain(String* res, ExpressionAST* expr) {
  if (expr->expr_type == VARIABLE_GET_EXPR) {
    string_append(res, ((VariableGetAST*) expr)->name);
  }
  else if (expr->expr_type == MEMBER_ACCESS_EXPR) {
    _get_member_access_chain(res, ((MemberAccessAST*) expr)->base);
    string_append_char(res, '.');
    string_append(res, ((MemberAccessAST*) expr)->name);
  }
  else {jai_log(DEBUG_ERROR, "Member access is only supported on variables\n"); }
}
internal String get_member_access_chain(MemberAccessAST* acc) {
  String s = string_create(0);
  _get_member_access_chain(&s, &acc->expr);
  return s;
}

internal void _compile_struct_init_to_c(String name, StructInitializationAST* init, FILE* file) {
  int i;
  for (i = 0; i < init->num_members; ++i) {
    MemberInitializationAST* member = init->members + i;
    int len;
    string_append_char(&name, '.');
    len = string_append(&name, member->name);
    if (member->value->expr_type == STRUCT_INIT_EXPR) {
      _compile_struct_init_to_c(name, (StructInitializationAST*) member->value, file);
    } else {
      fprintf(file, "\n%s = ", string_get(&name));
      compile_expression_to_c(member->value, file);
      fprintf(file, ";");
    }
    string_pop(&name, len+1);
  }
}
internal void compile_struct_init_to_c(ExpressionAST* expr, StructInitializationAST* init, FILE* file, bool mangle) {
  /* TODO: check for lvalue */
  if (expr->expr_type == VARIABLE_GET_EXPR) {
    String s = string_create(mangle ? "v_" : "");
    string_append(&s, ((VariableGetAST*) expr)->name);
    _compile_struct_init_to_c(s, init, file);
    string_free(&s);
  }
  else if (expr->expr_type == MEMBER_ACCESS_EXPR) {
    String chain = get_member_access_chain((MemberAccessAST*) expr);
    _compile_struct_init_to_c(chain, init, file);
    string_free(&chain);
  }
}

internal void compile_statement_to_c(StatementAST* stmt, FILE* file) {
  switch (stmt->type) {

    case EXPRESSION_STMT: {
      ExpressionAST* expr = (ExpressionAST*) stmt;
      fprintf(file, "\n");
      compile_expression_to_c(expr, file);
      fprintf(file, ";");
    } break;

    case LOOP_STMT: {
      LoopAST* loop = (LoopAST*) stmt;
      int i;

      /* ranged based loop? */
      if (loop->to) {
        
        if (loop->iter.name) {
          fprintf(file, "\nfor (int v_%s = ", loop->iter.name);
        } else {
          fputs("\nfor (int it_index = ", file);
        }
        compile_expression_to_c(loop->from, file);

        fprintf(file, ", end_index = ");
        compile_expression_to_c(loop->to, file);

        if (loop->index.name) {
          fprintf(file, ", v_%s = 0", loop->index.name);
        }

        if (loop->iter.name) {
          fprintf(file, "; v_%s < end_index; ++v_%s", loop->iter.name, loop->iter.name);
        } else {
          fputs("; it_index < end_index; ++it_index", file);
        }

        if (loop->index.name) {
          fprintf(file, ", ++v_%s", loop->index.name);
        }

        fprintf(file, ") {");
      }
      /* array loop? */
      else {
        if (loop->from->type->type == STATIC_ARRAY_TYPE) {
          if (loop->index.name) {
            fprintf(file, "\nfor (int v_%s = 0; v_%s < %i; ++v_%s) {", loop->index.name, loop->index.name, ((StaticArrayType*) loop->from->type)->size, loop->index.name);
          } else {
            fprintf(file, "\nfor (int it_index = 0; it_index < %i; ++it_index) {", ((StaticArrayType*) loop->from->type)->size);
          }
        }
        else if (loop->from->type->type == ARRAY_TYPE) {
          if (loop->index.name) {
            fprintf(file, "\nfor (int v_%s = 0, end_index = ", loop->index.name);
          } else {
            fprintf(file, "\nfor (int it_index = 0, end_index = ");
          }
          compile_expression_to_c(loop->from, file);
          fprintf(file, ".length; ");
          if (loop->index.name) {
            fprintf(file, "v_%s < end_index; ++v_%s) {", loop->index.name, loop->index.name);
          } else {
            fprintf(file, "it_index < end_index; ++it_index) {");
          }
        } else {assert(false);}

        /* get array element */
        if (loop->iter.name) {
          ArrayType* arr = (ArrayType*) loop->from->type;
          compile_variable_decl_to_c(arr->base_type, loop->iter.name, true, file);
          print(file, " = (($T*) ", arr->base_type);
          compile_expression_to_c(loop->from, file);
          if (loop->from->type->type == ARRAY_TYPE) {
            fprintf(file, ".data");
          }
          fputc(')', file);

          if (loop->index.name) {
            fprintf(file, "[v_%s];", loop->index.name);
          } else {
            fprintf(file, "[it_index];");
          }
        }
      }
      for (i = 0; i < loop->body.num_statements; ++i) {
        compile_statement_to_c(loop->body.statements[i], file);
      }
      fprintf(file, "\n};");
    } break;

    case VARIABLE_DECLARATION_STMT: {
      VariableDeclarationAST* var = (VariableDeclarationAST*) stmt;
      compile_variable_decl_to_c(var->type, var->name, true, file);

      /* structs have initialization and stuff */
      if (var->type->type == STRUCT_TYPE) {

        StructInitializationAST* init = (StructInitializationAST*) var->value;
        StructType* str = (StructType*) var->type;
        fprintf(file, " = {0};");
        if (str->has_initializer) {
          UNIMPLEMENTED;
        }
        if (var->value) {
          compile_struct_init_to_c(var->value, init, file, true);
        }

      }
      else if (var->value) {
        fprintf(file, " = ");
        compile_expression_to_c(var->value, file);
        fprintf(file, ";");
      }
      else {
        fprintf(file, " = {0};");
      }
    } break;

    case ASSIGNMENT_STMT: {
      AssignmentAST* ass = (AssignmentAST*) stmt;
      fprintf(file, "\n");

      if (ass->lhs->expr_type == VARIABLE_GET_EXPR || ass->lhs->expr_type == MEMBER_ACCESS_EXPR || ass->lhs->expr_type == ARRAY_SUBSCRIPT_EXPR) {
        if (ass->rhs->expr_type == STRUCT_INIT_EXPR) {
          StructInitializationAST* init = (StructInitializationAST*) ass->rhs;
          fprintf(file, "\nmemset(&");
          compile_expression_to_c(ass->lhs, file);
          fprintf(file, ", 0, sizeof(");
          compile_expression_to_c(ass->lhs, file);
          fprintf(file, "));");
          compile_struct_init_to_c(ass->lhs, init, file, true);
        } else if (ass->rhs->type->type == STATIC_ARRAY_TYPE && ass->lhs->type->type == ARRAY_TYPE) {
          compile_expression_to_c(ass->lhs, file);
          fprintf(file, " = static_array_into_array(");
          compile_expression_to_c(ass->rhs, file);
          /* TODO: size_t */
          fprintf(file, ", %i);", ((StaticArrayType*) ass->rhs->type)->size);
        } else {
          compile_expression_to_c(ass->lhs, file);
          fprintf(file, " = ");
          compile_expression_to_c(ass->rhs, file);
          fprintf(file, ";");
        }
      } else {
        /* TODO: pretty-print expression types */
        jai_log_at(ERROR, ass->stmt.pos, "Assignment to expression of type %i is not allowed\n", ass->lhs->expr_type);
      }
    } break;

    case RETURN_STMT: {
      ReturnAST* ret = (ReturnAST*) stmt;
      if (ret->value->expr_type == STRUCT_INIT_EXPR) {
        StructInitializationAST* init = (StructInitializationAST*) ret->value;
        StructType* str = (StructType*) init->expr.type;
        fprintf(file, "\n{");
        compile_variable_decl_to_c(&str->type, "__returnval", false, file);
        fprintf(file, " = {0};");
        if (str->has_initializer) {
          UNIMPLEMENTED;
        } else {
          VariableGetAST var = {0};
          var.name = "__returnval";
          var.expr.expr_type = VARIABLE_GET_EXPR;
          var.expr.type = init->expr.type;
          compile_struct_init_to_c(&var.expr, init, file, false);
          fprintf(file, "\nreturn __returnval;\n}");
        }
      } else {
        fprintf(file, "\nreturn ");
        compile_expression_to_c(ret->value, file);
        fprintf(file, ";");
      }
    } break;

    case UNKNOWN_STMT: {
      jai_log(DEBUG_ERROR, "Trying to compile an unknown statement type???\n");
    } break;

  }
}

internal void compile_block_to_c(CompoundStatementAST* block, FILE* file) {
  int i;
  fprintf(file, "{");
  for (i = 0; i < block->num_statements; ++i) {
    compile_statement_to_c(block->statements[i], file);
  }
  fprintf(file, "\n};\n");
}

/** Compile AST to LLVM */
/*
internal void compile_to_llvm() {
  UNIMPLEMENTED;
}
*/

/** Main **/

int main(int argc, char const *argv[]) {

  #ifdef DEBUG
    test();
  #endif

  if (argc == 1) {
    jai_log_at(ERROR, prev_pos, "Usage: %s <filename>\n", argv[0]);
    return 1;
  }
  else {
    next_pos.file = (char*) argv[1];
    file = fopen(argv[1], "r");
    if (!file) {
      jai_log_at(ERROR, prev_pos, "Could not open file %s\n", argv[1]);
      return 1;
    }
  }

  /* enable formatting? */
  #ifdef LINUX
    if (isatty(1)) {
      enableFormatting();
    }
  #endif

  /* init globals */
  {
    perm_arena = arena_create();
    generated_types = array_create(32, sizeof(Type*));
    _print_buf = string_create(0);
    _print_buf = string_create(0);
    void_type_ast.name = "void";
    builtin_types[1].type = VOID_TYPE;
    builtin_types[2].type = INT_TYPE;
    builtin_types[3].type = FLOAT_TYPE;
    builtin_types[4].type = F32_TYPE;
    builtin_types[5].type = F64_TYPE;
    builtin_types[6].type = U8_TYPE;
    builtin_types[7].type = U16_TYPE;
    builtin_types[8].type = U32_TYPE;
    builtin_types[9].type = U64_TYPE;
    builtin_types[10].type = I8_TYPE;
    builtin_types[11].type = I16_TYPE;
    builtin_types[12].type = I32_TYPE;
    builtin_types[13].type = I64_TYPE;
    builtin_types[14].type = USIZE_TYPE;
  }

  /** Parsing **/

  {
    CompoundStatementAST* global_scope = arena_push(&perm_arena, sizeof(CompoundStatementAST));
    DynArray statements = array_create(32, sizeof(StatementAST*));

    eatToken();
    global_scope->stmt.type = COMPOUND_STMT;
    

    while (token != TOK_EOF) {
      StatementAST* stmt = parseStatement();
      if (stmt) {
        switch (stmt->type) {
          case FUNCTION_DECLARATION_STMT: {
            FunctionDeclarationAST* fun = (FunctionDeclarationAST*) stmt;
            fun->body.parent_scope = global_scope;
            array_pushVal(&statements, &stmt);
            jai_log(DEBUG_INFO, "Adding function definition %s\n", ((FunctionDeclarationAST*) stmt)->name);
          } break;
          case VARIABLE_DECLARATION_STMT: {
            array_pushVal(&statements, &stmt);
            jai_log(DEBUG_INFO, "Adding variable declaration %s with declared type %s\n", ((VariableDeclarationAST*) stmt)->name, print_type_ast_name(((VariableDeclarationAST*) stmt)->type_ast));
          } break;
          case STRUCT_DECLARATION_STMT: {
            array_pushVal(&statements, &stmt);
            jai_log(DEBUG_INFO, "Adding type $t\n", &((StructDeclarationAST*) stmt)->str);
          } break;
          default:
            jai_log_at(ERROR, stmt->pos, "Only variable, struct and function definitions allowed at global scope\n");
            break;
        }
      } else {
        if (token != TOK_EOF) {
          return 1;
        }
      }
    }

    global_scope->statements = arena_push_array(&statements, &perm_arena);
    global_scope->num_statements = arrayCount(&statements);
    arrayFree(&statements);

    if (found_error) {
      jai_log(ERROR, "Exiting due to previous errors\n");
      return 1;
    }

    /** Do type inference and build scope tree **/

    doTypeInferenceForScope(global_scope);

    if (found_error) {
      jai_log(ERROR, "Exiting due to previous errors\n");
      return 1;
    }

    /** Compile to C **/

    {
      DynArray mains = getFunctionDeclarations("main", global_scope);
      if (arrayCount(&mains) == 1) {
        FunctionDeclarationAST* jai_main = *(FunctionDeclarationAST**) arrayGet(&mains, 0);
        if (jai_main->num_args == 0) {
          /* TODO: check signature of main */
          FILE* header = tmpfile();
          FILE* body = tmpfile();
          FILE* tail = tmpfile();
          if (body && header && tail) {
            int i,j;

            /* some builtin types */
            /* TODO: make to work on all systems */
            fprintf(header,
              "#include <stddef.h>\n\n"
              "#define ARRAY_SIZE(a) (sizeof(a)/sizeof(*a))\n"
              "typedef char T_i8;\n"
              "typedef unsigned char T_u8;\n"
              "typedef short T_i16;\n"
              "typedef unsigned short T_u16;\n"
              "typedef int T_i32;\n"
              "typedef unsigned int T_u32;\n"
              "typedef long long T_i64;\n"
              "typedef unsigned long long T_u64;\n"
              "typedef float T_f32;\n"
              "typedef double T_f64;\n"
              "typedef float T_float;\n"
              "typedef int T_int;\n"
              "typedef size_t T_usize;\n"
              "typedef void T_void;\n\n"
            );
            fprintf(header,
              "void* memset( void* dest, int ch, size_t count );\n\n"
              "typedef struct Slice {size_t length; void* data;} Slice;\n\n"
              "static inline Slice static_array_into_array(void* data, int length) {Slice s; s.data = data; s.length = length; return s;}\n\n"
            );

            /* compile all structs in global scope */
            /* TODO: compile all structs and functions in all scopes */
            {
              DynArray compiled_structs = array_create(16, sizeof(StructType*));

              /* write types */
              for (i = 0; i < global_scope->num_statements; ++i) {
                StatementAST* stmt = global_scope->statements[i];
                if (stmt->type == STRUCT_DECLARATION_STMT) {
                  StructType* str = &((StructDeclarationAST*) stmt)->str;
                  compile_type_to_c(header, body, str, &compiled_structs);
                }
              }

              arrayFree(&compiled_structs);
              fprintf(header, "\n");
            }

            /* compile all functions */
            for (i = 0; i < global_scope->num_statements; ++i) {
              StatementAST* stmt = global_scope->statements[i];
              if (stmt->type == FUNCTION_DECLARATION_STMT) {
                FunctionDeclarationAST* fun = (FunctionDeclarationAST*) stmt;

                if (fun->is_foreign) {

                  print(header, "$T %s(", fun->return_type, fun->name);

                  for (j = 0; j < fun->num_args; ++j) {
                    ArgumentAST* arg = fun->args + j;
                    print(header, "$T v_%s", arg->type, arg->name);
                    if (j != fun->num_args - 1) {
                      fprintf(header, ", ");
                    }
                  }
                  fprintf(header, ");\n");

                }

                else {

                  print(header, "$T $f(", fun->return_type, fun);
                  print(body, "$T $f(", fun->return_type, fun);

                  for (j = 0; j < fun->num_args; ++j) {
                    ArgumentAST* arg = fun->args + j;
                    print(header, "$T v_%s", arg->type, arg->name);
                    print(body, "$T v_%s", arg->type, arg->name);
                    if (j != fun->num_args - 1) {
                      fprintf(header, ", ");
                      fprintf(body, ", ");
                    }
                  }
                  fprintf(header, ");\n");
                  fprintf(body, ") ");

                  compile_block_to_c(&fun->body, body);
                  fprintf(body, "\n");
                }
              }
            }

            /* TODO: check signature of main */
            print(tail, "int main(int argc, const char* argv[]) {\n\t$f();\n};\n", jai_main);
            arrayFree(&mains);

            {
              char buf[256];
              FILE* output = fopen("/tmp/output.c", "w");

              if (output) {

                /* write header */
                fprintf(output, "\n\n/*** HEADER ***/\n\n");
                rewind(header);
                while (1) {
                  int read = fread(buf, 1, 256, header);
                  fwrite(buf, read, 1, output);
                  if (feof(header)) break;
                }
                fclose(header);

                /* write body */
                fprintf(output, "\n\n/*** DEFINITIONS ***/\n\n");
                rewind(body);
                while (1) {
                  int read = fread(buf, 1, 256, body);
                  fwrite(buf, read, 1, output);
                  if (feof(body)) break;
                }
                fclose(body);

                /* write tail */
                rewind(tail);
                while (1) {
                  int read = fread(buf, 1, 256, tail);
                  fwrite(buf, read, 1, output);
                  if (feof(tail)) break;
                }
                fclose(tail);
              } else {
                jai_log(ERROR, "Could not open output file '/tmp/output.c'\n");
              }

            }
          } else {jai_log(ERROR, "Failed to open temp files\n"); }
        } else {jai_log_at(ERROR, jai_main->body.stmt.pos, "main must not take any arguments");}
      } else if (arrayCount(&mains) > 1) {jai_log(ERROR, "Only one main can be defined"); }
      else {jai_log(ERROR, "No main function declared\n"); }
    }
  }
  return 0;
}
