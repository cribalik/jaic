#define DEBUG
#include "terminal.c"
#include "memarena.c"
#include "common.c"
#include "array.c"
#include "utils.c"
#include "string.c"
#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#ifdef LINUX
  #include <unistd.h>
#endif

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
} Token;

/* values set by the tokenizer */
global char* identifierStr;
global double floatVal;
global int intVal;

global bool found_error = false;

/* the input source file */
global FILE* file;

#define MAIN_MEMORY_SIZE 64*1024*1024

global MemArena perm_arena;

#define pushPosition \
  FilePos _position = prev_pos; \
  fgetpos(file, &_position.fpos); \
  int _num_chars_last_read = num_chars_last_read; \
  logDebugInfo("Pushing %s:%i:%i %c\n", _position.file, _position.line, _position.column-1, curr_char);

#define popPosition \
  logDebugInfo("Jumping back to %s:%i:%i %c\n", prev_pos.file, prev_pos.line, prev_pos.column-1, curr_char); \
  fsetpos(file, &_position.fpos); \
  fseek(file, -_num_chars_last_read - 1, SEEK_CUR); \
  next_pos = _position; \
  curr_char = ' '; \
  eatToken();

#define pushState \
  void* _arena_current = arenaGetCurrent(perm_arena);
#define popState \
  arenaPopTo(&perm_arena, _arena_current);

typedef struct {
  char* file;
  int line;
  int column;
  fpos_t fpos;
} FilePos;
global FilePos prev_pos = {.line = 1, .column = 0};
global FilePos next_pos = {.line = 1, .column = 0};

static int binopPrecedence(char op) {
  switch (op) {
    case '-': return 10;
    case '+': return 10;
    case '*': return 20;
    case '/': return 20;
    default: return 0;
  }
}


/** Tokenizer stuff **/

static int num_chars_read = 0;
static char getChar(FILE* file) {
  char c = getc(file);
  /* logDebugInfo("%i: %c\n", num_chars_read, c); */
  ++num_chars_read;
  ++next_pos.column;
  if (c == '\n') {
    ++next_pos.line;
    next_pos.column = 0;
  }
  return c;
}

static int curr_char = ' ';
static void eatWhitespace() {
  while (isspace(curr_char)) curr_char = getChar(file);
}
static int gettok() {
  #define BUFFER_SIZE 512
  static char buf[BUFFER_SIZE];

  while (isspace(curr_char)) {
    curr_char = getChar(file);
  }

  // check for comments
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

  // identifier
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
      } else {
        return TOK_IDENTIFIER;
      }
    }
  }

  // number literal
  else if (isdigit(curr_char)) {
    int i;
    for (i = 0; i < BUFFER_SIZE && isdigit(curr_char); ++i) {
      buf[i] = curr_char;
      curr_char = getChar(file);
    }

    if (i == BUFFER_SIZE) {
      logErrorAt(prev_pos, "Reached max size for int\n");
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
          logErrorAt(prev_pos, "Reached max size for float number\n");
        }
        // TODO: check for suffixes here e.g. 30.0f
        else {
          buf[i] = '\0';
          floatVal = strtod(buf, NULL);
          return TOK_FLOAT;
        }
      }
      else if (curr_char == '.') {
        fseek(file, -1, SEEK_CUR);
        buf[i-1] = '\0';
        intVal = atoi(buf);
        return TOK_INT;
      } else {
        logErrorAt(prev_pos, "Unexpected character '%c', expected floating point number or '..'\n", curr_char);
      }
    }
    // TODO: check for suffixes here e.g. 10u32
    else {
      buf[i] = '\0';
      intVal = atoi(buf);
      return TOK_INT;
    }
  }

  // eof
  else if (curr_char == EOF) {
    return TOK_EOF;
  }

  // arrow
  else if (curr_char == '-') {
    curr_char = getChar(file);
    if (curr_char == '>') {
      curr_char = getChar(file);
      return TOK_ARROW;
    } else {
      return '-';
    }
  }

  // double dots
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

static int token;
static char* print_token() {
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

static int num_chars_last_read = 0;
static void eatToken() {
  // logDebugInfo("before gettok: line: %i column: %i\n", prev_pos.line, prev_pos.column);
  int i = num_chars_read;
  token = gettok();
  eatWhitespace();
  num_chars_last_read = num_chars_read - i;
  // logDebugInfo("last read: %i token: %s\n", num_chars_last_read, print_token());
}

static void goto_matching_brace() {
  FilePos pos = prev_pos;
  int d = 0;
  while (d) {
    eatToken();
    if (token == '{') ++d;
    else if (token == '}') --d;
    else if (token == EOF) {
      logErrorAt(pos, "No matching brace found\n");
      return;
    }
  }
  eatToken();
}


/** AST Definitions **/

typedef struct ExpressionAST ExpressionAST;

/* Statements */

typedef enum StatementType {
  UNKNOWN_STMT,
  STRUCT_DECLARATION_STMT,
  VARIABLE_DECLARATION_STMT,
  FUNCTION_DECLARATION_STMT,
  ASSIGNMENT_STMT,
  EXPRESSION_STMT,
  COMPOUND_STMT, // new scope
  LOOP_STMT,
  RETURN_STMT,
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
  // TODO: optimize variable lookups
} CompoundStatementAST;

typedef struct ReturnAST {
  StatementAST stmt;
  ExpressionAST* value;
} ReturnAST;

/* Type */

typedef enum Type {
  UNKNOWN,
  /* builtin types */
  VOID,
  INT,
  FLOAT,
  U8,

  /* struct */
  STRUCT,

  /* function */
  FUNCTION
} Type;

typedef struct TypeAST {
  StatementAST stmt;
  char* name;
  Type type;
  FilePos pos;
} TypeAST;

global TypeAST builtin_types[] = {
  {},
  {.type = VOID, .name = "void"},
  {.type = INT, .name = "int"},
  {.type = FLOAT, .name = "float"},
  {.type = U8, .name = "u8"},
};

typedef struct MemberAST {
  char* name;
  char* type_name;
  TypeAST* type;
  FilePos pos;
} MemberAST;
typedef struct StructAST {
  TypeAST type;
  int num_members;
  MemberAST* members;
  bool has_initializer;
} StructAST;

/* Expressions */

typedef enum ExpressionType {
  FLOAT_EXPR,
  INT_EXPR,
  CALL_EXPR,
  VARIABLE_GET_EXPR,
  MEMBER_ACCESS_EXPR,
  STRUCT_INIT_EXPR,
  BINOP_EXPR,
} ExpressionType;

typedef struct ExpressionAST {
  StatementAST stmt;
  ExpressionType expr_type;
  TypeAST* type;
} ExpressionAST;

typedef struct MemberAccessAST {
  ExpressionAST expr;
  ExpressionAST* base; // in a.x, this is a
  char* name; // in a.x, this is x
} MemberAccessAST;

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

typedef struct FloatAST {
  ExpressionAST expr;
  float value;
} FloatAST;

typedef struct IntAST {
  ExpressionAST expr;
  int value;
} IntAST;

struct FunctionDeclarationAST;
typedef struct CallAST {
  ExpressionAST expr;
  char* name;
  int num_args;
  struct FunctionDeclarationAST* fun;
  ExpressionAST** args;
} CallAST;

typedef struct BinaryExpressionAST {
  ExpressionAST expr;
  char operator;
  ExpressionAST* lhs;
  ExpressionAST* rhs;
} BinaryExpressionAST;

typedef struct VariableDeclarationAST {
  StatementAST stmt;
  char* name;
  char* type_name;
  TypeAST* type;
  ExpressionAST* value;
} VariableDeclarationAST;

typedef struct LoopAST {
  CompoundStatementAST body;
  VariableDeclarationAST it;
  char* iterator_name;
  char* index_name;
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
  char* return_type_name;
  TypeAST* return_type;
  FilePos pos;
  bool is_foreign;
} FunctionDeclarationAST;

typedef struct AssignmentAST {
  StatementAST stmt;
  ExpressionAST* lhs;
  ExpressionAST* rhs;
} AssignmentAST;

static int test() {
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

static StatementAST* parseStatement();

static int parseBlock(CompoundStatementAST* result, DynArray* prefix) {
  if (token != '{') {
    return 1;
  }
  eatToken();
  DynArray statements;
  DynArray* array;
  if (prefix) {
    array = prefix;
  } else {
    array = &statements;
    arrayInit(array, 32, sizeof(StatementAST*));
  }
  while (1) {
    if (token == '}') {
      eatToken();
      break;
    }
    StatementAST* stmt = parseStatement();
    if (stmt) {
      arrayPushVal(array, &stmt);
    } else {
      logErrorAt(prev_pos, "Failed to parse statement\n");
      arrayFree(array);
      return 1;
    }
  }
  result->num_statements = arrayCount(array);
  result->statements = pushArrayToArena(array, &perm_arena);
  result->stmt.type = COMPOUND_STMT;
  arrayFree(array);
  return 0;
}

static FunctionDeclarationAST* parseFunctionDeclaration(char* name, bool try) {
  local_persist int functionID = 0;
  FunctionDeclarationAST* result = arenaPush(&perm_arena, sizeof(FunctionDeclarationAST));
  result->name = name;
  result->body.stmt.pos = prev_pos;
  result->body.stmt.type = FUNCTION_DECLARATION_STMT;
  if (token != '(') {
    if (!try) {
      logErrorAt(prev_pos, "token %s not start of a function prototype\n", print_token(token));
    }
    return 0;
  }

  // arguments
  eatToken();
  DynArray /* ArgumentAST* */ args;
  arrayInit(&args, 0, sizeof(ArgumentAST));
  while (1) {

    if (token == ')') {
      eatToken();
      break;
    }

    ArgumentAST* arg = arrayPush(&args);
    arg->stmt.pos = prev_pos;

    if (token != TOK_IDENTIFIER) {
      if (!try) {
        logErrorAt(prev_pos, "Identifier expected in parameter list, found %c\n", token);
      }
      arrayFree(&args);
      return 0;
    }

    arg->name = arenaPushString(&perm_arena, identifierStr);
    eatToken();
    logDebugInfo("Found parameter %s\n", arg->name);

    if (token != ':') {
      if (!try) {
        logErrorAt(prev_pos, "No type found after parameter name (did you forget a ':'?\n");
      }
      arrayFree(&args);
      return 0;
    }
    eatToken();

    if (token != TOK_IDENTIFIER) {
      if (!try) {
        logErrorAt(prev_pos, "Typename expected after ':'\n");
      }
      arrayFree(&args);
      return 0;
    }

    arg->type_name = arenaPushString(&perm_arena, identifierStr);
    eatToken();
    logDebugInfo(" - with type %s\n", arg->type_name);

    // TODO: default parameters

    if (token == ',') {
      eatToken();
    }
    else if (token == ')') {
      eatToken();
      break;
    }
    else {
      logDebugInfo("Expected ',' or ')' after parameter\n");
      arrayFree(&args);
      return 0;
    }
  }
  result->num_args = arrayCount(&args);
  result->args = pushArrayToArena(&args, &perm_arena);
  arrayFree(&args);

  // return value
  if (token == TOK_ARROW) {
    eatToken();
    if (token != TOK_IDENTIFIER) {
      logErrorAt(prev_pos, "Expected return type, got %s\n", print_token());
      logDebugInfo("no type identifier after arrow");
      arrayFree(&args);
      return 0;
    }
    result->return_type_name = arenaPushString(&perm_arena, identifierStr);
    eatToken();
  }
  else {
    result->return_type_name = "void";
  }

  if (token == '#') {
    eatToken();
    if (token == TOK_IDENTIFIER && strcmp(identifierStr, "foreign") == 0) {
      eatToken();
      result->is_foreign = true;
    } else {
      logErrorAt(prev_pos, "Invalid compiler directive, did you mean to use 'foreign' ?\n");
      return 0;
    }
  }
  else if (token == '{') {
    eatToken();
    DynArray statements;
    arrayInit(&statements, 32, sizeof(StatementAST*));
    while (1) {
      if (token == '}') {
        eatToken();
        break;
      }
      StatementAST* stmt = parseStatement();
      if (stmt) {
        arrayPushVal(&statements, &stmt);
      } else {
        logErrorAt(prev_pos, "Failed to parse statement in %s\n", result->name);
        // escape out of body
        goto_matching_brace();
        return 0;
      }
    }
    result->body.num_statements = arrayCount(&statements);
    result->body.statements = pushArrayToArena(&statements, &perm_arena);
    arrayFree(&statements);

  } else {
    logErrorAt(prev_pos, "No '{' or '#foreign' found after function parameter list\n");
    return 0;
  }

  result->id = functionID++;

  return result;
}

static ExpressionAST* parseExpression();
static ExpressionAST* parsePrimitive() {
  FilePos pos = prev_pos;
  switch (token) {
    case TOK_FLOAT: {
      FloatAST* ast = arenaPush(&perm_arena, sizeof(FloatAST));
      ast->expr.expr_type = FLOAT_EXPR;
      ast->expr.stmt.pos = pos;
      ast->value = floatVal;
      eatToken();
      logDebugInfo("Found a float literal with value %f\n", ast->value);
      return &ast->expr;
    } break;

    case TOK_INT: {
      IntAST* ast = arenaPush(&perm_arena, sizeof(IntAST));
      ast->expr.expr_type = INT_EXPR;
      ast->expr.stmt.pos = pos;
      ast->value = intVal;
      eatToken();
      logDebugInfo("Found an int literal with value %i\n", ast->value);
      return &ast->expr;
    } break;

    case TOK_IDENTIFIER: {
      ExpressionAST* result;
      byte* start = arenaGetCurrent(perm_arena);
      char* name = arenaPushString(&perm_arena, identifierStr);
      eatToken();

      // function call?
      if (token == '(') {

        // function call
        eatToken();
        DynArray args;
        arrayInit(&args, 0, sizeof(ExpressionAST*));
        ExpressionAST* expr = 0;

        // get args arguments
        while (1) {
          if (token == ')') {
            eatToken();
            break;
          }

          expr = parseExpression();
          if (!expr) {
            logErrorAt(prev_pos, "Invalid expression inside function call to '%s'\n", name);
            arenaPopTo(&perm_arena, start);
            arrayFree(&args);
            return 0;
          }

          arrayPushVal(&args, &expr);

          if (token == ')') {
            eatToken();
            break;
          }
          if (token == ',') {
            eatToken();
          } else {
            logErrorAt(prev_pos, "Expected ',' between function input parameters\n");
          }
        }

        CallAST* call = arenaPush(&perm_arena, sizeof(CallAST));
        call->expr.stmt.pos = pos;
        call->expr.expr_type = CALL_EXPR;
        call->name = name;
        call->num_args = arrayCount(&args);
        if (call->num_args > 0) {
          call->args = pushArrayToArena(&args, &perm_arena);
        }
        arrayFree(&args);

        logDebugInfo("Found function call to '%s' with %i inputs\n", call->name, call->num_args);
        result = &call->expr;
      }
      else {
        VariableGetAST* ast = arenaPush(&perm_arena, sizeof(VariableGetAST));
        ast->expr.expr_type = VARIABLE_GET_EXPR;
        ast->expr.stmt.pos = pos;
        ast->name = name;
        logDebugInfo("Found a variable get with name '%s'\n", name);
        result = &ast->expr;
      }

      // member access?
      while (token == '.') {
        eatToken();
        if (token == TOK_IDENTIFIER) {
          MemberAccessAST* ast = arenaPush(&perm_arena, sizeof(MemberAccessAST));
          ast->expr.expr_type = MEMBER_ACCESS_EXPR;
          ast->expr.stmt.pos = prev_pos;
          ast->base = result;
          ast->name = arenaPushString(&perm_arena, identifierStr);
          result = &ast->expr;
          eatToken();
        } else {
          logErrorAt(prev_pos, "Identifier expected after '.', instead got %s\n", print_token());
          return 0;
        }
      }
      return result;
    } break;

    case '(': {
      eatToken();
      ExpressionAST* result = parseExpression();
      if (!result || token != ')') {
        return 0;
      }
      eatToken();
      return result;
    } break;

    case '{': {
      eatToken();
      StructInitializationAST* ast = arenaPush(&perm_arena, sizeof(StructInitializationAST));
      ast->expr.expr_type = STRUCT_INIT_EXPR;
      ast->expr.stmt.pos = pos;
      if (token != '}') {
        DynArray members;
        arrayInit(&members, 4, sizeof(MemberInitializationAST));
        while (1) {
          MemberInitializationAST* member = arrayPush(&members);
          bool success = false;
          if (token == TOK_IDENTIFIER) {
            member->name = arenaPushString(&perm_arena, identifierStr);
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
                  logErrorAt(prev_pos, "Expected ',' or '}', but found %s\n", print_token());
                }
              } else {logErrorAt(prev_pos, "Failed to parse expression\n");}
            } else {logErrorAt(prev_pos, "Expected '=' after member name, found %s\n", print_token());}
          } else {logErrorAt(prev_pos, "Expected identifier, instead found %s\n", print_token());}

          if (!success) {
            while (token != '}') {
              eatToken();
            }
            eatToken();
            ast = 0;
            break;
          }
          //we failed, eat everything to matching brace
        }
        if (ast) {
          ast->members = pushArrayToArena(&members, &perm_arena);
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
      logErrorAt(prev_pos, "Failed to parse expression\n");
      return 0;
    } break;
  }
}

static ExpressionAST* parseExpression() {
  ExpressionAST* expr = parsePrimitive();
  if (expr) {
    if (binopPrecedence(token)) {
      BinaryExpressionAST* bin = arenaPush(&perm_arena, sizeof(BinaryExpressionAST));
      bin->expr.expr_type = BINOP_EXPR;
      bin->operator = token;
      bin->lhs = expr;
      logDebugInfoAt(prev_pos, "Found binary expression with operator %s\n", print_token());
      eatToken();
      bin->rhs = parseExpression();
      return &bin->expr;
      // TODO: operator precedence
    }
  }
  return expr;
}

static StatementAST* _parseStatement() {
  FilePos pos_of_identifier = prev_pos;

  if (token == TOK_LOOP) {
    LoopAST* loop = arenaPush(&perm_arena, sizeof(LoopAST));
    loop->body.stmt.type = LOOP_STMT;
    loop->body.stmt.pos = prev_pos;
    eatToken();

    if (token == TOK_IDENTIFIER) {
      loop->iterator_name = arenaPushString(&perm_arena, identifierStr);
      eatToken();

      if (token == ',') {
        eatToken();
        if (token == TOK_IDENTIFIER) {
          loop->index_name = arenaPushString(&perm_arena, identifierStr);
          eatToken();
        }
      }

      if (token == ':') {
        eatToken();

        ExpressionAST* from = parseExpression();
        if (from) {
          loop->from = from;
          if (token == TOK_ARROW) {
            eatToken();
            loop->to = parseExpression();
            if (loop->to) {
          // parse block
              if (token == '{') {
                eatToken();
                DynArray statements;
                arrayInit(&statements, 16, sizeof(StatementAST*));
                while (1) {
                  if (token == '}') {
                    eatToken();
                    break;
                  }
                  StatementAST* stmt = parseStatement();
                  if (stmt) {
                    arrayPushVal(&statements, &stmt);
                  } else {
                    logErrorAt(prev_pos, "Failed to parse statement in loop body\n");
                  }
                }
                loop->body.num_statements = arrayCount(&statements);
                loop->body.statements = pushArrayToArena(&statements, &perm_arena);
                arrayFree(&statements);
                return &loop->body.stmt;
              } else {
                logErrorAt(prev_pos, "Expected block after loop. Did you forget '{'?\n");
              }
              return 0;
            } else {logErrorAt(prev_pos, "Could not parse expression at end of loop\n"); }
          } else {logErrorAt(prev_pos, "Unexpected token %s. Did you mean to write '->'?\n", print_token()); }
        } else {logErrorAt(prev_pos, "Could not parse expression at start of loop\n"); }
      } else {logErrorAt(prev_pos, "Expected ':'\n");}
    } else {logErrorAt(prev_pos, "Expected a name for element in list.. For example 'for x : list {...}'\n");}
    return 0;
  }

  else if (token == TOK_RETURN) {
    eatToken();
    ReturnAST* ast = arenaPush(&perm_arena, sizeof(ReturnAST));
    ast->stmt.type = RETURN_STMT;
    ast->stmt.pos = prev_pos;
    ast->value = parseExpression();
    if (ast->value) {
      return &ast->stmt;
    } else {logErrorAt(ast->stmt.pos, "Failed to parse return statement\n");}
  }

  else {
    ExpressionAST* expr = parseExpression();
    if (expr) {
      /* Declaration? */
      if (expr->expr_type == VARIABLE_GET_EXPR && token == ':') {
        VariableGetAST* lhs = (VariableGetAST*) expr;
        char* type = 0;
        eatToken();

        if (token == TOK_IDENTIFIER) {
          type = arenaPushString(&perm_arena, identifierStr);
          logDebugInfo("With declared type %s\n", type);
          eatToken();
          if (token != '=') {
            VariableDeclarationAST* var = arenaPush(&perm_arena, sizeof(VariableDeclarationAST));
            var->stmt.type = VARIABLE_DECLARATION_STMT;
            var->name = lhs->name;
            var->type_name = type;
            logDebugInfo("Found a variable declaration for %s with type %s and no value\n", var->name, var->type_name);
            // TODO: default initialization of variable here!
            return &var->stmt;
          }
        }

        if (token == '=') {
          // check for function, struct or variable definition
          eatToken();
          pushPosition;
          pushState;

          // struct?
          if (token == TOK_STRUCT) {
            eatToken();
            if (token == '{') {
              eatToken();

              StructAST* str = arenaPush(&perm_arena, sizeof(StructAST));
              str->type.stmt.type = STRUCT_DECLARATION_STMT;
              str->type.pos = pos_of_identifier;
              str->type.type = STRUCT;
              str->type.name = lhs->name;
              DynArray members; /* MemberAST */
              arrayInit(&members, 4, sizeof(MemberAST));
              bool success = false;
              while (1) {
                MemberAST* member = arrayPush(&members);
                if (token == TOK_IDENTIFIER) {
                  member->name = arenaPushString(&perm_arena, identifierStr);
                  member->pos = prev_pos;
                  eatToken();
                  if (token == ':') {
                    eatToken();
                    // TODO: parse default value
                    if (token == TOK_IDENTIFIER) {
                      member->type_name = arenaPushString(&perm_arena, identifierStr);
                      eatToken();
                      if (token == '}') {
                        eatToken();
                        success = true;
                        break;
                      } else {
                        continue;
                      }
                    } else { logErrorAt(prev_pos, "No type after member\n"); }
                  } else { logErrorAt(prev_pos, "No type after member. Did you forget a ':'?\n"); }
                } else { logErrorAt(prev_pos, "Expected member name, instead found %s\n", print_token()); }
                while (token != '}') {
                  eatToken();
                }
                eatToken();
                return 0;
              }

              if (success) {
                if (!type) {
                  str->num_members = arrayCount(&members);
                  str->members = pushArrayToArena(&members, &perm_arena);
                  arrayFree(&members);
                  return &str->type.stmt;
                } else { logErrorAt(prev_pos, "You should not define a type of a type declaration (drop the type after ':')\n"); }
              }
            } else {
              logErrorAt(pos_of_identifier, "Expected '{' after struct keyword\n");
            }
            return 0;
          }

          // TODO: switch function and expression checks?
          // function?
          {
            FunctionDeclarationAST* fun = parseFunctionDeclaration(lhs->name, true);
            if (fun) {
              fun->pos = pos_of_identifier;
              logDebugInfoAt(pos_of_identifier, "Function declaration for '%s' found\n", fun->name);
              if (!type) {
                logDebugInfo("Found a function definition with name %s, and %i arguments\n", fun->name, fun->num_args);
                return &fun->body.stmt;
              } else {
                logErrorAt(pos_of_identifier, "Function declaration should not have type. Write ':=' instead\n");
                return 0;
              }
            }
          }

          // expression?
          popPosition;
          popState;
          {
            ExpressionAST* value = parseExpression();
            if (value) {
              VariableDeclarationAST* var = arenaPush(&perm_arena, sizeof(VariableDeclarationAST));
              var->stmt.type = VARIABLE_DECLARATION_STMT;
              var->stmt.pos = pos_of_identifier;
              var->name = lhs->name;
              var->type_name = type ? type : 0;
              var->value = value;
              var->type = 0;
              logDebugInfo("Found a variable declaration for %s with type %s and a value of type %i\n", var->name, var->type_name, var->value->expr_type);
              return &var->stmt;
            }
          }

          return 0;
        }

        else {
          logErrorAt(expr->stmt.pos, "Unexpected token %s, expected type name, '=', or ';'\n", print_token());
          return 0;
        }
      }

      // assignment?
      if (token == '=') {
        eatToken();
        AssignmentAST* ass = arenaPush(&perm_arena, sizeof(AssignmentAST));
        ass->stmt.type = ASSIGNMENT_STMT;
        ass->stmt.pos = pos_of_identifier;
        ass->lhs = expr;
        ass->rhs = parseExpression();
        if (ass->rhs) {
          return &ass->stmt;
        }
        else {
          logErrorAt(prev_pos, "Failed to parse right hand side of assignment, expected expression\n");
          return 0;
        }
      }

      // just an expression
      else {
        expr->stmt.type = EXPRESSION_STMT;
        return &expr->stmt;
      }

      logErrorAt(prev_pos, "Unexpected token %s after ':'\n", print_token());
      return 0;
    } else {logErrorAt(pos_of_identifier, "Failed to parse statement\n"); }
  }

  return 0;
}
static StatementAST* parseStatement() {
  while (1) {
    if (token == EOF) {
      return 0;
    } if (token != ';') {
      break;
    }
    eatToken();
  }
  StatementAST* result = _parseStatement();
  while (1) {
    if (token == EOF || token != ';') {
      break;
    }
    eatToken();
  }
  return result;
}


/** 2nd pass: type inference and scope checks **/

static void evaluateTypeOfStruct(StructAST* str, CompoundStatementAST* scope);

static TypeAST* getTypeDeclaration(char* name, CompoundStatementAST* scope) {
  // add the builtin to the global scope
  for (int i = 1; i < ARRSIZE(builtin_types); ++i) {
    if (strcmp(builtin_types[i].name, name) == 0) {
      return builtin_types + i;
    }
  }

  // check builtin types
  while (scope) {
    for (int i = 0; i < scope->num_statements; ++i) {
      StatementAST* stmt = scope->statements[i];
      if (stmt->type == STRUCT_DECLARATION_STMT) {
        StructAST* str = (StructAST*) stmt;
        if (strcmp(str->type.name, name) == 0) {
          evaluateTypeOfStruct(str, scope);
          return &str->type;
        }
      }
    }
    scope = scope->parent_scope;
  }
  return 0;
};

static DynArray getFunctionDeclarations(char* name, CompoundStatementAST* scope) {
  // TODO: builtin functions?
  DynArray funs;
  arrayInit(&funs, 2, sizeof(FunctionDeclarationAST*));
  while (scope) {
    for (int i = 0; i < scope->num_statements; ++i) {
      StatementAST* stmt = scope->statements[i];
      if (stmt->type == FUNCTION_DECLARATION_STMT) {
        FunctionDeclarationAST* fun = (FunctionDeclarationAST*) stmt;
        if (strcmp(fun->name, name) == 0) {
          arrayPushVal(&funs, &fun);
        }
      }
    }
    scope = scope->parent_scope;
  }
  return funs;
}

static void evaluateTypeOfVariableDeclaration(VariableDeclarationAST* name, CompoundStatementAST* scope);

static VariableDeclarationAST it_index_declaration = {
  .name = "it_index",
  .type_name = "int",
  .type = &builtin_types[INT]
};
static VariableDeclarationAST* getVariableDeclaration(char* name, CompoundStatementAST* scope) {
  while (scope) {
    /* check loop implicit variables */
    if (scope->stmt.type == LOOP_STMT) {
      LoopAST* loop = (LoopAST*) scope;
      if (loop->iterator_name && strcmp(name, loop->iterator_name) == 0) {
        return &it_index_declaration;
      } else if (loop->index_name && strcmp(name, loop->index_name) == 0) {
        return &loop->it;
      }
    }
    /* TODO: check only statements before this statement */
    /* check local variables */
    for (int i = 0; i < scope->num_statements; ++i) {
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
      for (int i = 0; i < fun->num_args; ++i) {
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

static void _evaluateTypeOfStruct(StructAST* str, DynArray* parents, CompoundStatementAST* scope) {
  for (int i = 0; i < str->num_members; ++i) {
    if (!str->members[i].type) {
      str->members[i].type = getTypeDeclaration(str->members[i].type_name, scope);
      if (!str->members[i].type) {
        logErrorAt(str->members[i].pos, "Could not find type %s\n", str->members[i].type_name);
        return;
      }
      if (str->members[i].type->type == STRUCT) {
        for (char **parent = arrayBegin(parents), **end = arrayEnd(parents); parent != end; ++parent) {
          if (strcmp(*parent, str->members[i].type->name) == 0) {
            logErrorAt(str->members[i].pos, "Type %s cannot contain itself. Did you mean to use a pointer?\n", *parent);
            exit(1);
          }
        }
        arrayPushVal(parents, &str->members[i].type->name);
        _evaluateTypeOfStruct((StructAST*) str->members[i].type, parents, scope);
        arrayPop(parents);
      }
    }
  }
}
static void evaluateTypeOfStruct(StructAST* str, CompoundStatementAST* scope) {
  DynArray parents;
  arrayInit(&parents, 1, sizeof(char*));
  arrayPushVal(&parents, &str->type.name);
  _evaluateTypeOfStruct(str, &parents, scope);
  arrayFree(&parents);
}

static void evaluateTypeOfExpression(ExpressionAST* expr, TypeAST* evidence, CompoundStatementAST* scope);
static void evaluateTypeOfVariableDeclaration(VariableDeclarationAST* var, CompoundStatementAST* scope) {
  if (!var || var->type) return;

  TypeAST* declared_type = 0;

  if (var->type_name) {
    declared_type = getTypeDeclaration(var->type_name, scope);
    if (!declared_type) {
      logErrorAt(var->stmt.pos, "Declared type '%s' for '%s' doesn't exist\n", var->type_name, var->name);
      var->type = 0;
      return;
    }
    var->type = declared_type;
  }

    /* TODO: check if is_declaration, if not, we can use the type from the declaration! */

  if (var->value) {
    evaluateTypeOfExpression(var->value, var->type, scope);
    if (!var->value->type) {
      logErrorAt(var->stmt.pos, "Couldn't infer type for %s\n", var->name);
      var->type = 0;
      return;
    }
    var->type = var->value->type;
  }

    // check that infered expression type and stated type match
  if (var->value && var->value->type && declared_type && var->value->type != declared_type) {
    logErrorAt(prev_pos, "Inferred type of '%s': '%s' differs from declared type '%s'\n", var->name, var->value->type->name, declared_type->name);
    var->type = 0;
    return;
  }

  return;
}

static void evaluateTypeOfFunction(FunctionDeclarationAST* fun, CompoundStatementAST* scope) {
  if (!fun || fun->return_type) {
    return;
  }
  assert(fun->return_type_name); // return_type should have been set to void

  for (int i = 0; i < fun->num_args; ++i) {
    fun->args[i].type = getTypeDeclaration(fun->args[i].type_name, scope);
    if (!fun->args[i].type) {
      logErrorAt(fun->args[i].stmt.pos, "Could not find type '%s'\n", fun->args[i].type_name);
    }
  }

  fun->return_type = getTypeDeclaration(fun->return_type_name, scope);
  if (!fun->return_type) {
    logErrorAt(fun->pos, "Could not find definition of return type '%s' of '%s'\n", fun->return_type_name, fun->name);
  }
}

static void evaluateTypeOfExpression(ExpressionAST* expr, TypeAST* evidence, CompoundStatementAST* scope) {
  if (expr->type) {
    return;
  }

  switch (expr->expr_type) {

    case FLOAT_EXPR: {
      expr->type = &builtin_types[FLOAT];
    } break;

    case INT_EXPR: {
      expr->type = &builtin_types[INT];
    } break;

    case CALL_EXPR: {
      CallAST* call = (CallAST*) expr;
      logDebugInfo("Inferring call to %s\n", call->name);
      DynArray funs = getFunctionDeclarations(call->name, scope);
      if (arrayCount(&funs)) {
        // eval functions and find best match
        int matches = 0;
        for (FunctionDeclarationAST** it = arrayBegin(&funs), **end = arrayEnd(&funs); it < end; ++it) {
          FunctionDeclarationAST* fun = *it;
          if (!fun->return_type && fun->return_type_name) {
            evaluateTypeOfFunction(fun, scope);
          }
          // TODO: implicit type conversions?
          bool match = true;
          if (call->num_args != fun->num_args) {
            match = false;
          } else {
            for (int i = 0; i < fun->num_args; ++i) {
              call->args[i]->type = 0;
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
          // TODO: print out actual matches, and not all overload
          logErrorAt(expr->stmt.pos, "Multiple matching overloads for %s.\n", call->name);
          logNote("Overloads are:\n");
          for (FunctionDeclarationAST** it = arrayBegin(&funs), **end = arrayEnd(&funs); it < end; ++it) {
            logNoteAt((*it)->pos, "\n");
          }
        } else if (matches == 0) {
          logErrorAt(expr->stmt.pos, "Could not find matching function overload for %s.\n", call->name);
          logNote("Alternatives are:\n");
          for (FunctionDeclarationAST** it = arrayBegin(&funs), **end = arrayEnd(&funs); it < end; ++it) {
            logNoteAt((*it)->pos, "\n");
          }
        }

      } else {
        logErrorAt(call->expr.stmt.pos, "Unknown function '%s'\n", call->name);
      }

      arrayFree(&funs);
      // for (FunctionDeclarationAST** it = arrayEnd(&funs), **end = arrayEnd(&funs); it < end; ++it) {
        // logErrorAt((*it)->pos, "Alternative\n");
      // }
    } break;

    case VARIABLE_GET_EXPR: {
      VariableGetAST* var_ref = (VariableGetAST*) expr;
      VariableDeclarationAST* var = getVariableDeclaration(var_ref->name, scope);
      if (!var) {
        logErrorAt(var_ref->expr.stmt.pos, "Use of undeclared variable '%s'", var_ref->name);
        return;
      }
      expr->type = var->type;
    } break;

    case MEMBER_ACCESS_EXPR: {
      MemberAccessAST* ast = (MemberAccessAST*) expr;
      evaluateTypeOfExpression(ast->base, 0, scope);
      if (ast->base->type) {
        if (ast->base->type->type == STRUCT) {

          // find member
          StructAST* str = (StructAST*) ast->base->type;
          MemberAST* match = 0;
          for (int i = 0; i < str->num_members; ++i) {
            MemberAST* member = str->members + i;
            if (strcmp(member->name, ast->name) == 0) {
              match = member;
              break;
            }
          }

          if (match) {
            assert(match->type);
            ast->expr.type = match->type;
          } else {logErrorAt(ast->expr.stmt.pos, "struct %s has no member %s\n", ast->base->type->name, ast->name);}
        } else {logErrorAt(ast->base->stmt.pos, "Can only access members of structs, %s is not a struct\n", ast->base->type->name);}
      }
    } break;


    case STRUCT_INIT_EXPR: {
      StructInitializationAST* ast = (StructInitializationAST*) expr;
      if (evidence) {
        if (evidence->type == STRUCT) {
          StructAST* target = (StructAST*) evidence;
          bool all_match = true;
          for (int i = 0; i < ast->num_members; ++i) {
            // TODO: make sure each member is only mentioned once
            bool name_match = false;
            for (int j = 0; j < target->num_members; ++j) {
              if (strcmp(ast->members[i].name, target->members[j].name) == 0) {
                name_match = true;
                ast->members[i].member = target->members + j;
              }
            }
            if (!name_match) {
              logErrorAt(expr->stmt.pos, "%s has no member %s\n", target->type.name, ast->members[i].name);
              all_match = false;
              break;
            }
            assert(ast->members[i].member->type);
            evaluateTypeOfExpression(ast->members[i].value, ast->members[i].member->type, scope);
            if (ast->members[i].value->type != ast->members[i].member->type) {
              logErrorAt(expr->stmt.pos, "The member '%s' in '%s' has type '%s', but the expression has type '%s'\n", ast->members[i].member->name, target->type.name, ast->members[i].member->type->name, ast->members[i].value->type->name);
              all_match = false;
              break;
            }
          }
          if (all_match) {
            expr->type = evidence;
          }
        } else {logErrorAt(expr->stmt.pos, "Cannot use struct initialization for non-struct type %s\n", evidence->name);}
      } else {logErrorAt(expr->stmt.pos, "Not enough type information top evaluate type of struct initialization\n");}
    } break;

    case BINOP_EXPR: {
      BinaryExpressionAST* ast = (BinaryExpressionAST*) expr;
      evaluateTypeOfExpression(ast->lhs, 0, scope);
      evaluateTypeOfExpression(ast->rhs, ast->lhs->type, scope);
      // TODO: check if operator overload exists
      if (ast->lhs->type == ast->rhs->type) {
        ast->expr.type = ast->lhs->type;
      } else {
        logErrorAt(ast->lhs->stmt.pos, "Type of left hand side '%s' does not match type of right hand side '%s'\n", ast->lhs->type->name, ast->rhs->type->name);
      }
    } break;
  }
}

static void doTypeInferenceForScope(CompoundStatementAST* scope) {
  // TODO: check for duplicates declarations within same scope

  for (int i = 0; i < scope->num_statements; ++i) {
    StatementAST* stmt = scope->statements[i];
    switch (stmt->type) {

      case STRUCT_DECLARATION_STMT: {
        StructAST* str = (StructAST*) stmt;
        evaluateTypeOfStruct(str, scope);
        logDebugInfo("Evaluated type of struct '%s'\n", str->type.name);
      } break;

      case VARIABLE_DECLARATION_STMT: {
        VariableDeclarationAST* var = (VariableDeclarationAST*) stmt;
        evaluateTypeOfVariableDeclaration(var, scope);
        if (var->type) {
          logDebugInfoAt(var->stmt.pos, "Evaluated type of '%s' to '%s'\n", var->name, var->type->name);
        } else {
          logErrorAt(var->stmt.pos, "Failed to evaluate type of '%s'\n", var->name);
        }
      } break;

      case FUNCTION_DECLARATION_STMT: {
        FunctionDeclarationAST* fun = (FunctionDeclarationAST*) stmt;
        fun->body.parent_scope = scope;
        evaluateTypeOfFunction(fun, scope);
        if (fun->body.num_statements) {
          doTypeInferenceForScope(&fun->body);
        }
        logDebugInfoAt(fun->pos, "Evaluated body of '%s'\n", fun->name);
      } break;

      case EXPRESSION_STMT: {
        ExpressionAST* expr = (ExpressionAST*) stmt;
        evaluateTypeOfExpression(expr, 0, scope);
        if (expr->type) {
          logDebugInfoAt(expr->stmt.pos, "Evaluated type of expression to %s\n", expr->type->name);
        } else {
          logErrorAt(expr->stmt.pos, "Failed to infer type of expression\n");
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
        // TODO: not int, but size_t
        evaluateTypeOfExpression(loop->from, &builtin_types[INT], scope);
        evaluateTypeOfExpression(loop->to, &builtin_types[INT], scope);
        if (!loop->from->type || !loop->to->type) {
          logErrorAt(loop->from->stmt.pos, "Loop indexes are not integers\n");
        }
        if (loop->from->type != loop->to->type) {
          logErrorAt(loop->from->stmt.pos, "Types of start index and stop index differ\n");
        }
        loop->it.type = loop->from->type;
        doTypeInferenceForScope(&loop->body);
      } break;

      case ASSIGNMENT_STMT: {
        AssignmentAST* ass = (AssignmentAST*) stmt;
        evaluateTypeOfExpression(ass->lhs, 0, scope);
        evaluateTypeOfExpression(ass->rhs, ass->lhs->type, scope);
        if (!ass->lhs->type) {
          logErrorAt(stmt->pos, "Could not infer type of left hand side of assignment\n");
        }
        else if (!ass->rhs->type) {
          logErrorAt(stmt->pos, "Could not infer type of right hand side of assignment\n");
        }
        else if (ass->lhs->type != ass->rhs->type) {
          logErrorAt(stmt->pos, "Left hand side and right hand side of assignment do not have same types!\n");
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
              logErrorAt(ret->stmt.pos, "Return type %s does not match function return type %s\n", ret->value->type->name, fun->return_type->name);
            }
          } else {logErrorAt(ret->stmt.pos, "Could not infer type of return value\n")};
        } else {logErrorAt(ret->stmt.pos, "Can only use return inside function\n");}
      } break;

      case UNKNOWN_STMT: {
        logDebugError("Unknown statement\n");
      } break;

    }

  }
}


/** Compile AST to C **/

static void print_function_mangle(FILE* file, FunctionDeclarationAST* fun) {
  fprintf(file, "%s_%i", fun->name, fun->id);
}
static void compile_type_to_c(FILE* header, FILE* body, StructAST* str, DynArray* done) {
  arrayPushVal(done, &str);

  // check if we need to compile the members types first
  for (int i = 0; i < str->num_members; ++i) {
    MemberAST* member = str->members + i;
    if (member->type->type == STRUCT) {
      StructAST* membertype = (StructAST*) member->type;
      bool has_been_compiled = false;
      for (StructAST** s = arrayBegin(done), **end = arrayEnd(done); s != end; ++s) {
        if (*s == membertype) {
          has_been_compiled = true;
        }
      }
      if (!has_been_compiled) {
        compile_type_to_c(header, body, membertype, done);
      }
    }
  }

  logDebugInfo("Compiling struct %s to C\n", str->type.name);

  /* Compile struct definition */
  fprintf(header, "typedef struct T_%s T_%s;\n", str->type.name, str->type.name);
  fprintf(body, "typedef struct T_%s {\n", str->type.name);
  for (int i = 0; i < str->num_members; ++i) {
    MemberAST* member = str->members + i;
    fprintf(body, "\tT_%s %s;\n", member->type->name, member->name);
  }
  fprintf(body, "} T_%s;\n\n", str->type.name);
  /* TODO: check for default values */
  if (str->has_initializer) {
    UNIMPLEMENTED;
  }
}

static void compile_expression_to_c(ExpressionAST* expr, FILE* body) {
  switch (expr->expr_type) {
    case FLOAT_EXPR: {
      FloatAST* f = (FloatAST*) expr;
      fprintf(body, "%lf", f->value);
    } break;

    case INT_EXPR: {
      IntAST* i = (IntAST*) expr;
      fprintf(body, "%i", i->value);
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
      if (call->fun->is_foreign) {
        fprintf(body, "%s(", call->name);
      }
      else {
        print_function_mangle(body, call->fun); fprintf(body, "(");
      }
      for (int i = 0; i < call->num_args; ++i) {
        compile_expression_to_c(call->args[i], body);
        if (i != call->num_args - 1) {
          fprintf(body, ", ");
        }
      }
      fprintf(body, ")");
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
      logErrorAt(expr->stmt.pos, "<internal>: Cannot directly compile a struct initialization\n");
    } break;
  }
}

static void compile_struct_init_to_c(String name, StructInitializationAST* init, FILE* file) {
  for (int i = 0; i < init->num_members; ++i) {
    MemberInitializationAST* member = init->members + i;
      stringAppendChar(&name, '.');
    int len = stringAppend(&name, member->name);
    if (member->value->expr_type == STRUCT_INIT_EXPR) {
      compile_struct_init_to_c(name, (StructInitializationAST*) member->value, file);
    } else {
      fprintf(file, "\n%s = ", stringGet(&name));
      compile_expression_to_c(member->value, file);
      fprintf(file, ";");
    }
    stringPop(&name, len+1);
  }
}

static void get_expression_string(ExpressionAST* expr, String* s) {
  if (expr->expr_type == MEMBER_ACCESS_EXPR) {
    MemberAccessAST* ast = (MemberAccessAST*) expr;
    get_expression_string(ast->base, s);
    stringAppendChar(s, '.');
    stringAppend(s, ast->name);
  } else if (expr->expr_type == VARIABLE_GET_EXPR) {
    stringAppend(s, "v_");
    stringAppend(s, ((VariableGetAST*)expr)->name);
  } else {
    logErrorAt(expr->stmt.pos, "Unsupported member access base type\n");
  }
}

static void compile_statement_to_c(StatementAST* stmt, FILE* file) {
  switch (stmt->type) {

    case EXPRESSION_STMT: {
      ExpressionAST* expr = (ExpressionAST*) stmt;
      fprintf(file, "\n");
      compile_expression_to_c(expr, file);
      fprintf(file, ";");
    } break;

    case LOOP_STMT: {
      LoopAST* loop = (LoopAST*) stmt;
      // TODO: use size_t instead
      if (loop->iterator_name) {
        fprintf(file, "\nfor (int v_%s = ", loop->iterator_name);
      } else {
        fprintf(file, "\nfor (int it_index = ");
      }
      compile_expression_to_c(loop->from, file);
      fprintf(file, ", end_index = ");
      compile_expression_to_c(loop->to, file);
      if (loop->index_name) {
        fprintf(file, ", v_%s = 0", loop->index_name);
      }
      if (loop->iterator_name) {
        fprintf(file,
          "; v_%s < end_index; ++v_%s"
        , loop->iterator_name, loop->iterator_name);
      } else {
        fprintf(file,
          "; it_index < end_index; ++it_index");
      }
      if (loop->index_name) {
        fprintf(file, ", ++v_%s", loop->index_name);
      }
      fprintf(file, ") {");
      for (int i = 0; i < loop->body.num_statements; ++i) {
        compile_statement_to_c(loop->body.statements[i], file);
      }
      fprintf(file, "\n};");
    } break;

    case VARIABLE_DECLARATION_STMT: {
      VariableDeclarationAST* var = (VariableDeclarationAST*) stmt;
      fprintf(file, "\n");

      // struct?
      if (var->type->type == STRUCT) {
        StructInitializationAST* init = (StructInitializationAST*) var->value;
        StructAST* str = (StructAST*) var->type;
        fprintf(file, "T_%s v_%s = {0};", var->type->name, var->name);
        if (str->has_initializer) {
          UNIMPLEMENTED;
        }
        if (var->value) {
          String name;
          stringInit(&name, "v_");
          stringAppend(&name, var->name);
          compile_struct_init_to_c(name, init, file);
          stringFree(&name);
        }
      }
      // builtin
      else {
        fprintf(file, "T_%s v_%s", var->type->name, var->name);
        if (var->value) {
          fprintf(file, " = ");
          compile_expression_to_c(var->value, file);
        }
        fprintf(file, ";");
      }
    } break;

    case ASSIGNMENT_STMT: {
      AssignmentAST* ass = (AssignmentAST*) stmt;
      fprintf(file, "\n");

      if (ass->lhs->expr_type == VARIABLE_GET_EXPR || ass->lhs->expr_type == MEMBER_ACCESS_EXPR) {
        if (ass->rhs->expr_type == STRUCT_INIT_EXPR) {
          StructInitializationAST* init = (StructInitializationAST*) ass->rhs;
          // compile_expression_to_c(ass->lhs, file);
          String name;
          stringInit(&name, 0);
          get_expression_string(ass->lhs, &name);
          fprintf(file, "\nmemset(&%s, 0, sizeof(%s));", stringGet(&name), stringGet(&name));
          compile_struct_init_to_c(name, init, file);
        } else {
          compile_expression_to_c(ass->lhs, file);
          fprintf(file, " = ");
          compile_expression_to_c(ass->rhs, file);
          fprintf(file, ";");
        }
      } else {
        // TODO: pretty-print expression types
        logErrorAt(ass->stmt.pos, "Assignment to expression of type %i is not allowed\n", ass->lhs->expr_type);
      }
    } break;

    case RETURN_STMT: {
      ReturnAST* ret = (ReturnAST*) stmt;
      if (ret->value->expr_type == STRUCT_INIT_EXPR) {
        StructInitializationAST* init = (StructInitializationAST*) ret->value;
        StructAST* str = (StructAST*) init->expr.type;
        fprintf(file, "\n{\nT_%s __returnval = {0};", ret->value->type->name);
        if (str->has_initializer) {
          UNIMPLEMENTED;
        }
        String name;
        stringInit(&name, "__returnval");
        compile_struct_init_to_c(name, init, file);
        fprintf(file, "\nreturn __returnval;\n}");
      } else {
        fprintf(file, "\nreturn ");
        compile_expression_to_c(ret->value, file);
        fprintf(file, ";");
      }
    } break;

    case UNKNOWN_STMT: {
      logDebugError("Trying to compile an unknown statement type???\n");
    } break;

  }
}

static void compile_block_to_c(CompoundStatementAST* block, FILE* file) {
  fprintf(file, "{");
  for (int i = 0; i < block->num_statements; ++i) {
    compile_statement_to_c(block->statements[i], file);
  }
  fprintf(file, "\n};\n");
}

/** Main **/

int main(int argc, char const *argv[]) {
  #ifdef DEBUG
    test();
  #endif

  if (argc == 1) {
    logErrorAt(prev_pos, "Usage: %s <filename>\n", argv[0]);
    return 1;
  }
  else {
    next_pos.file = (char*) argv[1];
    file = fopen(argv[1], "r");
    if (!file) {
      logErrorAt(prev_pos, "Could not open file %s\n", argv[1]);
      return 1;
    }
  }

  // enable formatting?
  #ifdef LINUX
    if (isatty(1)) {
      enableFormatting();
    }
  #endif

  // init globals
  arenaInit(&perm_arena);

  /** Parsing **/

  eatToken();
  CompoundStatementAST* global_scope = arenaPush(&perm_arena, sizeof(CompoundStatementAST));
  global_scope->stmt.type = COMPOUND_STMT;
  DynArray statements;/* StatementAST* */
  arrayInit(&statements, 32, sizeof(StatementAST*));

  while (token != TOK_EOF) {
    StatementAST* stmt = parseStatement();
    if (stmt) {
      switch (stmt->type) {
        case FUNCTION_DECLARATION_STMT: {
          FunctionDeclarationAST* fun = (FunctionDeclarationAST*) stmt;
          fun->body.parent_scope = global_scope;
          arrayPushVal(&statements, &stmt);
          logDebugInfo("Adding function definition %s\n", ((FunctionDeclarationAST*) stmt)->name);
        } break;
        case VARIABLE_DECLARATION_STMT: {
          arrayPushVal(&statements, &stmt);
          logDebugInfo("Adding variable declaration %s\n", ((VariableDeclarationAST*) stmt)->name);
        } break;
        case STRUCT_DECLARATION_STMT: {
          arrayPushVal(&statements, &stmt);
          logDebugInfo("Adding type %s\n", ((StructAST*) stmt)->type.name);
        } break;
        default:
          logErrorAt(stmt->pos, "Only variable, struct and function definitions allowed at global scope\n");
          break;
      }
    } else {
      if (token != TOK_EOF) {
        return 1;
      }
    }
  }

  global_scope->statements = pushArrayToArena(&statements, &perm_arena);
  global_scope->num_statements = arrayCount(&statements);
  arrayFree(&statements);

  if (found_error) {
    logError("Exiting due to previous errors\n");
    return 1;
  }

  /** Do type inference and build scope tree **/

  doTypeInferenceForScope(global_scope);

  if (found_error) {
    logError("Exiting due to previous errors\n");
    return 1;
  }

  /** Compile to C **/

  {
    DynArray mains = getFunctionDeclarations("main", global_scope);
    if (arrayCount(&mains) == 1) {
      // TODO: check signature of main
      FILE* header = tmpfile();
      FILE* body = tmpfile();
      FILE* tail = tmpfile();
      FunctionDeclarationAST* jai_main = *(FunctionDeclarationAST**) arrayGet(&mains, 0);
      if (body && header && tail) {

        // some builtin types
        // TODO: make to work on all systems
        fprintf(header,
          "#include <stddef.h>\n\n"
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
          "typedef void T_void;\n\n"
          "void* memset( void* dest, int ch, size_t count );\n\n"
        );

        /* compile all structs in global scope */
        /* TODO: compile all structs in all scopes */
        {
          DynArray compiled_structs;
          arrayInit(&compiled_structs, 16, sizeof(StructAST*));

          /* write types */
          for (int i = 0; i < global_scope->num_statements; ++i) {
            StatementAST* stmt = global_scope->statements[i];
            if (stmt->type == STRUCT_DECLARATION_STMT) {
              StructAST* str = (StructAST*) stmt;

              /* check if it has been compiled already */
              bool has_been_compiled = false;
              for (StructAST** s = arrayBegin(&compiled_structs), **end = arrayEnd(&compiled_structs); s != end; ++s) {
                if (*s == str) {
                  has_been_compiled = true;
                }
              }

              if (!has_been_compiled) {
                compile_type_to_c(header, body, str, &compiled_structs);
              }
            }
          }

          arrayFree(&compiled_structs);
          fprintf(header, "\n");
        }

        /* compile all functions */
        {
          for (int i = 0; i < global_scope->num_statements; ++i) {
            StatementAST* stmt = global_scope->statements[i];
            if (stmt->type == FUNCTION_DECLARATION_STMT) {
              FunctionDeclarationAST* fun = (FunctionDeclarationAST*) stmt;

              if (fun->is_foreign) {
                fprintf(header, "%s ", fun->return_type->name);
                fprintf(header, "%s", fun->name);

                fprintf(header, "(");
                for (int i = 0; i < fun->num_args; ++i) {
                  ArgumentAST* arg = fun->args + i;
                  fprintf(header, "T_%s v_%s", arg->type->name, arg->name);
                  if (i != fun->num_args - 1) {
                    fprintf(header, ", ");
                  }
                }
                fprintf(header, ");\n");
              }

              else {

                fprintf(header, "T_%s ", fun->return_type->name);
                fprintf(body, "T_%s ", fun->return_type->name);

                print_function_mangle(header, fun);
                print_function_mangle(body, fun);

                fprintf(header, "(");
                fprintf(body, "(");
                for (int i = 0; i < fun->num_args; ++i) {
                  ArgumentAST* arg = fun->args + i;
                  fprintf(header, "T_%s v_%s", arg->type_name, arg->name);
                  fprintf(body, "T_%s v_%s", arg->type_name, arg->name);
                  if (i != fun->num_args - 1) {
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
        }

        /* TODO: check signature of main */
        fprintf(tail, "int main(int argc, const char* argv[]) {\n\t"); print_function_mangle(tail, jai_main); fprintf(tail, "();\n};\n");
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
            logError("Could not open output file '/tmp/output.c'\n");
          }

        }
      } else {
        logError("Failed to open temp files\n");
      }
    }
    else if (arrayCount(&mains) > 1) {
      logError("only one main can be defined :O");
    }
    else {
      logError("No main function declared\n");
    }
  }
  return 0;
}
