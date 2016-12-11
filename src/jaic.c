#define DEBUG
#define USE_COLORS
#include "terminal.c"
#include "memarena.c"
#include "utils.c"
#include "array.c"
#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#ifdef LINUX
  #include <unistd.h>
#endif

#define global static

static char* pushString(MemArena* arena, char* str) {
  int len = strlen(str) + 1;
  char* result = arenaPush(arena, len);
  memcpy(result, str, len);
  return result;
}
static void* pushArrayToArena(DynArray* arr, MemArena* arena) {
  int size;
  void* source;
  void* dest;
  arrayGetData(arr, &source, &size);
  dest = arenaPush(arena, size);
  memcpy(dest, source, size);
  return dest;
}

typedef enum {
  TOK_EOF = -1,
  TOK_IDENTIFIER = -3,
  TOK_FLOAT = -4,
  TOK_INT = -5,
  TOK_UNKNOWN = -6,
  TOK_ARROW = -7,
  TOK_STRUCT = -8,
} Token;

// values set by the tokenizer
global char* identifierStr;
global double floatVal;
global int intVal;

global bool found_error = false;

// the input source file
global FILE* file;

#define MAIN_MEMORY_SIZE 64*1024*1024

global MemArena perm_arena;

#define pushPosition \
  FilePos _position = next_pos; \
  fgetpos(file, &_position.fpos); \
  char _current_char = curr_char; \
  logDebugInfo("Pushing %s:%i:%i\n", _position.file, _position.line, _position.column);
#define popPosition \
  fsetpos(file, &_position.fpos); \
  next_pos = _position; \
  curr_char = _current_char; \
  logDebugInfo("Jumping back to %s:%i:%i\n", next_pos.file, next_pos.line, next_pos.column);

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
global FilePos prev_pos = {.line = 1, .column = 1};
global FilePos next_pos = {.line = 1, .column = 1};

int binopPrecedence(char op) {
  switch (op) {
    case '-': return 10;
    case '+': return 10;
    case '*': return 20;
    case '/': return 20;
    default: return 0;
  }
}

static char getChar(FILE* file) {
  char c = getc(file);
  ++next_pos.column;
  if (c == '\n') {
    ++next_pos.line;
    next_pos.column = 1;
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

static void eatToken() {
  // logDebugInfo("before gettok: line: %i column: %i\n", prev_pos.line, prev_pos.column);
  token = gettok();
  eatWhitespace();
  // logDebugInfo("after gettok: line: %i column: %i\n", next_pos.line, next_pos.column);
  // printf("%s\n", print_token());
}

typedef enum {
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

typedef enum {
  STRUCT_DECLARATION_STMT,
  VARIABLE_DECLARATION_STMT,
  FUNCTION_DECLARATION_STMT,
  EXPRESSION_STMT,
  COMPOUND_STMT, // new scope
} StatementType;

typedef struct {
  StatementType type;
  FilePos pos;
} StatementAST;

typedef struct {
  StatementAST stmt;
  char* name;
  Type type;
  FilePos pos;
} TypeAST;

TypeAST builtin_types[] = {
  {},
  {.type = VOID, .name = "void"},
  {.type = INT, .name = "int"},
  {.type = FLOAT, .name = "float"},
  {.type = U8, .name = "u8"},
};

typedef enum {
  FLOAT_AST,
  INT_AST,
  CALL_AST,
  VARIABLE_AST,
  STRUCT_INIT_AST,
  BINOP_AST,
} ExpressionType;

typedef struct {
  StatementAST stmt;
  ExpressionType expr_type;
  TypeAST* type;
} ExpressionAST;

typedef struct CompoundStatementAST {
  StatementAST stmt;
  struct CompoundStatementAST* parent_scope;
  int num_statements;
  StatementAST** statements;
  // TODO: optimize variable lookups
} CompoundStatementAST;

typedef struct {
  char* name;
  char* type_name;
  TypeAST* type;
  FilePos pos;
} MemberAST;
typedef struct {
  TypeAST type;
  int num_members;
  MemberAST* members;
} StructAST;

typedef struct {
  char* name;
  ExpressionAST* value;
  MemberAST* member;
} MemberInitializationAST;
typedef struct {
  ExpressionAST expr;
  int num_members;
  MemberInitializationAST* members;
} StructInitializationAST;

typedef struct {
  char* name;
  char* type_name;
  TypeAST* type;
  FilePos pos;
} ArgumentAST;

typedef struct {
  StatementAST stmt;
  char* name;
  int num_args;
  ArgumentAST* args;
  char* return_type_name;
  TypeAST* return_type;
  FilePos pos;
  bool is_foreign;
  int num_statements;
  CompoundStatementAST body;
} FunctionDeclarationAST;

typedef struct {
  StatementAST stmt;
  char* name;
  char* type_name;
  TypeAST* type;
  ExpressionAST* value;
  FilePos pos;
} VariableDeclarationAST;

/* Expressions */

typedef struct {
  ExpressionAST expr;
  char* name;
} VariableGetAST;

typedef struct {
  ExpressionAST expr;
  float value;
} FloatAST;

typedef struct {
  ExpressionAST expr;
  int value;
} IntAST;

typedef struct {
  ExpressionAST expr;
  char* name;
  int num_args;
  FunctionDeclarationAST* fun;
  ExpressionAST** args;
} CallAST;

typedef struct {
  ExpressionAST expr;
  char operator;
  ExpressionAST* lhs;
  ExpressionAST* rhs;
} BinaryExpressionAST;

static int test() {
  arrayTest();
  return 0;
}

typedef enum {
  IdentifierType_UNKNOWN = 0,
  IdentifierType_FUNCTION,
  IdentifierType_VARIABLE,
  IdentifierType_TYPE
} IdentifierType;

/* (a:int, b:float)->int {a+b} */
static FunctionDeclarationAST* parseFunctionDeclaration(bool try) {
  FunctionDeclarationAST* result = arenaPush(&perm_arena, sizeof(FunctionDeclarationAST));
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
    arg->pos = prev_pos;

    if (token != TOK_IDENTIFIER) {
      if (!try) {
        logErrorAt(prev_pos, "Identifier expected in parameter list, found %c\n", token);
      }
      arrayFree(&args);
      return 0;
    }

    arg->name = pushString(&perm_arena, identifierStr);
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

    arg->type_name = pushString(&perm_arena, identifierStr);
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
    result->return_type_name = pushString(&perm_arena, identifierStr);
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
    // CONTINUE
    int depth = 1;
    while (depth > 0) {
      if (token == '{') ++depth;
      else if (token == '}') --depth;
      else if (token == TOK_EOF) {
        logErrorAt(prev_pos, "'{' never matched with a '}'");
        exit(1);
      }
      eatToken();
    }
  } else {
    logErrorAt(prev_pos, "No '{' found after function parameter list\n");
    return 0;
  }

  return result;
}

static ExpressionAST* parseExpression();
static ExpressionAST* parsePrimitive() {
  FilePos pos = prev_pos;
  switch (token) {
    case TOK_FLOAT: {
      FloatAST* ast = arenaPush(&perm_arena, sizeof(FloatAST));
      ast->expr.expr_type = FLOAT_AST;
      ast->expr.stmt.pos = pos;
      ast->value = floatVal;
      eatToken();
      logDebugInfo("Found a float literal with value %f\n", ast->value);
      return &ast->expr;
    } break;

    case TOK_INT: {
      IntAST* ast = arenaPush(&perm_arena, sizeof(IntAST));
      ast->expr.expr_type = INT_AST;
      ast->expr.stmt.pos = pos;
      ast->value = intVal;
      eatToken();
      logDebugInfo("Found an int literal with value %i\n", ast->value);
      return &ast->expr;
    } break;

    case TOK_IDENTIFIER: {
      byte* start = arenaGetCurrent(perm_arena);
      char* name = pushString(&perm_arena, identifierStr);
      eatToken();
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

        // push to permanent storage
        CallAST* call = arenaPush(&perm_arena, sizeof(CallAST));
        call->expr.stmt.pos = pos;
        call->expr.expr_type = CALL_AST;
        call->name = name;
        call->num_args = arrayCount(&args);
        if (call->num_args > 0) {
          call->args = pushArrayToArena(&args, &perm_arena);
        }
        arrayFree(&args);

        logDebugInfo("Found function call to '%s' with %i inputs\n", call->name, call->num_args);
        return &call->expr;
      }
      else {
        VariableGetAST* ast = arenaPush(&perm_arena, sizeof(VariableGetAST));
        ast->expr.expr_type = VARIABLE_AST;
        ast->expr.stmt.pos = pos;
        ast->name = name;
        logDebugInfo("Found a variable get with name %s\n", name);
        return &ast->expr;
      }
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
      ast->expr.expr_type = STRUCT_INIT_AST;
      ast->expr.stmt.pos = pos;
      if (token != '}') {
        DynArray members;
        arrayInit(&members, 4, sizeof(MemberInitializationAST));
        while (1) {
          MemberInitializationAST* member = arrayPush(&members);
          bool success = false;
          if (token == TOK_IDENTIFIER) {
            member->name = pushString(&perm_arena, identifierStr);
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
      logErrorAt(prev_pos, "Expression expected, but found neither a literal, function call or a variable name, instead found %c\n", token);
      return 0;
    } break;
  }
}

static ExpressionAST* parseExpression() {
  ExpressionAST* expr = parsePrimitive();
  if (expr) {
    if (binopPrecedence(token)) {
      eatToken();
      BinaryExpressionAST* bin = arenaPush(&perm_arena, sizeof(BinaryExpressionAST));
      bin->expr.expr_type = BINOP_AST;
      bin->operator = token;
      bin->lhs = expr;
      bin->rhs = parseExpression();
      return &bin->expr;
      // TODO: operator precedence
    }
  }
  return expr;
}

static void evaluateTypeOfStruct(StructAST* str, CompoundStatementAST* scope);

TypeAST* getTypeDeclaration(char* name, CompoundStatementAST* scope) {
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

DynArray getFunctionDeclarations(char* name, CompoundStatementAST* scope) {
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

VariableDeclarationAST* getVariableDeclaration(char* name, CompoundStatementAST* scope) {
  while (scope) {
    for (int i = 0; i < scope->num_statements; ++i) {
      StatementAST* stmt = scope->statements[i];
      if (stmt->type == VARIABLE_DECLARATION_STMT) {
        VariableDeclarationAST* var = (VariableDeclarationAST*) stmt;
        if (strcmp(var->name, name) == 0) {
          return var;
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
static void evaluateTypeOfVariable(VariableDeclarationAST* var, CompoundStatementAST* scope) {
  if (!var) return;
  if (!var->type) {
    TypeAST* declared_type = 0;

    if (var->type_name) {
      declared_type = getTypeDeclaration(var->type_name, scope);
      if (!declared_type) {
        logErrorAt(var->pos, "Declared type '%s' for '%s' doesn't exist\n", var->type_name, var->name);
        var->type = 0;
        return;
      }
      var->type = declared_type;
    }

    if (var->value) {
      evaluateTypeOfExpression(var->value, var->type, scope);
      if (!var->value->type) {
        logErrorAt(var->pos, "Couldn't infer type for %s\n", var->name);
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
}

static void evaluateTypeOfFunction(FunctionDeclarationAST* fun, CompoundStatementAST* scope) {
  if (!fun || fun->return_type) {
    return;
  }
  assert(fun->return_type_name); // return_type should have been set to void

  for (int i = 0; i < fun->num_args; ++i) {
    fun->args[i].type = getTypeDeclaration(fun->args[i].type_name, scope);
    if (!fun->args[i].type) {
      logErrorAt(fun->args[i].pos, "Could not find type '%s'\n", fun->args[i].type_name);
    }
  }

  fun->return_type = getTypeDeclaration(fun->return_type_name, scope);
}

static void evaluateTypeOfExpression(ExpressionAST* expr, TypeAST* evidence, CompoundStatementAST* scope) {
  if (expr->type) {
    return;
  }

  switch (expr->expr_type) {

    case FLOAT_AST: {
      expr->type = &builtin_types[FLOAT];
    } break;

    case INT_AST: {
      expr->type = &builtin_types[INT];
    } break;

    case CALL_AST: {
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

    case VARIABLE_AST: {
      VariableGetAST* var_ref = (VariableGetAST*) expr;
      VariableDeclarationAST* var = getVariableDeclaration(var_ref->name, scope);
      if (!var) {
        logErrorAt(prev_pos, "Use of undeclared variable '%s'", var_ref->name);
        return;
      }
      evaluateTypeOfVariable(var, scope);
      expr->type = var->type;
    } break;

    case STRUCT_INIT_AST: {
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

    case BINOP_AST: {
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

StatementAST* parseStatement() {
  while (1) {
    if (token == EOF) {
      return 0;
    } if (token != ';') {
      break;
    }
    eatToken();
  }

  switch (token) {

    case TOK_IDENTIFIER: {
      FilePos pos_of_identifier = prev_pos;
      char* name = pushString(&perm_arena, identifierStr);
      logDebugInfo("Found declaration identifier %s\n", name);
      char* type = 0;
      bool is_declaration = false;
      eatToken();

      if (token == ':') {
        is_declaration = true;
        eatToken();
        if (token == TOK_IDENTIFIER) {
          type = pushString(&perm_arena, identifierStr);
          eatToken();
        }
      }

      if (token == '=') {
        pushPosition;
        pushState;
        eatToken();

        // struct?
        if (token == TOK_STRUCT) {
          eatToken();
          if (token == '{') {
            eatToken();

            StructAST* str = arenaPush(&perm_arena, sizeof(StructAST));
            str->type.stmt.type = STRUCT_DECLARATION_STMT;
            str->type.pos = pos_of_identifier;
            str->type.type = STRUCT;
            str->type.name = name;
            DynArray members; /* MemberAST */
            arrayInit(&members, 4, sizeof(MemberAST));
            bool success = false;
            while (1) {
              MemberAST* member = arrayPush(&members);
              if (token == TOK_IDENTIFIER) {
                member->name = pushString(&perm_arena, identifierStr);
                member->pos = prev_pos;
                eatToken();
                if (token == ':') {
                  eatToken();
                    // TODO: can have value as well!
                  if (token == TOK_IDENTIFIER) {
                    member->type_name = pushString(&perm_arena, identifierStr);
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

        // TODO: switch function and expression checks
        // function?
        {
          FunctionDeclarationAST* fun = parseFunctionDeclaration(true);
          if (fun) {
            fun->stmt.type = FUNCTION_DECLARATION_STMT;
            fun->name = name;
            fun->pos = pos_of_identifier;
            logDebugInfoAt(pos_of_identifier, "Function declaration for '%s' found\n", fun->name);
            if (type) {
              logErrorAt(pos_of_identifier, "Function declaration should not have type\n");
              return 0;
            }
            else if (!is_declaration) {
              logErrorAt(pos_of_identifier, "Cannot redefine a function. (did you forget to do ':=' instead of just '='?)");
              return 0;
            }
              // TODO: check if identifier already exists
            else {
              logDebugInfo("Found a function definition with name %s, and %i arguments\n", fun->name, fun->num_args);
              return &fun->stmt;
            }
          }
        }

          // expression?
        popPosition;
        popState;
        eatToken();
        {
          ExpressionAST* value = parseExpression();
          if (value) {
            VariableDeclarationAST* var = arenaPush(&perm_arena, sizeof(VariableDeclarationAST));
            var->stmt.type = VARIABLE_DECLARATION_STMT;
            var->pos = pos_of_identifier;
            var->name = name;
            var->type_name = type ? type : 0;
            var->value = value;
            var->type = 0;
            logDebugInfo("Found a variable declaration for %s with type %s and a value of type %i\n", var->name, var->type_name, var->value->expr_type);
            return &var->stmt;
          }
        }


        logErrorAt(pos_of_identifier, "Neither expression or function prototype found after declaration\n");
        return 0;
      }

      // declaration without value?
      if (is_declaration) {
        VariableDeclarationAST* var = arenaPush(&perm_arena, sizeof(VariableDeclarationAST));
        var->stmt.type = VARIABLE_DECLARATION_STMT;
        var->name = name;
        var->type_name = type;
        logDebugInfo("Found a variable declaration for %s with type %s and no value\n", var->name, var->type_name);
        return &var->stmt;
        // TODO: default initialization here!
      }

      else {
        logErrorAt(prev_pos, "Unexpected token %s after ':'\n", print_token());
      }
    } break;

    case TOK_EOF: {
      return 0;
    } break;

    default: {
      logErrorAt(prev_pos, "Unexpected token %c\n", token);
      return 0;
    } break;
  }
  return 0;
}

void doTypeInferenceForScope(CompoundStatementAST* scope) {
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
        evaluateTypeOfVariable(var, scope);
        logDebugInfoAt(var->pos, "Evaluated type of '%s' to \n", var->name);
      } break;

      case FUNCTION_DECLARATION_STMT: {
        FunctionDeclarationAST* fun = (FunctionDeclarationAST*) stmt;
        evaluateTypeOfFunction(fun, scope);
        if (fun->body.num_statements) {
          doTypeInferenceForScope(&fun->body);
        }
        logDebugInfoAt(fun->pos, "Evaluated body of '%s'\n", fun->name);
      } break;

      case EXPRESSION_STMT: {
        ExpressionAST* expr = (ExpressionAST*) stmt;
        evaluateTypeOfExpression(expr, 0, scope);
        logDebugInfoAt(expr->stmt.pos, "Evaluated type of expression to %s\n", expr->type->name);
      } break;

      case COMPOUND_STMT: {
        doTypeInferenceForScope((CompoundStatementAST*) stmt);
      } break;
    }
  }
}

void compile_type_to_c(FILE* header, FILE* structs, StructAST* str, DynArray* done) {
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
        compile_type_to_c(header, structs, membertype, done);
      }
    }
  }

  logDebugInfo("Compiling struct %s to C\n", str->type.name);

  // TODO: fix indentation?
  fprintf(header, "typedef struct jai_%s jai_%s;\n", str->type.name, str->type.name);
  fprintf(structs, "typedef struct jai_%s {\n", str->type.name);
  for (int i = 0; i < str->num_members; ++i) {
    MemberAST* member = str->members + i;
    fprintf(structs, "\t%s %s;\n", member->type->name, member->name);
  }
  fprintf(structs, "} jai_%s;\n\n", str->type.name);
}

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
  DynArray statements;/* StatementAST* */
  arrayInit(&statements, 32, sizeof(StatementAST*));

  while (token != TOK_EOF) {
    StatementAST* stmt = parseStatement();
    if (stmt) {
      switch (stmt->type) {
        case FUNCTION_DECLARATION_STMT: {
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
          logError("Only variable, struct and function definitions allowed at global scope\n");
      }
    } else {
      if (token != TOK_EOF) {
        logError("Failed to parse statement\n");
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

  /** Type inferens **/

  doTypeInferenceForScope(global_scope);

  if (found_error) {
    logError("Exiting due to previous errors\n");
    return 1;
  }

  // Compile to C
  {
    DynArray mains = getFunctionDeclarations("main", global_scope);
    if (arrayCount(&mains) == 1) {
      // TODO: check inparameters of main
      FILE* header = tmpfile();
      FILE* body = tmpfile();
      FILE* tail = tmpfile();
      if (body && header && tail) {

        {
          DynArray compiled_structs;
          arrayInit(&compiled_structs, 16, sizeof(StructAST*));

          // write types
          for (int i = 0; i < global_scope->num_statements; ++i) {
            StatementAST* stmt = global_scope->statements[i];
            if (stmt->type == STRUCT_DECLARATION_STMT) {
              StructAST* str = (StructAST*) stmt;
              compile_type_to_c(header, body, str, &compiled_structs);
            }
          }

          arrayFree(&compiled_structs);
        }

        {
          for (int i = 0; i < global_scope->num_statements; ++i) {
            StatementAST* stmt = global_scope->statements[i];
            if (stmt->type == FUNCTION_DECLARATION_STMT) {
              FunctionDeclarationAST* fun = (FunctionDeclarationAST*) stmt;
              if (fun->is_foreign) {continue;}
              fprintf(header, "jai_%s", fun->name);
              fprintf(body, "jai_%s", fun->name);

              // name mangle
              for (int i = 0; i < fun->num_args; ++i) {
                ArgumentAST* arg = fun->args + i;
                fprintf(header, "_%s", arg->type_name);
                fprintf(body, "_%s", arg->type_name);
              }

              fprintf(header, "(");
              fprintf(body, "(");
              for (int i = 0; i < fun->num_args; ++i) {
                ArgumentAST* arg = fun->args + i;
                fprintf(header, "%s %s", arg->type_name, arg->name);
                fprintf(body, "%s %s", arg->type_name, arg->name);
                if (i != fun->num_args - 1) {
                  fprintf(header, ", ");
                  fprintf(body, ", ");
                }
              }
              fprintf(header, ");\n");
              fprintf(body, ") {\n");

              // body
              for (int i = 0; i < fun->num_statements; ++i) {
                StatementAST* stmt = fun->body.statements[i];
              }

              fprintf(body, "};\n\n");
            }
          }
        }

        fprintf(tail, "int main(int argc, const char* argv[]) {\n\tjai_main();\n}\n");
        /*
        FunctionDeclarationAST* mainf = *((FunctionDeclarationAST**) arrayGet(&mains, 0));
        fprintf(tail, "int main(int argc, const char* argv[]) {\n");
        for (int i = 0; i < mainf->body.num_statements; ++i) {
          StatementAST* stmt = mainf->body.statements[i];
          switch (stmt->type) {
            case VARIABLE_DECLARATION_STMT: {
              VariableDeclarationAST* var = (VariableDeclarationAST*) stmt;
              fprintf(tail, "%s %s;", var->type->name, var->name);
              if (var->value) {
                fprintf(head, "void __jai__initialize_%s(%s\n", var->type->name, var->type->name);
                fprintf(tail, ";\n");
              }
              // TODO: default initialization
            } break;

            case EXPRESSION_STMT: {

            } break;

            default:;
          }
        }
          */
        arrayFree(&mains);

        {
          char buf[256];
          FILE* output = fopen("/tmp/output.c", "w");

          if (output) {

            // write header
            fprintf(output, "\n\n/*** HEADER ***/\n\n");
            rewind(header);
            while (1) {
              int read = fread(buf, 1, 256, header);
              fwrite(buf, read, 1, output);
              if (feof(header)) break;
            }
            fclose(header);

            // write body
            fprintf(output, "\n\n/*** STRUCTS ***/\n\n");
            rewind(body);
            while (1) {
              int read = fread(buf, 1, 256, body);
              logDebugInfo("%.*s\n", read, buf);
              fwrite(buf, read, 1, output);
              if (feof(body)) break;
            }
            fclose(body);

            // write tail
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
