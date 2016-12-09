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
      logError(prev_pos, "Reached max size for int\n");
    }
    else if (curr_char == '.') {
      buf[i++] = '.';
      curr_char = getChar(file);
      for (; i < BUFFER_SIZE && isdigit(curr_char); ++i) {
        buf[i] = curr_char;
        curr_char = getChar(file);
      }

      if (i == BUFFER_SIZE) {
        logError(prev_pos, "Reached max size for float number\n");
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
  VOID,
  STRUCT,
  INT,
  FLOAT,
} Type;

typedef struct {
  char* name;
  Type type;
  FilePos pos;
} TypeAST;

typedef enum {
  FLOAT_AST,
  INT_AST,
  CALL_AST,
  VARIABLE_AST,
  STRUCT_INIT_AST,
} ExpressionType;

typedef struct {
  ExpressionType expr_type;
  TypeAST* type;
  FilePos pos;
} ExpressionAST;

// some static types
global TypeAST voidType;
global TypeAST intType;
global TypeAST floatType;

static void initTypes() {
  voidType.type = VOID;
  voidType.name = "void";
  intType.type = INT;
  intType.name = "int";
  floatType.type = FLOAT;
  floatType.name = "float";
}

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
  FilePos pos;
} StructAST;

typedef struct {
  char* name;
  ExpressionAST* value;
  MemberAST* member;
} MemberInitializationAST;
typedef struct {
  ExpressionAST parent;
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
  char* name;
  int num_args;
  ArgumentAST* args;
  char* return_type_name;
  TypeAST* return_type;
  FilePos pos;
  // TODO: body??
} FunctionDeclarationAST;

typedef struct {
  char* name;
  char* type_name;
  TypeAST* type;
  ExpressionAST* value;
  FilePos pos;
} VariableDeclarationAST;

/* Expressions */

typedef struct {
  ExpressionAST parent;
  char* name;
} VariableGetAST;

typedef struct {
  ExpressionAST parent;
  float value;
} FloatAST;

typedef struct {
  ExpressionAST parent;
  int value;
} IntAST;

typedef struct {
  ExpressionAST parent;
  char* name;
  int num_args;
  FunctionDeclarationAST* fun;
  ExpressionAST** args;
} CallAST;

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

static int isIdentifierDefined(char* identifier);

// struct list
global DynArray types; /* TypeAST* */ 
static TypeAST* getTypeDeclaration(char*);
static void addTypeDeclaration(TypeAST* ast) {
  logDebugInfo("Adding type %s\n", ast->name);
  if (!isIdentifierDefined(ast->name)) {
    TypeAST** slot = arrayPush(&types);
    *slot = ast;
  }
  // TODO; print position
  else {logError(ast->pos, "Identifier %s is already defined\n", ast->name);} 
}
static TypeAST* getTypeDeclaration(char* name) {
  TypeAST** it = arrayBegin(&types);
  for (int i = 0, len = arrayCount(&types); i < len; ++i, ++it) {
    if (strcmp(name, (*it)->name) == 0) {
      return *it;
    }
  }
  return 0;
}

// variable list
global DynArray variables; /*VariableDeclarationAST* */
static void addVariableDeclaration(VariableDeclarationAST* ast) {
  logDebugInfo("Adding variable declaration %s\n", ast->name);
  if (!isIdentifierDefined(ast->name)) {
    VariableDeclarationAST** slot = arrayPush(&variables);
    *slot = ast;
  }
  else {logError(ast->pos, "Identifier %s is already defined\n", ast->name);}
}
static VariableDeclarationAST* getVariableDeclaration(char* name) {
  VariableDeclarationAST** it = arrayBegin(&variables);
  for (int i = 0, len = arrayCount(&variables); i < len; ++i, ++it) {
    if (strcmp(name, (*it)->name) == 0) {
      return *it;
    }
  }
  return 0;
}

// function list
global DynArray functions; /* FunctionDeclarationAST* */
static DynArray/*<FunctionDeclarationAST*>*/ getFunctionDeclarations(char* name);
static void addFunctionDeclaration(FunctionDeclarationAST* ast) {
  logDebugInfo("Adding function definition %s\n", ast->name);
  int exists = isIdentifierDefined(ast->name);
  if (exists == IdentifierType_UNKNOWN || exists == IdentifierType_FUNCTION) {
    // TODO: check if function signature already exists
    FunctionDeclarationAST** slot = (FunctionDeclarationAST**) arrayPush(&functions);
    *slot = ast;
  }
  else {logError(ast->pos, "Identifier %s is already defined\n", ast->name);}
}
static bool functionDeclarationExists(char* name) {
  for (FunctionDeclarationAST** it = arrayBegin(&functions), **end = arrayEnd(&functions); it < end; ++it) {
    if (strcmp(name, (*it)->name) == 0) {
      return true;
    }
  }
  return false;
}
static DynArray/*<FunctionDeclarationAST*>*/ getFunctionDeclarations(char* name) {
  DynArray arr;
  arrayInit(&arr, 0, sizeof(void*));
  FunctionDeclarationAST** it = arrayBegin(&functions);
  for (int i = 0, len = arrayCount(&functions); i < len; ++i, ++it) {
    if (strcmp(name, (*it)->name) == 0) {
      arrayPushVal(&arr, it);
    }
  }
  return arr;
}

static int isIdentifierDefined(char* identifier) {
  if (functionDeclarationExists(identifier)) {
    return IdentifierType_FUNCTION;
  }
  else if (getVariableDeclaration(identifier))  {
    return IdentifierType_VARIABLE;
  }
  else if (getTypeDeclaration(identifier)) {
    return IdentifierType_TYPE;
  }
  return IdentifierType_UNKNOWN;
}

/* (a:int, b:float)->int {a+b} */
static FunctionDeclarationAST* parseFunctionDeclaration(bool try) {
  FunctionDeclarationAST* result = arenaPush(&perm_arena, sizeof(FunctionDeclarationAST));
  if (token != '(') {
    if (!try) {
      logError(prev_pos, "token %s not start of a function prototype\n", print_token(token));
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
        logError(prev_pos, "Identifier expected in parameter list, found %c\n", token);
      }
      arrayFree(&args);
      return 0;
    }

    arg->name = pushString(&perm_arena, identifierStr);
    eatToken();
    logDebugInfo("Found parameter %s\n", arg->name);

    if (token != ':') {
      if (!try) {
        logError(prev_pos, "No type found after parameter name (did you forget a ':'?\n");
      }
      arrayFree(&args);
      return 0;
    }
    eatToken();

    if (token != TOK_IDENTIFIER) {
      if (!try) {
        logError(prev_pos, "Typename expected after ':'\n");
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

  // return value
  if (token == TOK_ARROW) {
    eatToken();
    if (token != TOK_IDENTIFIER) {
      logError(prev_pos, "Expected return type, got %s\n", print_token());
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

  if (token != '{') {
    logError(prev_pos, "No '{' found after function parameter list\n");
    return 0;
  }
  eatToken();

  int depth = 1;
  while (depth > 0) {
    if (token == '{') ++depth;
    else if (token == '}') --depth;
    else if (token == TOK_EOF) {
      if (!try) {
        logError(prev_pos, "'{' never matched with a '}'");
      }
      exit(1);
    }
    eatToken();
  }

  return result;
}

static ExpressionAST* parseExpression() {
  FilePos pos = prev_pos;
  switch (token) {
    case TOK_FLOAT: {
      FloatAST* ast = arenaPush(&perm_arena, sizeof(FloatAST));
      ast->parent.expr_type = FLOAT_AST;
      ast->parent.pos = pos;
      ast->value = floatVal;
      eatToken();
      logDebugInfo("Found a float literal with value %f\n", ast->value);
      return &ast->parent;
    } break;

    case TOK_INT: {
      IntAST* ast = arenaPush(&perm_arena, sizeof(IntAST));
      ast->parent.expr_type = INT_AST;
      ast->parent.pos = pos;
      ast->value = intVal;
      eatToken();
      logDebugInfo("Found an int literal with value %i\n", ast->value);
      return &ast->parent;
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
            logError(prev_pos, "Invalid expression inside function call to '%s'\n", name);
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
            logError(prev_pos, "Expected ',' between function input parameters\n");
          }
        }

        // push to permanent storage
        CallAST* call = arenaPush(&perm_arena, sizeof(CallAST));
        call->parent.pos = pos;
        call->parent.expr_type = CALL_AST;
        call->name = name;
        call->num_args = arrayCount(&args);
        if (call->num_args > 0) {
          call->args = pushArrayToArena(&args, &perm_arena);
        }
        arrayFree(&args);

        logDebugInfo("Found function call to '%s' with %i inputs\n", call->name, call->num_args);
        return &call->parent;
      }
      else {
        VariableGetAST* ast = arenaPush(&perm_arena, sizeof(VariableGetAST));
        ast->parent.expr_type = VARIABLE_AST;
        ast->parent.pos = pos;
        ast->name = name;
        logDebugInfo("Found a variable get with name %s\n", name);
        return &ast->parent;
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
      ast->parent.expr_type = STRUCT_INIT_AST;
      ast->parent.pos = pos;
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
                  logError(prev_pos, "Expected ',' or '}', but found %s\n", print_token());
                }
              } else {logError(prev_pos, "Failed to parse expression\n");}
            } else {logError(prev_pos, "Expected '=' after member name, found %s\n", print_token());}
          } else {logError(prev_pos, "Expected identifier, instead found %s\n", print_token());}

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
        return &ast->parent;
      } else {
        return 0;
      }
    } break;

    default: {
      logError(prev_pos, "Expression expected, but found neither a literal, function call or a variable name, instead found %c\n", token);
      return 0;
    } break;
  }
  return 0;
}

static void _evaluateTypeOfStruct(StructAST* str, DynArray* parents) {
  for (int i = 0; i < str->num_members; ++i) {
    if (!str->members[i].type) {
      str->members[i].type = getTypeDeclaration(str->members[i].type_name);
      if (!str->members[i].type) {
        logError(str->members[i].pos, "Could not find type %s\n", str->members[i].type_name);
        return;
      }
      if (str->members[i].type->type == STRUCT) {
        for (char **parent = arrayBegin(parents), **end = arrayEnd(parents); parent != end; ++parent) {
          if (strcmp(*parent, str->members[i].type->name) == 0) {
            logError(str->members[i].pos, "Type %s cannot contain itself. Did you mean to use a pointer?\n", *parent);
            exit(1);
          }
        }
        arrayPushVal(parents, &str->members[i].type->name);
        _evaluateTypeOfStruct((StructAST*) str->members[i].type, parents);
        arrayPop(parents);
      }
    }
  }
  // TODO, FIXME: check for recursive type
}
static void evaluateTypeOfStruct(StructAST* str) {
  DynArray parents;
  arrayInit(&parents, 1, sizeof(char*));
  arrayPushVal(&parents, &str->type.name);
  _evaluateTypeOfStruct(str, &parents);
}

static void evaluateTypeOfExpression(ExpressionAST* expr, TypeAST* evidence);
static void evaluateTypeOfVariable(VariableDeclarationAST* var) {
  if (!var) return;
  if (!var->type) {
    TypeAST* declared_type = 0;

    if (var->type_name) {
      declared_type = getTypeDeclaration(var->type_name);
      if (!declared_type) {
        logError(var->pos, "Declared type '%s' for '%s' doesn't exist\n", var->type_name, var->name);
        var->type = 0;
        return;
      }
      var->type = declared_type;
    }

    if (var->value) {
      evaluateTypeOfExpression(var->value, var->type);
      if (!var->value->type) {
        logError(var->pos, "Couldn't infer type for %s\n", var->name);
        var->type = 0;
        return;
      }
      var->type = var->value->type;
    }

    // check that infered expression type and stated type match
    if (var->value && var->value->type && declared_type && var->value->type != declared_type) {
      logError(prev_pos, "Inferred type of '%s': '%s' differs from declared type '%s'\n", var->name, var->value->type->name, declared_type->name);
      var->type = 0;
      return;
    }

    return;
  }
}

static void evaluateTypeOfFunction(FunctionDeclarationAST* fun) {
  if (!fun || fun->return_type) {
    return;
  }
  assert(fun->return_type_name); // return_type should have been set to void

  for (int i = 0; i < fun->num_args; ++i) {
    fun->args[i].type = getTypeDeclaration(fun->args[i].type_name);
    if (!fun->args[i].type) {
      logError(fun->args[i].pos, "Could not find type '%s'\n", fun->args[i].type_name);
    }
  }

  fun->return_type = getTypeDeclaration(fun->return_type_name);
}

static void evaluateTypeOfExpression(ExpressionAST* expr, TypeAST* evidence) {
  if (expr->type) {
    return;
  }

  switch (expr->expr_type) {

    case FLOAT_AST: {
      expr->type = &floatType;
    } break;

    case INT_AST: {
      expr->type = &intType;
    } break;

    case CALL_AST: {
      CallAST* call = (CallAST*) expr;
      logDebugInfo("Inferring call to %s\n", call->name);
      DynArray funs = getFunctionDeclarations(call->name);
      if (arrayCount(&funs)) {
        // eval functions and find best match
        int matches = 0;
        for (FunctionDeclarationAST** it = arrayBegin(&funs), **end = arrayEnd(&funs); it < end; ++it) {
          FunctionDeclarationAST* fun = *it;
          if (!fun->return_type && fun->return_type_name) {
            evaluateTypeOfFunction(fun);
          }
          // TODO: implicit type conversions?
          bool match = true;
          if (call->num_args != fun->num_args) {
            match = false;
          } else {
            for (int i = 0; i < fun->num_args; ++i) {
              call->args[i]->type = 0;
              evaluateTypeOfExpression(call->args[i], fun->args[i].type);
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
          logError(expr->pos, "Multiple matching overloads for %s.\n", call->name);
          logNote("Overloads are:\n");
          for (FunctionDeclarationAST** it = arrayBegin(&funs), **end = arrayEnd(&funs); it < end; ++it) {
            logNoteAt((*it)->pos, "\n");
          }
        } else if (matches == 0) {
          logError(expr->pos, "Could not find matching function overload for %s.\n", call->name);
          logNote("Alternatives are:\n");
          for (FunctionDeclarationAST** it = arrayBegin(&funs), **end = arrayEnd(&funs); it < end; ++it) {
            logNoteAt((*it)->pos, "\n");
          }
        }

      } else {
        logError(call->parent.pos, "Unknown function '%s'\n", call->name);
        return;
      }


      for (FunctionDeclarationAST** it = arrayEnd(&funs), **end = arrayEnd(&funs); it < end; ++it) {
        logError((*it)->pos, "Alternative\n");
      }
    } break;

    case VARIABLE_AST: {
      VariableGetAST* var_ref = (VariableGetAST*) expr;
      VariableDeclarationAST* var = getVariableDeclaration(var_ref->name);
      if (!var) {
        logError(prev_pos, "Use of undeclared variable '%s'", var_ref->name);
        return;
      }
      evaluateTypeOfVariable(var);
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
              logError(expr->pos, "%s has no member %s\n", target->type.name, ast->members[i].name);
              all_match = false;
              break;
            }
            assert(ast->members[i].member->type);
            evaluateTypeOfExpression(ast->members[i].value, ast->members[i].member->type);
            if (ast->members[i].value->type != ast->members[i].member->type) {
              logError(expr->pos, "The member '%s' in '%s' has type '%s', but the expression has type '%s'\n", ast->members[i].member->name, target->type.name, ast->members[i].member->type->name, ast->members[i].value->type->name);
              all_match = false;
              break;
            }
          }
          if (all_match) {
            expr->type = evidence;
          }
        } else {logError(expr->pos, "Cannot use struct initialization for non-struct type %s\n", evidence->name);}
      } else {logError(expr->pos, "Not enough type information top evaluate type of struct initialization\n");}
    }
  }
}

int main(int argc, char const *argv[]) {
  #ifdef DEBUG
    test();
  #endif
  (void) arenaPop;

  if (argc == 1) {
    logError(prev_pos, "Usage: %s <filename>\n", argv[0]);
    return 1;
  }
  else {
    next_pos.file = (char*) argv[1];
    file = fopen(argv[1], "r");
    if (!file) {
      logError(prev_pos, "Could not open file %s\n", argv[1]);
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
  initTypes();
  arenaInit(&perm_arena);
  arrayInit(&types, 16, sizeof(TypeAST*));
  addTypeDeclaration(&intType);
  addTypeDeclaration(&floatType);
  addTypeDeclaration(&voidType);
  arrayInit(&variables, 16, sizeof(VariableDeclarationAST*));
  arrayInit(&functions, 16, sizeof(FunctionDeclarationAST*));

  eatToken();
  while (1) {
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
                    } else { logError(prev_pos, "No type after member\n"); }
                  } else { logError(prev_pos, "No type after member. Did you forget a ':'?\n"); }
                } else { logError(prev_pos, "Expected member name, instead found %s\n", print_token()); }
                while (token != '}') {
                  eatToken();
                }
                eatToken();
                goto next;
              }

              if (success) {
                if (!type) {
                  str->num_members = arrayCount(&members);
                  str->members = pushArrayToArena(&members, &perm_arena);
                  addTypeDeclaration((TypeAST*) str);
                } else { logError(prev_pos, "You should not define a type of a type declaration (drop the type after ':')\n"); }
              }
              arrayFree(&members);
              goto next;
            } else {
              logError(pos_of_identifier, "Expected '{' after struct keyword\n");
            }


            goto next;
          }

          // TODO: switch function and expression checks
          // function?
          {
            FunctionDeclarationAST* fun = parseFunctionDeclaration(true);
            if (fun) {
              fun->name = name;
              fun->pos = pos_of_identifier;
              logDebugInfoAt(pos_of_identifier, "Function declaration for '%s' found\n", fun->name);
              if (type) {
                logError(pos_of_identifier, "Function declaration should not have type\n");
              }
              else if (!is_declaration) {
                logError(pos_of_identifier, "Cannot redefine a function. (did you forget to do ':=' instead of just '='?)");
              }
              // TODO: check if identifier already exists
              else {
                logDebugInfo("Found a function definition with name %s, and %i arguments\n", fun->name, fun->num_args);
                addFunctionDeclaration(fun);
              }
              goto next;
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
              var->pos = pos_of_identifier;
              var->name = name;
              var->type_name = type ? type : 0;
              var->value = value;
              var->type = 0;
              logDebugInfo("Found a variable declaration for %s with type %s and a value of type %i\n", var->name, var->type_name, var->value->expr_type);
              addVariableDeclaration(var);
              goto next;
            }
          }


          logError(pos_of_identifier, "Neither expression or function prototype found after declaration\n");
          goto next;
        }

        // declaration without value?
        if (is_declaration) {
          VariableDeclarationAST* var = arenaPush(&perm_arena, sizeof(VariableDeclarationAST));
          ZERO(var);
          var->name = name;
          var->type_name = type;
          logDebugInfo("Found a variable declaration for %s with type %s and no value\n", var->name, var->type_name);
          addVariableDeclaration(var);
          // TODO: default initialization here!
        }

        else {
          logError(prev_pos, "Unexpected token after ':' (variable declarations are not implemented yet - did you forget the '=' afterwards?)\n");
        }

      } break;

      case ';': {
        eatToken();
      } break;

      case TOK_EOF: {
        goto type_pass;
      } break;

      default: {
        logError(prev_pos, "Unexpected token %c\n", token);
        return 0;
      } break;
    }
    next:;
  }

  type_pass:;

  {
    TypeAST** it = arrayBegin(&types);
    for (int i = 0, len = arrayCount(&types); i < len; ++i, ++it) {
      if ((*it)->type == STRUCT) {
        evaluateTypeOfStruct((StructAST*) *it);
        logDebugInfo("Evaluated type of struct '%s'\n", (*it)->name);
      }
    }
  }

  {
    VariableDeclarationAST** it = arrayBegin(&variables);
    for (int i = 0, len = arrayCount(&variables); i < len; ++i, ++it) {
      evaluateTypeOfVariable(*it);
      if ((*it)->type) {
        logDebugInfo("%s has type '%s'\n", (*it)->name, (*it)->type->name);
      }
    }
  }

  {
    FunctionDeclarationAST** it = arrayBegin(&functions);
    for (int i = 0, len = arrayCount(&functions); i < len; ++i, ++it) {
      evaluateTypeOfFunction(*it);
      logDebugInfo("%s has return type %s\n", (*it)->name, (*it)->return_type->name);
    }
  }
  return 0;
}
