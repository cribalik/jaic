#define DEBUG

#include "terminal.c"
#include "memarena.c"
#include "utils.c"
#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

typedef enum {
  TOK_EOF = -1,
  TOK_IDENTIFIER = -3,
  TOK_FLOAT = -4,
  TOK_INT = -5,
  TOK_UNKNOWN = -6,
} Token;

// values set by the tokenizer
static char* identifierStr;
static double floatVal;
static int intVal;

// the input source file
static FILE* file;

static int curr_char = ' ';
static int gettok() {
  #define BUFFER_SIZE 512
  static char buf[BUFFER_SIZE];

  while (isspace(curr_char)) {
    curr_char = getc(file);
  }

  // check for comments
  while (curr_char == '/') {
    curr_char = getc(file);
    if (curr_char == '/') {
      do {
        curr_char = getc(file);
      } while (curr_char != EOF && curr_char != '\n' && curr_char != '\r');
      if (curr_char == EOF) {
        return TOK_EOF;
      }
      goto endofcomment;
    }
    else if (curr_char == '*') {
      int depth = 1;
      curr_char = getc(file);
      while (1) {
        if (curr_char == '*') {
          curr_char = getc(file);
          if (curr_char == '/') {
            --depth;
            if (depth == 0) {
              curr_char = getc(file);
              goto endofcomment;
            }
          }
        }
        else if (curr_char == '/') {
          curr_char = getc(file);
          if (curr_char == '*') {
            ++depth;
          }
        } else if (curr_char == EOF) {
          printf("File ended while still in block comment\n");
          return TOK_UNKNOWN;
        }
        curr_char = getc(file);
      }
    }
    else {
      return '/';
    }
    endofcomment:
    while (isspace(curr_char)) {curr_char = getc(file);};
  }

  if (isalpha(curr_char)) {
    int i = 0;
    for (i = 0; i < BUFFER_SIZE && (isalpha(curr_char) || isdigit(curr_char)); ++i) {
      buf[i] = curr_char;
      curr_char = getc(file);
    }
    if (i == BUFFER_SIZE) {
      printf("Reached identifier size limit\n");
      return TOK_UNKNOWN;
    }
    else {
      buf[i] = '\0';
      identifierStr = buf;
      return TOK_IDENTIFIER;
    }
  }

  else if (isdigit(curr_char)) {
    int i;
    for (i = 0; i < BUFFER_SIZE && isdigit(curr_char); ++i) {
      buf[i] = curr_char;
      curr_char = getc(file);
    }

    if (i == BUFFER_SIZE) {
      logError("Reached max size for int\n");
    }
    else if (curr_char == '.') {
      curr_char = getc(file);
      for (; i < BUFFER_SIZE && isdigit(curr_char); ++i) {
        buf[i] = curr_char;
        curr_char = getc(file);
      }

      if (i == BUFFER_SIZE) {
        logError("Reached max size for float number\n");
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

  else if (curr_char == EOF) {
    return TOK_EOF;
  }

  else {
    int r = curr_char;
    curr_char = getc(file);
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
    default:
      buffer[0] = token;
      return buffer;
  }
  UNREACHABLE;
}

static void eatToken() {
  token = gettok();
  printf("%s\n", print_token());
}

typedef enum {
  INT,
  FLOAT,
} Type;

typedef struct {
  TypeAST parent;
  // TODO: members
} StructAST;

typedef enum {
  FLOAT_AST,
  INT_AST,
  CALL_AST,
  VARIABLE_AST,
} ExpressionType;

typedef struct {
  ExpressionType type;
} ExpressionAST;

/*
typedef struct {
  char* name;
  char* type;
} ArgumentAST;
*/

typedef struct {
  char* name;
  int num_args;
  char* args; // continuous strings of name1,type1, name2,type2, ...
  // TODO: body??
} FunctionDeclarationAST;

typedef struct {
  char* name;
  char* type_name;
  TypeAST* type;
  ExpressionAST* value;
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
  int num_input;
  ExpressionAST* input;
} CallAST;

static int test() {
  // arrayTest();
  return 0;
}

#define MAIN_MEMORY_SIZE 64*1024*1024

static MemArena main_arena;
static MemArena to_resolve;

static char* pushString(MemArena* arena, char* str) {
  int len = strlen(str) + 1;
  char* result = arenaPush(arena, len);
  memcpy(result, str, len);
  return result;
}

/* sort(array, (a,b) a+b) */
static FunctionDeclarationAST* parseFunction(bool try) {
  void* start = arenaGetCurrent(main_arena);
  FunctionDeclarationAST* result = arenaPush(&main_arena, sizeof(FunctionDeclarationAST));
  if (token != '(') {
    if (!try) {
      logError("token %s not start of a function prototype\n", print_token(token));
    }
    arenaPopTo(&main_arena, start);
    return 0;
  }

  int num_args = 0;
  result->args = arenaGetCurrent(main_arena);
  eatToken();
  while (1) {

    if (token == ')') {
      eatToken();
      break;
    }

    ++num_args;

    if (token != TOK_IDENTIFIER) {
      if (!try) {
        logError("Identifier expected in parameter list, found %c\n", token);
      }
      arenaPopTo(&main_arena, start);
      return 0;
    }

    char* name = pushString(&main_arena, identifierStr);
    eatToken();
    logDebug("Found parameter %s\n", name);

    if (token == ':') {
      eatToken();
      if (token != TOK_IDENTIFIER) {
        if (!try) {
          logError("Type expected after ':'\n");
        }
        arenaPopTo(&main_arena, start);
        return 0;
      }
      char* type = pushString(&main_arena, identifierStr);
      eatToken();
      logDebug(" - with type %s\n", type);
    }

    // TODO: default parameters

    if (token == ',') {
      eatToken();
    }
    else if (token == ')') {
      eatToken();
      break;
    }
    else {
      logDebug("Expected ',' or ')' after parameter\n");
      arenaPopTo(&main_arena, start);
      return 0;
    }
  }
  result->num_args = num_args;

  // TODO: return value

  if (token != '{') {
    if (!try) {
      logError("No '{' found after function parameter list\n");
    }
    return 0;
  }
  eatToken();

  int depth = 1;
  while (depth > 0) {
    if (token == '{') ++depth;
    else if (token == '}') --depth;
    else if (token == TOK_EOF) {
      if (!try) {
        logError("'{' never matched with a '}'");
      }
      exit(1);
    }
    eatToken();
  }

  return result;
}

static ExpressionAST* parseExpression() {
  void* result = 0;
  switch (token) {
    case TOK_FLOAT: {
      FloatAST* ast = arenaPush(&main_arena, sizeof(FloatAST));
      ast->parent.type = FLOAT_AST;
      ast->value = floatVal;
      eatToken();
      result = ast;
      logDebug("Found a float literal with value %f\n", ast->value);
    } break;

    case TOK_INT: {
      IntAST* ast = arenaPush(&main_arena, sizeof(IntAST));
      ast->parent.type = INT_AST;
      ast->value = intVal;
      eatToken();
      result = ast;
      logDebug("Found an int literal with value %i\n", ast->value);
    } break;

    case TOK_IDENTIFIER: {
      byte* start = arenaGetCurrent(main_arena);
      char* name = pushString(&main_arena, identifierStr);
      eatToken();
      if (token == '(') {
        eatToken();
        logError("Function calls are not supported yet\n");
        int depth = 1;
        while (depth) {
          if (token == '(') ++depth;
          else if (token == ')') --depth;
          else if (token == TOK_EOF) {
            logError("Brace in function call never matched\n");
            break;
          }
          eatToken();
        }
        arenaPopTo(&main_arena, start);
      }
      else {
        VariableGetAST* ast = arenaPush(&main_arena, sizeof(VariableGetAST));
        ast->parent.type = VARIABLE_AST;
        ast->name = name;
        result = ast;
        logDebug("Found a variable get with name %s\n", name);
      }
    } break;

    default: {
      logError("Expression expected, but found neither a literal, function call or a variable name, instead found %c\n", token);
    } break;
  }
  return (ExpressionAST*) result;
}

#define pushPosition fpos_t _position; fgetpos(file, &_position); logDebug("Pushing position %li\n", *((long int*) &_position));
#define popPosition fsetpos(file, &_position); curr_char = ' '; logDebug("Jumping back to position %li\n", *((long int*) &_position));

int main(int argc, char const *argv[]) {
  (void) test;
  (void) arenaPop;

  if (argc == 1) {
    logError("Usage: %s <filename>\n", argv[0]);
  }
  else {
    file = fopen(argv[1], "r");
    if (!file) {
      logError("Could not open file %s\n", argv[1]);
      return 1;
    }
  }

  arenaInit(&main_arena);

  eatToken();
  while (1) {
    switch (token) {

      case TOK_IDENTIFIER: {
        char* name = pushString(&main_arena, identifierStr);
        char* type = 0;
        bool is_definition = false;
        eatToken();

        if (token == ':') {
          is_definition = true;
          eatToken();
          if (token == TOK_IDENTIFIER) {
            type = pushString(&main_arena, identifierStr);
            eatToken();
          }
        }

        if (token == '=') {
          FunctionDeclarationAST* fun;
          ExpressionAST* value;
          pushPosition;
          eatToken();

          fun = parseFunction(true);
          if (fun) {
            fun->name = name;
            if (type) {
              logError("Function declaration should not have type");
            }
            else if (!is_definition) {
              logError("Cannot redefine a function. (did you forget to do ':=' instead of just '='?)");
            }
            // TODO: check if identifier already exists
            else {
              logDebug("Found a function definition with name %s, and %i arguments\n", fun->name, fun->num_args);
            }
            goto next;
          }

          popPosition;
          eatToken();
          value = parseExpression();
          if (value) {
            VariableDeclarationAST* ast = arenaPush(&main_arena, sizeof(VariableDeclarationAST));
            ast->name = name;
            ast->type = type ? type : 0;
            ast->value = value;
            // logDebug("Found a variable declaration for %s with type %s and a value of type %s\n", ast->name, ast->type, ast->value->type);
            logDebug("Found a variable declaration for %s with type %s and a value of type %i\n", ast->name, ast->type, ast->value->type);
            goto next;
          }

          logError("Neither expression or function prototype found after declaration\n");
          goto next;
        }

        if (is_definition) {
          VariableDeclarationAST* ast = arenaPush(&main_arena, sizeof(VariableDeclarationAST));
          ast->name = name;
          ast->type_name = type;
          logDebug("Found a variable declaration for %s with type %s and no value\n", ast->name, ast->type);
        }

        else {
          logError("Unexpected token after ':' (variable declarations are not implemented yet - did you forget the '=' afterwards?)\n");
        }

      } break;

      case ';': {
        eatToken();
      } break;

      default: {
        return 0;
      } break;
    }
    next:;
  }
  return 0;
}
