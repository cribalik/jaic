#define MEM_IMPLEMENTATION
#include "mem.h"
#include "array.h"
#include "common.h"
#include <string.h>
#include <errno.h>
#include <ctype.h>
#include <assert.h>

#if 1
  #define DEBUG_BUILD
#endif

#ifdef DEBUG_BUILD
  #define DEBUG(stmt) do {stmt;} while(0);
#else
  #define DEBUG(stmt)
#endif


typedef struct Type Type;
typedef union Node Node;
typedef struct ParserState ParserState;
typedef struct NodeHeader NodeHeader;

typedef enum NodeType {
  NODE_TYPE_NULL,

  NODE_TYPE_ASSIGNMENT,
  NODE_TYPE_POINTER,
  NODE_TYPE_ARRAY,
  NODE_TYPE_FLOAT,
  NODE_TYPE_INTEGER,
  NODE_TYPE_STRING,
  NODE_TYPE_CALL_EXPR,
  NODE_TYPE_VARIABLE_REF_EXPR,
  NODE_TYPE_MEMBER_ACCESS_EXPR,
  NODE_TYPE_BINOP_EXPR,
  NODE_TYPE_ARRAY_SUBSCRIPT_EXPR,
  NODE_TYPE_INITIALIZER,
  NODE_TYPE_INITIALIZER_ARG,
  NODE_TYPE_ADDRESSOF_AST,
  NODE_TYPE_COMPOUND,
  NODE_TYPE_LOOP,
  NODE_TYPE_STRUCT,
  NODE_TYPE_MEMBER,
  NODE_TYPE_VAR,
  NODE_TYPE_FN,
  NODE_TYPE_PARAMETER,
  NODE_TYPE_RETURN,
  NODE_TYPE_TAG,

  NODE_TYPE_COUNT
} NodeType;

struct NodeHeader {
  NodeType type;
  FilePos file_pos;
  Node *next; /* for lists */
};

typedef enum FunctionFlag {
  FUNCTION_VARARG = 1 << 0,
  FUNCTION_FOREIGN = 1 << 1
} FunctionFlag;

union Node {
  NodeHeader head;

  struct {
    NodeHeader head;
    Node *statements;
  } compound;

  struct {
    NodeHeader head;
    Node *value;
  } retur;

  struct {
    NodeHeader head;
    Node *body;
    Node *from, *to;
    char *iter_name;
    char *index_name;
  } loop;

  struct {
    NodeHeader head;
    char *name;
    Node *body;
    Node *parameters;
    int id;
    FunctionFlag flags;
    Node *return_type;
  } function;

  struct {
    NodeHeader head;
    char *name;
    Node *type;
  } parameter;

  struct {
    NodeHeader head;
    char *name;
    Node *members;
  } struc;

  struct {
    NodeHeader head;
    char *name;
    Node *type;
  } member_decl;

  struct {
    NodeHeader head;
    Node *base;
    char *name;
  } member_access;

  struct {
    NodeHeader head;
    Node *lhs;
    Node *rhs;
    int op;
  } bin;

  struct {
    NodeHeader head;
    Node *base;
    Node *arguments;
  } call;

  struct {
    NodeHeader head;
    char *name;
    Node *value;
  } argument;

  struct {
    NodeHeader head;
    Node *lhs;
    Node *rhs;
  } assignment;

  struct {
    NodeHeader head;
    char *name;
    Node *type;
    Node *value;
  } var;

  struct {
    NodeHeader head;
    Node *base;
    Node *subscript;
  } array_subscript;

  struct {
    NodeHeader head;
    double value;
  } float_literal;

  struct {
    NodeHeader head;
    long value;
  } int_literal;

  struct {
    NodeHeader head;
    char *value;
  } string_literal;

  struct {
    NodeHeader head;
    char *name;
  } var_ref;

  struct {
    NodeHeader head;
    Node *size;
    Node *subtype;
  } array;

  struct {
    NodeHeader head;
    Node *subtype;
  } pointer;

  struct {
    NodeHeader head;
    char *name;
    Node *target;
  } tag;

};

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

  STRING_TYPE,

  STRUCT_TYPE,

  ARRAY_TYPE,
  STATIC_ARRAY_TYPE,

  POINTER_TYPE,

  FUNCTION_TYPE
} TypeClass;

struct Type {
  TypeClass type;
};

struct ParserState {
  Type builtin_types[19];

  /* AST */
  Node *root;
  LBlock(Node) nodes;
  Node void_node;

  /* tokenizer */
  int token;

  char *tok_start, *tok_cursor;

  char *tok_identifier;
  int tok_identifier_len;

  char *tok_string;
  int tok_string_len;

  int tok_integer;

  float tok_float;

  LStack identifier_mem;

  /* current file */
  FileCache *file_cache;
  File file;
};
static ParserState parser;

static void parser_init(FileCache *cache, File *file) {
  parser.file_cache = cache;
  parser.file = *file;

  parser.builtin_types[VOID_TYPE].type = VOID_TYPE;
  parser.builtin_types[INT_TYPE].type = INT_TYPE;
  parser.builtin_types[FLOAT_TYPE].type = FLOAT_TYPE;
  parser.builtin_types[F32_TYPE].type = F32_TYPE;
  parser.builtin_types[F64_TYPE].type = F64_TYPE;
  parser.builtin_types[U8_TYPE].type = U8_TYPE;
  parser.builtin_types[U16_TYPE].type = U16_TYPE;
  parser.builtin_types[U32_TYPE].type = U32_TYPE;
  parser.builtin_types[U64_TYPE].type = U64_TYPE;
  parser.builtin_types[I8_TYPE].type = I8_TYPE;
  parser.builtin_types[I16_TYPE].type = I16_TYPE;
  parser.builtin_types[I32_TYPE].type = I32_TYPE;
  parser.builtin_types[I64_TYPE].type = I64_TYPE;
  parser.builtin_types[USIZE_TYPE].type = USIZE_TYPE;
  parser.builtin_types[STRING_TYPE].type = STRING_TYPE;

  lblock_init(&parser.nodes, 4096, sizeof(Node));

  parser.root = lblock_get(&parser.nodes);
  parser.root->head.type = NODE_TYPE_COMPOUND;

  lstack_init(&parser.identifier_mem, 4096);

  parser.tok_cursor = parser.tok_start = parser.file.data;
}

static FilePos get_filepos() {
  FilePos p;
  p.offset = (int)(parser.tok_start - parser.file.data);
  p.name = parser.file.name;
  return p;
}

static void parse__error(const char *fmt, ...) {
  va_list args;

  va_start(args, fmt);
  filepos_print(parser.file_cache, get_filepos(), "error", RED, fmt, args);
  va_end(args);

  #ifdef DEBUG_BUILD
    abort();
  #else
    exit(1);
  #endif
}

#ifdef DEBUG_BUILD
  #define parse_error parse__error
#else
  #define parse_error parse__error
#endif

static Node* node_alloc() {
  Node *n = lblock_get(&parser.nodes);
  memset(n, 0, sizeof(*n));
  n->head.file_pos = get_filepos();
  return n;
}

static void node_free(Node *n) {
  lblock_put(&parser.nodes, n);
}

static char* identifier_alloc() {
  char *s;

  if (parser.tok_identifier_len > parser.identifier_mem.block_size)
    parse_error("Identifier length (%i) exeeded limit (%i)\n", parser.tok_identifier_len, parser.identifier_mem.block_size);

  s = lstack_push_ex(&parser.identifier_mem, parser.tok_identifier_len+1, 1);
  memcpy(s, parser.tok_identifier, parser.tok_identifier_len);
  s[parser.tok_identifier_len] = 0;
  return s;
}

static char* string_alloc() {
  char *s;

  if (parser.tok_string_len > parser.identifier_mem.block_size)
    parse_error("Identifier length (%i) exeeded limit (%i)\n", parser.tok_string_len, parser.identifier_mem.block_size);

  s = lstack_push_ex(&parser.identifier_mem, parser.tok_string_len+1, 1);
  memcpy(s, parser.tok_string, parser.tok_string_len);
  s[parser.tok_string_len] = 0;
  return s;
}

typedef enum Token {
  TOKEN_EOF = -1,
  TOKEN_IDENTIFIER = -3,
  TOKEN_FLOAT = -4,
  TOKEN_INT = -5,
  TOKEN_UNKNOWN = -6,
  TOKEN_ARROW = -7,
  TOKEN_STRUCT = -8,
  TOKEN_LOOP = -9,
  TOKEN_DOUBLEDOT = -10,
  TOKEN_TRIPLEDOT = -11,
  TOKEN_RETURN = -12,
  TOKEN_FN = -13,
  TOKEN_VAR = -14,
  TOKEN_DOUBLE_EQUALS = -15,
  TOKEN_STRING = -16
} Token;

static char* token_as_string() {
  static char buffer[2] = {0};
  char *s;
  Token t;

  if (parser.token >= 0) {
    buffer[0] = parser.token;
    return buffer;
  }

  t = parser.token;
  switch (t) {
    case TOKEN_FLOAT:
      return "<float>";
    case TOKEN_INT:
      return "<int>";
    case TOKEN_UNKNOWN:
      return "<unknown>";
    case TOKEN_STRUCT:
      return "struct";
    case TOKEN_LOOP:
      return "for";
    case TOKEN_RETURN:
      return "return";
    case TOKEN_FN:
      return "fn";
    case TOKEN_STRING:
      return "<string>";
    case TOKEN_DOUBLE_EQUALS:
      return "==";
    case TOKEN_IDENTIFIER:
      s = malloc(parser.tok_identifier_len+1);
      memcpy(s, parser.tok_identifier, parser.tok_identifier_len);
      s[parser.tok_identifier_len] = 0;
      return s;
    case TOKEN_EOF:
      return "<eof>";
    case TOKEN_ARROW:
      return "->";
    case TOKEN_VAR:
      return "var";
    case TOKEN_DOUBLEDOT:
      return "..";
    case TOKEN_TRIPLEDOT:
      return "...";
  }
  return "<unknown>";
}

static int token_identifier_eq(const char *str) {
  if ((int)strlen(str) != parser.tok_identifier_len)
    return 0;
  return !memcmp(parser.tok_identifier, str, parser.tok_identifier_len);
}

static void token_next() {

  if (parser.tok_cursor >= parser.file.end)
    parse_error("Unexpected end of file\n");

  while (isspace(*parser.tok_cursor))
    ++parser.tok_cursor;

  /* check for comments */
  while (*parser.tok_cursor == '/') {
    ++parser.tok_cursor;
    if (*parser.tok_cursor == '/') {
      do {
        ++parser.tok_cursor;
      } while (*parser.tok_cursor && *parser.tok_cursor != '\n' && *parser.tok_cursor != '\r');
      if (!*parser.tok_cursor) {
        parser.token = TOKEN_EOF;
        goto done;
      }
      goto endofcomment;
    }
    else if (*parser.tok_cursor == '*') {
      int depth = 1;
      ++parser.tok_cursor;
      while (1) {
        if (!*parser.tok_cursor)
          parse_error("File ended while still in block comment\n");

        if (*parser.tok_cursor == '*') {
          ++parser.tok_cursor;
          if (*parser.tok_cursor == '/') {
            --depth;
            if (depth == 0) {
              ++parser.tok_cursor;
              goto endofcomment;
            }
          }
        }
        else if (*parser.tok_cursor == '/') {
          ++parser.tok_cursor;
          depth += *parser.tok_cursor == '*';
        }
        ++parser.tok_cursor;
      }
    }
    else {
      parser.token = '/';
      goto done;
    }
    endofcomment:
    while (isspace(*parser.tok_cursor))
      ++parser.tok_cursor;
  }

  parser.tok_start = parser.tok_cursor;

  /* identifier */
  if (isalpha(*parser.tok_cursor)) {
    while (isalpha(*parser.tok_cursor) || isdigit(*parser.tok_cursor) || *parser.tok_cursor=='_')
      ++parser.tok_cursor;

    parser.tok_identifier = parser.tok_start;
    parser.tok_identifier_len = parser.tok_cursor - parser.tok_start;
    if (token_identifier_eq("struct")) {
      parser.token = TOKEN_STRUCT;
      goto done;
    }
    if (token_identifier_eq("loop")) {
      parser.token = TOKEN_LOOP;
      goto done;
    }
    if (token_identifier_eq("for")) {
      parser.token = TOKEN_LOOP;
      goto done;
    }
    if (token_identifier_eq("return")) {
      parser.token = TOKEN_RETURN;
      goto done;
    }
    if (token_identifier_eq("fn")) {
      parser.token = TOKEN_FN;
      goto done;
    }
    if (token_identifier_eq("var")) {
      parser.token = TOKEN_VAR;
      goto done;
    }

    parser.token = TOKEN_IDENTIFIER;
    goto done;
  }

  /* string literal */
  else if (*parser.tok_cursor == '"') {
    ++parser.tok_cursor;
    while (*parser.tok_cursor != '"' || parser.tok_cursor[-1] == '\\') {
      if (!*parser.tok_cursor)
        parse_error("Found end of file while reading string\n");
      ++parser.tok_cursor;
    }

    parser.tok_string = parser.tok_start+1;
    parser.tok_string_len = parser.tok_cursor - parser.tok_string;
    parser.token = TOKEN_STRING;
    ++parser.tok_cursor;
    goto done;
  }

  /* number literal */
  else if (isdigit(*parser.tok_cursor)) {
    char tmp;

    while (isdigit(*parser.tok_cursor))
      ++parser.tok_cursor;

    /* integer */
    if (*parser.tok_cursor != '.') {
      tmp = *parser.tok_cursor;
      *parser.tok_cursor = '\0';
      parser.tok_integer = atoi(parser.tok_start);
      *parser.tok_cursor = tmp;

      /* TODO: check for suffixes here e.g. 10u32 */
      parser.token = TOKEN_INT;
      goto done;
    }

    /* float */
    ++parser.tok_cursor;
    if (!isdigit(*parser.tok_cursor))
      parse_error("Expected digit after '.'\n");

    while (isdigit(*parser.tok_cursor))
      ++parser.tok_cursor;

    /* TODO: check for suffixes here e.g. 30.0f */
    tmp = *parser.tok_cursor;
    *parser.tok_cursor = '\0';
    parser.tok_float = strtod(parser.tok_start, 0);
    *parser.tok_cursor = tmp;

    parser.token = TOKEN_FLOAT;
    goto done;
  }

  /* eof */
  else if (!*parser.tok_cursor) {
    parser.token = TOKEN_EOF;
    goto done;
  }

  /* arrow */
  else if (parser.tok_cursor[0] == '-' && parser.tok_cursor[1] == '>') {
    parser.tok_cursor += 2;
    parser.token = TOKEN_ARROW;
    goto done;
  }

  /* triple dots */
  else if (parser.tok_cursor[0] == '.' && parser.tok_cursor[1] == '.' && parser.tok_cursor[2] == '.') {
    parser.tok_cursor += 3;
    parser.token = TOKEN_TRIPLEDOT;
    goto done;
  }

  /* double dots */
  else if (parser.tok_cursor[0] == '.' && parser.tok_cursor[1] == '.') {
    parser.tok_cursor += 2;
    parser.token = TOKEN_DOUBLEDOT;
    goto done;
  }

  else {
    parser.token = *parser.tok_cursor++;
    goto done;
  }

  parser.token = TOKEN_EOF;

  done:;
}

static int token_peek() {
  int t;
  char *prev;

  prev = parser.tok_start;

  token_next();
  t = parser.token;

  parser.tok_cursor = prev;
  token_next();

  return t;
}

static Node* parse_block();
static Node* parse_statement();
static Node* parse_expression();

static Node* parse_block() {
  Node *n;
  Node **stmt;
  if (parser.token != '{')
    parse_error("Expected '{' as start of block, but got %s.\n", token_as_string());
  token_next();

  n = node_alloc();
  n->head.type = NODE_TYPE_COMPOUND;
  stmt = &n->compound.statements;

  while (1) {
    if (parser.token == '}') {
      token_next();
      break;
    }

    *stmt = parse_statement();
    if (!*stmt)
      continue;
    stmt = &(*stmt)->head.next;
  }

  return n;
}

static Node* parse_statement() {
  Node *n;
  if (parser.token == TOKEN_EOF)
    return 0;
  if (parser.token == ';') {
    token_next();
    return 0;
  }

  n = node_alloc();

  switch (parser.token) {

  case TOKEN_IDENTIFIER:
    if (token_peek() != ':')
      break;

    n->head.type = NODE_TYPE_TAG;
    n->tag.name = identifier_alloc();
    token_next();
    if (parser.token == ';') {
      token_next();
      return n;
    }
    n->tag.target = parse_statement();
    return n;

  case TOKEN_LOOP:
    n->head.type = NODE_TYPE_LOOP;
    /* TODO: size_t */
    token_next();


    if (parser.token != TOKEN_IDENTIFIER && parser.token != '_')
      parse_error("Expected a name for element in list. For example 'for x : list {...}'\n");

    if (parser.token == TOKEN_IDENTIFIER)
      n->loop.iter_name = identifier_alloc();
    token_next();

    if (parser.token == ',') {
      token_next();
      if (parser.token != TOKEN_IDENTIFIER)
        parse_error("Expected index name\n");

      n->loop.index_name = identifier_alloc();
      token_next();
    }

    if (parser.token != ':')
      parse_error("Expected ':'\n");
    token_next();

    n->loop.from = parse_expression();

    /* range loop? */
    if (parser.token == TOKEN_DOUBLEDOT) {
      token_next();
      n->loop.to = parse_expression();
    }

    n->loop.body = parse_block();
    return n;

  case TOKEN_RETURN:
    n->head.type = NODE_TYPE_RETURN; 
    token_next();

    if (parser.token == ';') {
      token_next();
      return n;
    }

    n->retur.value = parse_expression();

    if (parser.token != ';')
      parse_error("Expected ';' after return statement\n");
    token_next();
    return n;

  case TOKEN_FN: {
    static int function_id = 0;
    Node **arg;

    n->head.type = NODE_TYPE_FN;
    n->function.id = function_id++;
    token_next();

    if (parser.token != TOKEN_IDENTIFIER)
      parse_error("Expected function name, found %s\n", token_as_string());
    n->function.name = identifier_alloc();
    token_next();

    /* arguments */
    if (parser.token != '(')
      parse_error("Token %s not start of a function prototype\n", token_as_string());
    token_next();

    arg = &n->function.parameters;
    while (1) {
      if (parser.token == ')') {
        token_next();
        break;
      }

      /* vararg? */
      if (parser.token == TOKEN_TRIPLEDOT) {
        n->function.flags |= FUNCTION_VARARG;
        token_next();
        if (parser.token != ')')
          parse_error("Vararg must be at the end of parameter list\n");
        token_next();
        break;
      }

      if (parser.token != TOKEN_IDENTIFIER)
        parse_error("Identifier expected in parameter list, found %s\n", token_as_string());

      *arg = node_alloc();
      (*arg)->head.type = NODE_TYPE_PARAMETER;
      (*arg)->parameter.name = identifier_alloc();
      
      token_next();

      if (parser.token != ':')
        parse_error("No type found after parameter name (did you forget a ':'?\n");
      token_next();

      (*arg)->parameter.type = parse_expression();
      arg = &(*arg)->head.next;

      /* TODO: default parameters */

      if (parser.token == ',') {
        token_next();
        continue;
      }

      if (parser.token == ')') {
        token_next();
        break;
      }

      parse_error("Expected ',' or ')' after parameter\n");
    }

    /* return value */
    if (parser.token == ':') {
      token_next();
      n->function.return_type = parse_expression();
    } else {
      n->function.return_type = &parser.void_node;
    }

    /* Function body */

    if (parser.token == '#') {
      token_next();
      if (parser.token != TOKEN_IDENTIFIER || !token_identifier_eq("foreign"))
        parse_error("Invalid compiler directive, did you mean to use 'foreign' ?\n");
      token_next();
      n->function.flags |= FUNCTION_FOREIGN;
      return n;
    }

    if ((n->function.flags & FUNCTION_VARARG) && !(n->function.flags & FUNCTION_FOREIGN))
      parse_error("Varags is only implemented for foreign functions\n");

    n->function.body = parse_block();

    return n;
  }

  /* struct? */
  case TOKEN_STRUCT: {
    Node **member;
    n->head.type = NODE_TYPE_STRUCT;
    token_next();

    if (parser.token != TOKEN_IDENTIFIER)
      parse_error("Expected struct name, got %s", token_as_string());
    n->struc.name = identifier_alloc();
    token_next();

    if (parser.token != '{')
      parse_error("Expected '{' after struct keyword\n");
    token_next();

    /* members */
    member = &n->struc.members;
    while (1) {
      if (parser.token == '}') {
        token_next();
        break;
      }

      if (parser.token != TOKEN_IDENTIFIER)
        parse_error("Expected member name, instead found %s\n", token_as_string());

      *member = node_alloc();
      (*member)->head.type = NODE_TYPE_MEMBER;
      (*member)->member_decl.name = identifier_alloc();
      token_next();
      if (parser.token != ':')
        parse_error("No type after member. Did you forget a ':'?\n");
      token_next();

      (*member)->member_decl.type = parse_expression();

      if (parser.token != ';')
        parse_error("Expected ';' after member\n");
      token_next();

      member = &(*member)->head.next;

      /* TODO: parse default value */
    }

    return n;
  }

  /* Variable declaration? */
  case TOKEN_VAR:
    n->head.type = NODE_TYPE_VAR;
    token_next();

    if (parser.token != TOKEN_IDENTIFIER)
      parse_error("Expected variable name, got token %s\n", token_as_string());
    n->var.name = identifier_alloc();
    token_next();

    /* type? */
    if (parser.token == ':') {
      token_next();
      n->var.type = parse_expression();
    }

    /* assignment? */
    if (parser.token == '=') {
      token_next();
      n->var.value = parse_expression();
    }

    /* if no assignment, was there at least a type declaration? */
    if (!n->var.value && !n->var.type)
      parse_error("Can't infer type of variable. Need type declaration or initial value\n");

    if (parser.token != ';')
      parse_error("Expected ';' after variable declaration\n");
    token_next();

    return n;

  default:
    break;
  }

  /* expression is default */
  node_free(n);
  n = parse_expression();

  /* assignment? */
  if (parser.token == '=') {
    Node *a;

    token_next();

    a = node_alloc();
    a->head.type = NODE_TYPE_ASSIGNMENT;
    a->head.file_pos = n->head.file_pos;
    a->assignment.lhs = n;
    a->assignment.rhs = parse_expression();

    n = a;
  }

  if (parser.token != ';')
    parse_error("Expected ';' after expression\n");
  token_next();
  /* just an expression */
  return n;
}

static Node* parse_primitive() {
  Node *n;

  n = node_alloc();
  switch (parser.token) {
    case TOKEN_FLOAT:
      n->head.type = NODE_TYPE_FLOAT;
      n->float_literal.value = parser.tok_float;
      token_next();
      return n;

    case TOKEN_INT:
      n->head.type = NODE_TYPE_INTEGER;
      n->int_literal.value = parser.tok_integer;
      token_next();
      return n;

    case TOKEN_STRING:
      n->head.type = NODE_TYPE_STRING;
      n->string_literal.value = string_alloc();
      token_next();
      return n;

    case TOKEN_IDENTIFIER:
      n->head.type = NODE_TYPE_VARIABLE_REF_EXPR;
      n->var_ref.name = identifier_alloc();
      token_next();
      return n;

    case '(':
      token_next();
      node_free(n);
      n = parse_expression();
      if (parser.token != ')')
        parse_error("Did not find matching ')'\n");
      token_next();
      return n;

    #if 0
    case '{': {
      StructInitializationAST* ast;
      eatToken();
      ast = arena_push(&perm_arena, sizeof(StructInitializationAST));
      ast->expr.expr_type = STRUCT_INIT_EXPR;
      ast->expr.stmt.pos = prev_pos;
      if (parser.token != '}') {
        DynArray members = array_create(4, sizeof(MemberInitializationAST));
        while (1) {
          MemberInitializationAST* member = array_push(&members);
          int success = 0;
          if (parser.token == TOKEN_IDENTIFIER) {
            member->name = arena_push_string(&perm_arena, g_identifier_str);
            eatToken();
            if (parser.token == ':') {
              eatToken();
              member->value = parseExpression();
              if (member->value) {
                if (parser.token == ',') {
                  eatToken();
                  success = 1;
                } else if (parser.token == '}') {
                  eatToken();
                  break;
                } else {
                  zen_log_at(ERROR, prev_pos, "Expected ',' or '}', but found %s\n", print_token());
                }
              } else {zen_log_at(ERROR, prev_pos, "Failed to parse expression\n");}
            } else {zen_log_at(ERROR, prev_pos, "Expected '=' after member name, found %s\n", print_token());}
          } else {zen_log_at(ERROR, prev_pos, "Expected identifier, instead found %s\n", print_token());}

          if (!success) {
            /* we failed, eat everything to matching brace */
            while (parser.token != '}') {
              eatToken();
            }
            eatToken();
            ast = 0;
            break;
          }
        }
        if (ast) {
          ast->members = arena_push_array(&members, &perm_arena);
          ast->num_members = array_count(&members);
        }
        array_free(&members);
      } else {
        eatToken();
      }
      return (ExpressionAST*)ast; }

    case '@': {
      AddressOfAST* result;
      eatToken();
      result = arena_push(&perm_arena, sizeof(AddressOfAST));
      *result = create_addressof_ast();
      result->base = parseExpression();
      return (ExpressionAST*)result; }
    #endif

    default:
      parse_error("Expected expression, but got %s\n", token_as_string());
      return 0;
  }
}

static int pre_op_precedence(int op) {
  switch (op) {
    case '[': return 30;
    case '^': return 30;
    case '-': return 10;
    default: return 0;
  }
}

static int post_op_precedence(int op) {
  switch (op) {
    case '.': return 50;
    case '[': return 40;
    case '(': return 40;
    case '*': return 20;
    case '/': return 20;
    case '%': return 20;
    case '-': return 10;
    case '+': return 10;
    case '<': return 5;
    case '>': return 5;
    default: return 0;
  }
}

static int operator_is_binary(int op) {
  return op == '*' || op == '/' || op == '-' || op == '+' || op == '<' || op == '>' || op == TOKEN_DOUBLE_EQUALS || op == '%';
}

static Node* parse__expression(int precedence) {
  Node *n;
  int new_precedence;

  /* prefix operators */
  new_precedence = pre_op_precedence(parser.token);
  if (new_precedence > precedence)
    precedence = new_precedence;

  if (parser.token == '[') {
    n = node_alloc();
    n->head.type = NODE_TYPE_ARRAY;
    token_next();
    if (parser.token != ']')
      n->array.size = parse_expression();
    if (parser.token != ']')
      parse_error("Expected closing bracket, found %s\n", token_as_string());
    token_next();
    n->array.subtype = parse__expression(new_precedence);
    return n;
  }

  if (parser.token == '^') {
    n = node_alloc();
    n->head.type = NODE_TYPE_POINTER;
    token_next();
    n->pointer.subtype = parse__expression(new_precedence);
    return n;
  }

  n = parse_primitive();

  /* postfix operators */

  while (1) {
    new_precedence = post_op_precedence(parser.token);
    if (new_precedence == 0 || new_precedence < precedence)
      return n;
    precedence = new_precedence;

    /* binary? */
    if (operator_is_binary(parser.token)) {
      Node *b;
      b = node_alloc();
      b->head.type = NODE_TYPE_BINOP_EXPR;
      b->head.file_pos = n->head.file_pos;
      b->bin.lhs = n;
      b->bin.op = parser.token;
      token_next();
      b->bin.rhs = parse__expression(new_precedence);
      n = b;
    }

    /* member access */
    else if (parser.token == '.') {
      Node *m;
      m = node_alloc();
      m->head.type = NODE_TYPE_MEMBER_ACCESS_EXPR;
      m->member_access.base = n;
      token_next();

      if (parser.token != TOKEN_IDENTIFIER)
        parse_error("Expected member name after '.', but got %s\n", token_as_string());
      m->member_access.name = identifier_alloc();
      token_next();

      n = m;
    }

    /* function call */
    else if (parser.token == '(') {
      Node *f;
      Node **argument;
      f = node_alloc();
      f->head.type = NODE_TYPE_CALL_EXPR;
      f->call.base = n;
      token_next();

      /* get args */
      argument = &f->call.arguments;
      while (1) {
        if (parser.token == ')') {
          token_next();
          break;
        }

        /* field assignment? */
        *argument = node_alloc();
        (*argument)->head.type = NODE_TYPE_PARAMETER;

        if (parser.token == TOKEN_IDENTIFIER && token_peek() == ':') {
          (*argument)->argument.name = identifier_alloc();
          token_next();

          if (parser.token != ':')
            parse_error("Expected ':', instead found %s\n", token_as_string());
          token_next();
        }
        (*argument)->argument.value = parse_expression();

        argument = &(*argument)->head.next;

        if (parser.token == ',') {
          token_next();
          continue;
        }
        if (parser.token == ')') {
          token_next();
          break;
        }
        parse_error("Expected ')' or ',' in parameter list, but found %s\n", token_as_string());
      }

      n = f;
    }

    /* array access */
    else if (parser.token == '[') {
      Node *a;
      a = node_alloc();
      a->head.type = NODE_TYPE_ARRAY_SUBSCRIPT_EXPR;
      a->array_subscript.base = n;
      token_next();

      a->array_subscript.subscript = parse_expression();
      if (parser.token != ']')
        parse_error("Found no matching ']'\n");
      token_next();

      n = a;
    }
  }

  return n;
}

static Node* parse_expression() {
  return parse__expression(0);
}

static void do_parse_step() {
  Node **stmt;

  stmt = &parser.root->compound.statements;

  token_next();

  while (1) {
    if (parser.token == TOKEN_EOF)
      break;

    *stmt = parse_statement();
    if (!*stmt)
      continue;

    stmt = &(*stmt)->head.next;
  }
}

int parse(FileCache *cache, File *file) {

  parser_init(cache, file);

  do_parse_step();

  /** Type inference-step **/
  #if 0
  do_type_inference(global_scope);
  if (found_error) {
    zen_log(ERROR, "Exiting due to errors\n");
    return 1;
  }
  #endif

  return 0;
}
