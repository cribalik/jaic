#include "common.hpp"
#include "array.hpp"

static void vm_error(const char *fmt, ...) {
  va_list args;

  fprintf(stderr, "%serror%s: ", RED, RESET);

  va_start(args, fmt);
  vfprintf(stderr, fmt, args);
  va_end(args);

  abort();
}

typedef struct VMState {
  File program;
  Word* stack;
  unsigned char *instr;
  unsigned char *program_start;
} VMState;
static VMState vm;


Instruction get_instr() {
  return (Instruction)*vm.instr++;
}

DataType get_regtype() {
  return (DataType)*vm.instr++;
}

Word* get_literal() {
  Word *r;
  r = (Word*)vm.instr;
  vm.instr += sizeof(Word);
  printf("got literal %lu\n", r->uint64);
  return r;
}

Word* get_value() {
  DataType t = get_regtype();
  switch (t) {
  case DATATYPE_NUM:
  case DATATYPE_NULL:
    vm_error("Got null instruction\n");
  case DATATYPE_LITERAL_I64:
  case DATATYPE_LITERAL_F64:
    return get_literal();
  case DATATYPE_STACK:
    printf("got stack value\n");
    Word *w = vm.stack - get_literal()->int64;
    printf("value on stack was %li\n", w->int64);
    return w;
  }
  return 0;
}

unsigned char* get_addr() {
  return vm.program_start + get_literal()->uint64;
}

typedef void (*VMFunPtr)(void);
static void vm_print() {
  printf("%i\n", (int)vm.stack[-1].int64);
}

static VMFunPtr vmfuns[] = {
  0,
  vm_print
};
STATIC_ASSERT(ARRAY_LEN(vmfuns) == VMFUN_NUM, all_vmfuns_defined);

static void run() {
  Word *dest, *src;

  vm.instr = get_addr();

  while (1) {
    switch (get_instr()) {
    case INSTR_NULL:
    case INSTR_NUM:
      vm_error("Unknown instruction");

    case INSTR_MV:
      puts("mv");
      dest = get_value();
      src = get_value();
      *dest = *src;
      break;

    case INSTR_SUBI:
      puts("subi");
      dest = get_value();
      src = get_value();
      dest->int64 -= src->int64;
      break;

    case INSTR_SUBF:
      puts("subf");
      dest = get_value();
      src = get_value();
      dest->f64 -= src->f64;
      break;

    case INSTR_ADDI:
      puts("addi");
      dest = get_value();
      src = get_value();
      dest->int64 += src->int64;
      break;

    case INSTR_ADDF:
      puts("addf");
      dest = get_value();
      src = get_value();
      dest->f64 += src->f64;
      break;

    case INSTR_MULI:
      puts("MULi");
      dest = get_value();
      src = get_value();
      dest->int64 *= src->int64;
      break;

    case INSTR_MULF:
      puts("MULf");
      dest = get_value();
      src = get_value();
      dest->f64 *= src->f64;
      break;

    case INSTR_DIVI:
      puts("DIVi");
      dest = get_value();
      src = get_value();
      dest->int64 /= src->int64;
      break;

    case INSTR_DIVF:
      puts("DIVf");
      dest = get_value();
      src = get_value();
      dest->f64 /= src->f64;
      break;

    case INSTR_PUSH:
      puts("push");
      dest = get_value();
      *vm.stack++ = *dest;
      break;

    case INSTR_PUSHN:
      puts("pushn");
      vm.stack += get_value()->uint64;
      break;

    case INSTR_POP:
      puts("pop");
      dest = get_value();
      vm.stack -= dest->uint64;
      break;

    case INSTR_CALL: {
      printf("function call\n");
      unsigned char *next = get_addr();
      vm.stack->uint64 = vm.instr - vm.program_start;
      ++vm.stack;
      printf("from %li\n", (i64)(vm.instr - vm.program_start));
      vm.instr = next;
      break;
    }

    case INSTR_ECALL: {
      puts("ecall");
      VMFun f = (VMFun)get_literal()->uint64;
      if (f >= VMFUN_NUM)
        vm_error("Invalid host function %i\n", (int)f);
      vmfuns[f]();
      break;
    }

    case INSTR_EXIT:
      exit(0);
      break;

    case INSTR_RET: {
      u64 st = get_literal()->uint64;
      vm.stack -= st;
      vm.instr = vm.program_start + vm.stack[-1].uint64;
      --vm.stack;
      printf("Return to %lu\n", vm.instr - vm.program_start);
      break;
    }
    }
  }
}

int main(int argc, const char **argv) {
  --argc, ++argv;

  if (argc < 1)
    vm_error("Usage: zvm FILE\n");

  puts("Starting Zen VM");

  vm.program = file_open(argv[0]);
  vm.instr = (unsigned char*)vm.program.data;
  vm.program_start = vm.instr;
  vm.stack = (Word*)malloc(sizeof(*vm.stack) * 1024);

  run();

  return 0;
}