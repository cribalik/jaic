#include "common.h"
#include "array.h"

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
  Register* stack;
  unsigned char *instr;
  unsigned char *program_start;
} VMState;
static VMState vm;


Instruction get_instr() {
  return *vm.instr++;
}

DataType get_regtype() {
  return *vm.instr++;
}

Register* get_reg() {
  Register *r;
  r = (Register*)vm.instr;
  vm.instr += sizeof(Register);
  return r;
}

Register* get_loc() {
  DataType t = get_regtype();
  switch (t) {
  case DATATYPE_NUM:
  case DATATYPE_NULL:
    vm_error("Got null instruction\n");
  case DATATYPE_I64:
  case DATATYPE_F64:
    return get_reg();
  case DATATYPE_STACK:
    return vm.stack - get_reg()->i64;
  }
  return 0;
}

unsigned char* get_addr() {
  return vm.program_start + get_reg()->u64;
}

typedef void (*VMFunPtr)(void);
static void vm_print() {
  printf("%i\n", (int)vm.stack[-1].i64);
}

static VMFunPtr vmfuns[] = {
  0,
  vm_print
};
STATIC_ASSERT(ARRAY_LEN(vmfuns) == VMFUN_NUM, all_vmfuns_defined);

static void run() {
  Register *dest, *src;

  vm.instr = get_addr();

  while (1) {
    switch (get_instr()) {
    case INSTR_NULL:
    case INSTR_NUM:
      vm_error("Unknown instruction");

    case INSTR_MV:
      dest = get_loc();
      src = get_loc();
      *dest = *src;
      break;

    case INSTR_SUBI:
      dest = get_loc();
      src = get_loc();
      dest->i64 -= src->i64;
      break;

    case INSTR_SUBF:
      dest = get_loc();
      src = get_loc();
      dest->f64 -= src->f64;
      break;

    case INSTR_PUSH:
      dest = get_loc();
      *vm.stack++ = *dest;
      break;

    case INSTR_POP:
      dest = get_loc();
      vm.stack -= dest->u64;
      break;

    case INSTR_CALL: {
      unsigned char *next = get_addr();
      vm.stack->u64 = (u64)vm.instr;
      ++vm.stack;
      vm.instr = next;
      break;
    }

    case INSTR_ECALL: {
      VMFun f = get_reg()->u64;
      if (f >= VMFUN_NUM)
        vm_error("Invalid host function %i\n", (int)f);
      vmfuns[f]();
      break;
    }

    case INSTR_EXIT:
      exit(0);
      break;

    case INSTR_RET:
      vm.instr = vm.program_start + vm.stack[-get_reg()->u64 - 1].u64;
      break;
    }
  }
}

int main(int argc, const char **argv) {
  --argc, ++argv;

  if (argc < 1)
    vm_error("Usage: zvm FILE\n");

  vm.program = file_open(argv[0]);
  vm.instr = (unsigned char*)vm.program.data;
  vm.program_start = vm.instr;
  vm.stack = malloc(sizeof(*vm.stack) * 1024);


  run();

  return 0;
}