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
  Array(Register) stack;
  unsigned char *instr;
} VMState;
static VMState vm;


Instruction get_instr() {
  return *vm.instr++;
}

DataType get_regtype() {
  return *vm.instr++;
}

Register* get_loc() {
  DataType t = get_regtype();
  vm.instr += sizeof(Register);
  switch (t) {
  case DATATYPE_NUM:
  case DATATYPE_NULL: vm_error("Got null instruction\n");
  case DATATYPE_I64:
  case DATATYPE_F64:
    return ((Register*)vm.instr)-1;
  case DATATYPE_STACK:
    return vm.stack - (((Register*)vm.instr)-1)->i64;
  }
  return 0;
}

static void run() {
  Register *dest, *src;

  switch (get_instr()) {
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
    break;
  case INSTR_CALL:
    break;
  case INSTR_ECALL:
    break;
  default:
    vm_error("Invalid instruction %i\n", (int)*vm.instr);
  }
}

int main(int argc, const char **argv) {
  --argc, ++argv;

  if (argc < 1)
    vm_error("Usage: zvm FILE\n");

  vm.program = file_open(argv[0]);
  vm.instr = (unsigned char*)vm.program.data;
  vm.stack = 0;

  run();

  return 0;
}