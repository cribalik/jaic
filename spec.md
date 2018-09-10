  INSTR_MV    <wloc> <rloc>
  INSTR_SUBI  <wloc> <rloc>
  INSTR_SUBF  <wloc> <rloc>
  INSTR_ADDI  <wloc> <rloc>
  INSTR_ADDF  <wloc> <rloc>
  INSTR_MULI  <wloc> <rloc>
  INSTR_MULF  <wloc> <rloc>
  INSTR_DIVI  <wloc> <rloc>
  INSTR_DIVF  <wloc> <rloc>
  INSTR_PUSH  <rloc>        # pushes the word <rloc> to the stack
  INSTR_PUSHN <rloc>        # pushes <rloc> words to stack
  INSTR_POP   <rloc>        # pops <rloc> words from stack
  INSTR_CALL  <uint>        # calls function at address <int>
  INSTR_ECALL <uint>        # calls host function with id <uint>
  INSTR_EXIT                # exits the program
  INSTR_RET   <uint>        # pops <uint> items off the stack and returns
