# Cascades

## Notes on running/building this project
*tba*

## About this project
This is a stack virtual machine I created to learn more about Haskell.
So far, there are 34 opcodes. Here is an overview:

- **Stack Manipulation**
  - `PSH` - Push a number onto the stack
  - `POP` - Remove a number from the stack
  - `DUP` - Duplicate the top number on the stack
- **Register, Memory, and Stack Operations**
  - `LVR` - Load a value to the register
  - `LDR` - Load a value in memory to the register
  - `STR` - Store the value of the register to memory
  - `PNT` - Change the value of the memory pointer
  - `INC` - Increment the memory pointer
  - `DEC` - Decrement the memory pointer
  - `SWP` - Swap the register and the top value of the stack
  - `RGS` - Move the value from the register to the stack
  - `SRG` - Move the top value from the stack to the register
- **Numeric Operations**
  - `ADD` - Pop two numbers `a` and `b` and push `b + a`
  - `SUB` - Pop two numbers `a` and `b` and push `b - a`
  - `MUL` - Pop two numbers `a` and `b` and push `b * a`
  - `DIV` - Pop two numbers `a` and `b` and push `b / a` if a is not 0
  - `MOD` - Pop two numbers `a` and `b` and push `b mod a` if a is not 0
- **Bitwise Operations**
  - `AND` - Pop two numbers `a` and `b` and push `b AND a`
  - `IOR` - Pop two numbers `a` and `b` and push `b OR a`
  - `XOR` - Pop two numbers `a` and `b` and push `b XOR a`
  - `NOT` - Pop the top value and push its complement
  - `SHL` - Shift the bits of the top value to the left
  - `SHR` - Shift the bits of the top value to the right
  - `CNT` - Push the number of one bits in the top value
- **Branching Operations**
  - `JMP` - Jump to the specified address
  - `JZE` - Jump if the top value is 0
  - `JNZ` - Jump if the top value is not 0
  - `JEQ` - Jump if the top value is equal to the register
  - `JNQ` - Jump if the top value is not equal to the register
  - `JGT` - Jump if the top value is greater than the register
  - `JLT` - Jump if the top value is lesser than the register
- **Other Operations**
  - `NOP` - Do nothing
  - `END` - Terminate the program
  - `ILL` - Illegal opcode for testing purposes

More opcodes to be implemented soon.

## Inspiration and Sources
This project was mainly inspired by [andrevdm's implementation] of a stack 
virtual machine. The code has since diverged massively and very few traces
remain.

## Future Plans
- Implement text and graphics I/O
- Read program files and run them
- Create a slightly higher level assembly language with labels and macros

[andrevdm's implementation]: https://github.com/andrevdm/SimpleHaskellStackVM
