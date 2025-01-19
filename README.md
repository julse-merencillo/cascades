# Cascades

## Notes on running/building this project
This project uses the Stack toolchain, and has only been tested with GHC 
version 9.6.6.

## About this project
This is a stack virtual machine I created to learn more about Haskell.
So far, there are 32 opcodes. Here is an overview:

- **Stack Manipulation**
  - `PSH Int` - Push a number onto the stack
  - `POP` - Remove a number from the stack
  - `DUP` - Duplicate the top number on the stack
- **Register, Memory, and Stack Operations**
  - `LVR Int` - Load a value to the register
  - `LMR Addr` - *Currently does nothing*
  - `SMR Addr` - *Currently does nothing*
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
  - `SHL Int` - Shift the bits of the top value to the left
  - `SHR Int` - Shift the bits of the top value to the right
  - `CNT` - Push the number of one bits in the top value
- **Branching Operations**
  - `JMP Addr` - Jump to the specified address
  - `JMS` - Jump to the specified address on top of the stack
  - `JMR` - Jump to the specified address on the register
  - `JZE Addr` - Jump if the top value is 0
  - `JNZ Addr` - Jump if the top value is not 0
  - `JEQ Addr` - Jump if the top value is equal to the register
  - `JNQ Addr` - Jump if the top value is not equal to the register
  - `JGT Addr` - Jump if the top value is greater than the register
  - `JLT Addr` - Jump if the top value is lesser than the register
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
- Read program files and run them
- Create a slightly higher level assembly language with labels and macros
- Implement text and graphics I/O

[andrevdm's implementation]: https://github.com/andrevdm/SimpleHaskellStackVM
