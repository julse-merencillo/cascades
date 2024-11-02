# Cascades

## Notes on running/building this project
This project uses the Stack toolchain, and has only been tested with GHC 
version 9.6.6.

## About this project
This is a stack virtual machine I created to learn more about Haskell.
So far, there are 23 opcodes. Here is an overview:

- **Stack Manipulation**
  - `PSH Int` - Push a number onto the stack
  - `POP` - Remove a number from the stack
  - `DUP` - Duplicate the top number on the stack
- **Numeric Operations**
  - `ADD` - Pop two numbers `a` and `b` and push `b + a`
  - `SUB` - Pop two numbers `a` and `b` and push `b - a`
  - `MUL` - Pop two numbers `a` and `b` and push `b * a`
  - `DIV` - Pop two numbers `a` and `b` and push `b / a` if a is not 0
  - `MOD` - Pop two numbers `a` and `b` and push `b mod a` if a is not 0
- **Branching Operations**
  - `JMP Addr` - Jump to the specified address
  - `JMS` - Jump to the specified address on top of the stack
  - `JZE Addr` - Jump if the top value on the stack is 0
  - `JNZ Addr` - Jump if the top value is not 0
  - `JEQ Addr` - Jump if the top value is equal to the value beneath it
  - `JNQ Addr` - Jump if the top value is not equal to the value beneath it
  - `JGT Addr` - Jump if the top value is greater than the value beneath it
  - `JLT Addr` - Jump if the top value is lesser than the value beneath it
  - `JQV Int Addr` - Jump if the top value is equal to the given value
  - `JNV Int Addr` - Jump if the top value is not equal to the given value
  - `JGV Int Addr` - Jump if the top value is greater than the given value
  - `JLV Int Addr` - Jump if the top value is lesser than the given value
- **Other Operations**
  - `NOP` - Do nothing
  - `END` - Terminate the program
  - `ILL` - Illegal opcode for testing purposes

More opcodes to be implemented soon.

## Inspiration and Sources
This project was mainly inspired by [andrevdm's implementation] of a stack 
virtual machine. The code has since diverged massively, but certain traces
still remain.

## Future Plans
- Read program files and run them
- Create a slightly higher level assembly language with labels and macros
- Implement text and graphics I/O

[andrevdm's implementation]: https://github.com/andrevdm/SimpleHaskellStackVM
