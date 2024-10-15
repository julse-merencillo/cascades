# Cascades

## About this project
This is a stack virtual machine I created to learn more about Haskell. 
Currently, the only instructions implemented are:
- `NOP`: Do nothing
- `HALT`: Stop execution
- `PUSH`: Push a value to the stack
- `POP`: Pop a value to the stack
- `ADD`: Pop two values `a` and `b` and push `b + a` to the stack
- `SUB`: Pop two values `a` and `b` and push `b - a` to the stack

There are no branching instructions, so the code currently runs linearly.

## Inspiration and Sources
This project was mainly inspired by [andrevdm's implementation] of a stack 
virtual machine. 

## Future Plans
- Add branching instructions
- Read program files and run them
- Create a slightly higher level assembly language with labels and macros
- Implement text and graphics I/O

[andrevdm's implementation]: https://github.com/andrevdm/SimpleHaskellStackVM
