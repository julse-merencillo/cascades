## [0.1.1] (2024/11/02) - Divergence

### Changes
- Moved the `OpCode` and `Instruction` definitions to `Cascade.Runtime`
- `Instruction` is now `InstructionList`, and its definition has also changed
- Program code now uses `Data.Sequence` instead of a linked list
- Assembly code is no longer serialized; to run code, use `execute` in place of
  `interpretByteCode`
- Renamed `HALT` to `END`
- Renamed `PUSH` to `PSH`

### Removals
- Cleared `Cascade.Assembler`; a new assembler will be implemented soon

### Additions
- Added various branching instructions, as well as `DUP`, `MUL`, `DIV`, and
  `MOD`; for more information, check `README.md`
- Added the instruction pointer to `CPU`

## [0.0.1] (2024/10/15) - Foundation

*"A journey of a thousand miles begins with a single step." - Lao Tzu*

### Additions
- sample Cascade assembly code in the `app` directory
- Cascade's assembler, found in `Cascade.Assembler`
- Cascade's interpreter, found in `Cascade.Runtime`

[0.1.1]: https://github.com/julse-merencillo/cascades/releases/tag/0.1.1
[0.0.1]: https://github.com/julse-merencillo/cascades/releases/tag/0.0.1
