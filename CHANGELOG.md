## [0.2.1] (2025/01/19) - Remembrance

### Changes
- Overall changes to program execution. A few notable highlights:
  - Execution history is no longer stored as a list. A print statement must be
    used in its place for logging.
  - Program execution now occurs in the IO monad, as more operations involving
    I/O will be implemented in the future.
  - Slight refactor and renames to highlight the fetch-decode-execute cycle.
  - To run programs, one must invoke `runProgram` instead of `execute`
- Changed the behavior of `JEQ`, `JNQ`, `JGT` and `JLT` to compare with the
  newly added register.
- Renamed `CPU` to `ExecUnit`. 
- Replaced the `state` field in `ExecUnit` with the `instr` field, which
  contains the currently executed instruction.
- Renamed `emptyCPU` to `initState`.

### Removals
- Removed `JQV`, `JNV`, `JGV` and `JLV`. With the addition of the register,
  these opcodes were unnecessary.
- Removed the dependency for `Data.Text` in `Cascade.Runtime`. This may be
  reverted in the future.

### Additions
- Added the `reg` field in `ExecUnit`, which represents a program register.
- Added `LVR`, `RGS`, `SRG` and `JMR`, which involves the register.
- Added `LMR` and `SMR`, though these two instructions currently do nothing.
- With a complete refactoring of program execution, added more sensible runtime
  error handling with `RuntimeError` and `Status`, indicating the current
  status of the program.

## [0.1.2] (2024/11/22) - Twofold

### Additions
- Added bitwise instructions `AND`, `IOR`, `XOR`, `NOT`, `SHL`, `SHR` and `CNT`

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
- Added various branching instructions, as well as `DUP`, `MUL`, `DIV` and
  `MOD`; for more information, check `README.md`
- Added the instruction pointer to `CPU`

## [0.0.1] (2024/10/15) - Foundation

*"A journey of a thousand miles begins with a single step." - Lao Tzu*

### Additions
- sample Cascade assembly code in the `app` directory
- Cascade's assembler, found in `Cascade.Assembler`
- Cascade's interpreter, found in `Cascade.Runtime`

[0.2.1]: https://github.com/julse-merencillo/cascades/releases/tag/0.2.1
[0.1.2]: https://github.com/julse-merencillo/cascades/releases/tag/0.1.2
[0.1.1]: https://github.com/julse-merencillo/cascades/releases/tag/0.1.1
[0.0.1]: https://github.com/julse-merencillo/cascades/releases/tag/0.0.1
