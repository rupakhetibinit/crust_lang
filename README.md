# Crust Programming Language
A small typed language, bytecode compiler, and stack virtual machine written in Rust. Source goes through lexing, parsing, type checking, and bytecode code generation, then executes on a register-light stack VM with call frames.

## Current Features

- Typed AST and type checking
- Integer and floating-point arithmetic
- `if` / `else` conditionals
- `for` loops with increment and decrement
- Function calls and recursive functions
- Variadic formatted printing with `{}` placeholders
- Bytecode disassembly
- Type-error diagnostics
- Bytecode VM execution benchmarks

## Architecture

| Crate | Role |
| --- | --- |
| `crates/frontend` | Lexer, parser, AST, type checker, typed IR |
| `crates/backend` | Bytecode chunk format, code generation, VM |
| `crates/crust` | CLI (`run` / `disassemble`) and Criterion benches |

## Run The Compiler

Run a sample from the repository root:

```powershell
cargo run -- run crates/crust/samples/compiler/arithmetic.crust
```

Disassemble a sample's bytecode:

```powershell
cargo run -- disassemble crates/crust/samples/compiler/loop.crust
```

Build and run the optimized binary:

```powershell
cargo build --release -p crust
target/release/crust.exe run crates/crust/samples/compiler/recursion.crust
```

## Compiler Samples

All current samples are in `crates/crust/samples/compiler`:

| Sample | Demonstrates |
| --- | --- |
| `arithmetic.crust` | Arithmetic precedence, locals, formatted output |
| `conditionals.crust` | `if` / `else` and comparisons |
| `loop.crust` | `for` loops, local increments, formatted output |
| `recursion.crust` | Recursive factorial and function calls |
| `printing.crust` | Multiple formatted print arguments |
| `type_error.crust` | Intentional type-checking failure |

## Benchmarks

Recursive Fibonacci(30), mean run time (hyperfine-style summary in `benchmarks/`):

| Implementation | Mean | Min | Max | Relative |
| --- | ---: | ---: | ---: | ---: |
| Crust optimized VM | 159.3 ± 13.2 ms | 143.8 ms | 202.3 ms | 1.00 |
| Python 3.14 | 254.0 ± 23.9 ms | 215.4 ms | 333.3 ms | 1.59× slower |

Baseline before VM optimization (`benchmarks/recursion-results.md`): Crust release VM 406.8 ± 24.8 ms vs Python 3.14 250.6 ± 31.5 ms.

The Python reference is [`benchmarks/recursion.py`](benchmarks/recursion.py). Result snapshots are in [`benchmarks/recursion-results.md`](benchmarks/recursion-results.md) and [`benchmarks/recursion-results-optimized.md`](benchmarks/recursion-results-optimized.md).

## VHS Demos

VHS source tapes are in `demo/source`. Rendered GIFs are in `demo/output`:

- `run.gif`
- `disassemble.gif`
- `conditionals.gif`
- `loop.gif`
- `recursion.gif`
- `printing.gif`
- `type_error.gif`
- `benchmark.gif`

Regenerate one demo from the repository root:

```powershell
vhs demo/source/recursion.tape
```

Regenerate all demos:

```powershell
Get-ChildItem demo/source/*.tape | ForEach-Object { vhs $_.FullName }
```

Validate tape syntax without rendering:

```powershell
vhs validate "demo/source/*.tape"
```

The tapes use `bat -P` to show source files and repository-root-relative paths, so they should be run from the repository root.
