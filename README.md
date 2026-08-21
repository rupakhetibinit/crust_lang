# Crust Programming Language
A small typed language, bytecode compiler, and virtual machine written in Rust.

## Current Features

- Typed AST and type checking
- Integer and floating-point arithmetic
- `if` / `else` conditionals
- `for` loops with increment and decrement
- Function calls and recursive functions
- Variadic formatted printing with `{}` placeholders
- Bytecode disassembly
- Type-error diagnostics

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
