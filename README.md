# Crust Programming Language
A small typed language, typed register VM, and embeddable runtime written in Rust. Source goes through lexing, parsing, and type checking, then lowers to a typed register IR with explicit calls, branches, and per-function register files.

The backend is designed to be consumed as a library: compile source into a `Module`, then execute it with an `Engine` that owns the host-function table and output sink. Cranelift JIT tier-up is the next step; the interpreter remains the fallback.

## Current Features

- Typed AST and type checking
- Integer and floating-point arithmetic
- `if` / `else` conditionals
- `for` loops with increment and decrement
- Function calls and recursive functions
- Variadic formatted printing with `{}` placeholders
- Typed register IR with disassembly
- Register VM interpreter with call frames
- Cranelift JIT tier-up: hot functions compile to native code, with interpreter fallback
- Host functions, including `print`, registered through the engine
- Type-error diagnostics
- Rust embedding API
- VM execution benchmarks

## Architecture

| Crate | Role |
| --- | --- |
| `crates/frontend` | Lexer, parser, AST, type checker, typed IR, host declarations |
| `crates/backend` | Typed register IR, code generation, interpreter, Cranelift tier-up, embedding API |
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

## Embedding In Rust

```rust
use crust_backend::{Engine, value::Value};
use crust_frontend::typed::{HostFnDecl, Type};

let mut engine = Engine::new();
engine.register_host(
    HostFnDecl {
        name: "answer".to_string(),
        params: vec![],
        variadic: false,
        return_type: Type::I64,
    },
    |_| Value::Int(42),
)?;

let module = engine.compile("fn main() -> void { print(\"{}\", answer()); }")?;
engine.execute(&module)?;
```

`Engine::with_output` accepts a callback so an embedding application can route `print` output anywhere. Host functions must be registered before compiling source that calls them, since the type checker needs their signatures.

## JIT Tier-Up

Calls are counted per function. After 64 calls (configurable with `Engine::set_jit_threshold`), an eligible function is compiled from the register IR by Cranelift. Each IR register becomes a Cranelift SSA variable; Cranelift's function builder inserts block parameters where a register is assigned on multiple paths. Compiled functions use a uniform native ABI and call back through a dispatch shim, so they can call interpreted functions and host functions without special cases. Functions using strings or host calls stay on the interpreter for now.

`Engine::call` invokes a compiled module function by name with runtime `Value` arguments, which is useful for both embedding and testing the JIT path.

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

The Python reference is [`benchmarks/recursion.py`](benchmarks/recursion.py). Result snapshots are in [`benchmarks/recursion-results.md`](benchmarks/recursion-results.md) and [`benchmarks/recursion-results-optimized.md`](benchmarks/recursion-results-optimized.md). These figures describe the previous stack VM and have not been re-measured on that reference machine. In the development orb, recursive `fib(30)` took about 0.32 s on the register interpreter and about 0.04 s once Cranelift tier-up compiled it.

## Roadmap

1. **JIT host calls and strings.** Allow compiled functions to call host functions and pass string constants, so `main`-style functions can be compiled too.
2. **Heap values.** Add tables/arrays and a small garbage collector while keeping the typed core.
3. **C ABI.** Wrap the engine for non-Rust hosts once the Rust API and value model settle.

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
