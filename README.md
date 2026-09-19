# Simple-Python

A simplified Python-like programming language, implemented from scratch in **Racket** — complete with its own lexer, parser and interpreter — plus a small **Tkinter** desktop IDE for writing and running programs in it.

The goal of the project is to design and implement a reduced but coherent subset of Python's syntax and semantics (functions, conditionals, loops, lists, boolean logic, arithmetic, `print`, etc.) as a language of its own, built on top of Racket's `parser-tools` lexer/parser generators and modeled with the environment/store (denotational-semantics) approach from *Essentials of Programming Languages* (EOPL).

## Contents

| File | Description |
|---|---|
| `lexer.rkt` | Tokenizer for the language, built with `parser-tools/lex`. Recognizes numbers, identifiers, keywords (`pass`, `break`, `continue`, `return`, `global`, `def`, `if`, `else`, `for`, `in`, `and`, `or`, `not`, `True`, `False`, `None`), operators (`=`, `==`, `<`, `<=`, `>`, `>=`, `+`, `-`, `*`, `/`, `**`), punctuation (`( ) [ ] : , ;`), and whitespace. |
| `parser.rkt` | An LALR grammar (`parser-tools/yacc`) that turns the token stream into an AST, covering statements, functions, `if`/`else`, `for`-`in` loops, boolean/comparison/arithmetic expression precedence, function calls, list literals and indexing. |
| `datatypes.rkt` | All AST node types and runtime value types, defined with EOPL's `define-datatype` (programs, statements, expressions at every precedence level, environments, expression values, lazy-evaluation "thunks", etc.). |
| `env_store.rkt` | The mutable store (a reference-cell heap: `newref`/`deref`/`setref!`) and the variable environment (`extend-env`/`apply-env`), following the EOPL store-passing interpreter model. Also defines the built-in `print` function binding that is seeded into every environment. |
| `eopl_errors.rkt` | Pretty-printers that turn AST nodes back into readable source text, and the interpreter's error reporters (unbound identifier, invalid left-hand side, divide-by-zero, index out of bounds, invalid type/cast, too many arguments, etc.). |
| `interpreter.rkt` | The tree-walking evaluator (`value-of-*` functions) that gives meaning to every statement and expression, including lazy (thunked) expression evaluation, function calls/returns, `global`, `for` loops with `break`/`continue`, and short-circuiting `and`/`or`. Exposes `run` (evaluate a source string) and `execute` (evaluate a source file). |
| `execute.rkt` | Command-line entry point. Parses a `-a`/`--argument` flag containing the program source and runs it through the interpreter. |
| `IDE.py` | A small Tkinter GUI text editor/IDE for the language: new/open/save/save-as, cut/copy/paste, zoom in/out, and a "Run Code" button that shells out to `execute.rkt` via Racket and prints the program's output. |
| `IDE.exe` | A prebuilt Windows executable of `IDE.py` (packaged with PyInstaller) for running the IDE without a Python installation. |
| `document.pdf` | Language reference: the full list of supported statements, expressions and syntax with explanations/examples. |

## Features

- **Statements**: variable assignment, `global`, `return` (with or without a value), `pass`, `break`, `continue`, function definitions (`def`, with or without parameters), `if`/`else`, `for ... in ...` loops.
- **Expressions**: boolean logic with short-circuit `and`/`or`/`not`, comparisons (`==`, `<`, `<=`, `>`, `>=`), arithmetic (`+`, `-`, `*`, `/`, `**`) with standard precedence, function calls, list literals, and list indexing (`list[i]`).
- **Data types**: numbers, booleans (`True`/`False`), `None`, and lists (including list concatenation via `+`).
- **Built-in `print`** function, bound automatically in every program's global environment.
- **User-defined functions** with parameters, return values, and recursion, evaluated with lazy (thunked) argument/expression evaluation.
- **Store-based interpreter**: variables live in a mutable reference store rather than being substituted directly, in the style of EOPL's denotational interpreters.
- **Two ways to run programs**: a command-line entry point (`execute.rkt`) and a graphical desktop IDE (`IDE.py` / `IDE.exe`) with file management and a one-click "Run Code" action.
- **Descriptive runtime errors**: unbound identifiers, invalid assignment targets, divide-by-zero, out-of-bounds list indexing, type mismatches, invalid casts, and argument-count mismatches are all reported with readable messages that echo back the offending source expression.

## Tech stack

- **[Racket](https://racket-lang.org/)** — the language and interpreter are implemented in `#lang racket`.
  - `parser-tools` (`lex`, `lex-sre`, `yacc`) — lexer and LALR parser generators.
  - `eopl` — the EOPL teaching library, used for `define-datatype`/`cases` pattern matching on the AST and for the environment/store model.
  - `try-catch` — a third-party Racket package used for exception handling inside the interpreter (e.g. falling back to creating a new binding when a variable isn't found yet).
- **Python 3** with **Tkinter** (standard library) — the desktop IDE, which shells out to Racket via `subprocess` to execute code.

## Prerequisites

- [Racket](https://download.racket-lang.org/) (any recent version with `parser-tools` and `eopl` available; both ship with the standard Racket distribution).
- The `try-catch` Racket package:
  ```bash
  raco pkg install try-catch
  ```
- Python 3 with Tkinter available (only needed to run the GUI IDE from source — `tkinter` ships with most standard CPython installs). On Windows, `IDE.exe` can be used instead without installing Python.
- Racket itself must be on your `PATH` as `racket`, since both `IDE.py` and `execute.rkt`'s workflow invoke it as a subprocess/command-line tool.

## Usage

### Run a program from the command line

`execute.rkt` takes the program source as a string via `-a`/`--argument`:

```bash
racket execute.rkt -a "x = 5; print(x);"
```

### Run a program from the Racket REPL

You can also load the interpreter directly and use `run` (source string) or `execute` (source file):

```racket
#lang racket
(require "interpreter.rkt")
(run "x = 1; y = 2; print(x + y);")
```

### Use the graphical IDE

```bash
python IDE.py
```

This opens a text editor where you can write, open and save simplified-Python programs (`.spy` files), then click **Run Code** (or use the bottom status bar) to execute the current buffer through Racket and see the output in a console window. On Windows, `IDE.exe` can be run directly without a Python installation.

### Example program

```python
def factorial(n):
    if n <= 1:
        return 1;
    else:
        return n * factorial(n - 1);
    ;
;

x = factorial(5);
print(x);

for i in [1, 2, 3]:
    print(i);
;
```

Every statement is terminated with `;` (including the closing statement of a block), matching the grammar defined in `parser.rkt`. See `document.pdf` for the complete, authoritative syntax reference with further examples.

## Project structure

```
simple-python/
├── lexer.rkt          # Tokenizer
├── parser.rkt          # Grammar / AST construction
├── datatypes.rkt        # AST + runtime value type definitions (EOPL)
├── env_store.rkt        # Environment + mutable store, built-in print binding
├── eopl_errors.rkt       # AST pretty-printing + error reporters
├── interpreter.rkt       # Tree-walking evaluator (run/execute entry points)
├── execute.rkt          # CLI entry point
├── IDE.py             # Tkinter GUI IDE (source)
├── IDE.exe             # Prebuilt Windows IDE executable
└── document.pdf          # Language reference / documentation
```

## Notable implementation details

- **Lazy evaluation via thunks**: assigning an expression to a variable stores an `expression-thunk` (the expression plus its defining environments) rather than an immediately computed value; the value is only forced the first time the variable is read, then cached back into the store.
- **EOPL-style environment/store separation**: variables are bound to store references (`newref`/`deref`/`setref!`) rather than holding values directly, matching the interpreter architecture taught in *Essentials of Programming Languages*.
- **Functions carry their own closure environment** (`function-thunk`) and, on each call, the current environment is folded into a new global environment so recursive and nested calls resolve correctly.
- **`print` is not a language keyword** — it is a function value seeded into the base environment (`env_store.rkt`), and the interpreter special-cases calls to it to `display` their argument.
- **GUI and interpreter are decoupled**: `IDE.py` never touches the Racket source directly — it writes a temporary shell script that calls `execute.rkt -a "<code>"` and shells out to it, so the IDE only needs Racket to be installed and callable from the command line.
