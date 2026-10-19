# marelle

Marelle is a Scheme compiler to x86-64 Linux, written in Chez Scheme.
It emits assembly and links static executables against its own runtime, with no libc.

- Closures, tail calls, `letrec`, named `let`, `do`, `case`, `define-values`
- Copying garbage collector
- Exact integers of any size (Karatsuba multiplication) and exact rationals
- Strings, symbols, chars, vectors, lists
- Register allocation and a small peephole pass

The runtime library (`lib/*.ss`) is written in the same Scheme, with unsafe low-level
defines (`declare unsafe`) for raw words, memory and syscalls.

## Requirements

- Chez Scheme (`scheme`)
- GNU binutils (`as`, `ld`)
- Python 3, for `run.py` and the tests

## Usage

```sh
make                      # compile the compiler and the runtime library
./run.py -e '(/ 6 4)'     # prints 3/2
./run.py program.scm      # compile, link and run a file
make test                 # the test suite, tests/
```

The value of the last form is printed.

## Layout

- `compiler/`: the compiler, as Chez libraries
- `lib/`: the runtime library, compiled once into `build/lib.o`
- `run`, `compile`: compiler entry points; `run.py` drives the whole pipeline
- `tests/`: one `.t` file per test, the program then its expected outcome

## License

MIT, see [LICENSE](LICENSE).
