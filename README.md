# marelle

Marelle is a Scheme compiler to x86-64 Linux, written in Scheme, running under Chez Scheme or
compiled by itself.
It writes static executables of its own runtime and the program, with no libc, no assembler and
no linker.

- Closures, tail calls, `letrec`, named `let`, `do`, `case`, `define-values`
- Copying garbage collector
- Exact integers of any size (Karatsuba multiplication) and exact rationals
- Strings, symbols, chars, vectors, lists
- Register allocation and a small peephole pass

The runtime library (`lib/*.ss`) is written in the same Scheme, with unsafe low-level
defines (`declare unsafe`) for raw words, memory and syscalls.

## Requirements

- Chez Scheme (`scheme`), to bootstrap the compiler
- Python 3, for `run.py` and the tests
- GNU binutils (`as`, `ld`), only to check the compiler against them (`make test-as`,
  `make test-asm`) and for `bench.py`

## Usage

```sh
make stage1               # the runtime library, then the compiler compiled by itself
make install              # build/stage1 as ~/.local/bin/marelle (PREFIX=...)
marelle prog.scm          # a program to the executable prog, from any directory
marelle -o out prog.scm   # to out
marelle --run prog.scm    # compile and run, writing no file
marelle --display prog.scm  # the executable prints the value of the last form
marelle -e '(/ 6 4)'      # compile and run the expression, printing 3/2
marelle -S prog.scm       # the asm; --pass PASS, the IR after a pass
./run.py -e '(/ 6 4)'     # prints 3/2
./run.py program.scm      # compile, link and run a file
./run.py --pass closure -e '(car (list 1))'  # the IR after a pass, without running
make test                 # the test suite, tests/
make fixpoint             # the compiler compiled by itself gives itself again
./bench.py                # the best of 5 runs of each program of bench/
./bench.py --offsets 8    # the mean over 8 positions of the library's code
make bench                # the benchmarks against Chez Scheme
./stats.py                # instructions, calls and frame words of the library and tests
```

`run.py` and the tests compile with `--display`.

## Layout

- `compiler/`: the compiler, as Chez libraries
- `lib/`: the runtime library, compiled once into `build/lib.img`, at fixed addresses
- `schemec`: the compiler's entry point, Scheme to executables; `run.py` drives the whole pipeline
- `flatten`: `schemec` and `compiler/` as one program, for self-hosting
- `tests/`: per test, the program `NAME.scm` and its expected outcome `NAME.toml`
- `bench/`: benchmark programs, timed by `bench.py`

## License

MIT, see [LICENSE](LICENSE).
