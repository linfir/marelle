#!/usr/bin/env python3
"""Times the programs of bench/: each compiled once, then run several times; prints the best
time of each and its output. With --offsets K, the library's code is also moved by 0, 8, ...,
8(K - 1) bytes, a link each, and the mean of the best times is printed with their range: where
a hot loop or jump falls relative to 32-byte boundaries moves a time by up to 15%."""

import argparse
import sys
import tempfile
import time
from pathlib import Path
from subprocess import run

from run import PipelineFailure, compile_source, lib_missing, run_stage

ROOT = Path(__file__).resolve().parent
BENCH = ROOT / "bench"


def assemble(source, obj):
    source_path = obj.with_suffix(".s")
    source_path.write_text(source)
    run_stage("as", ["as", "-o", obj, source_path])


def library_objects(offsets, work):
    """build/lib.o, or the library assembled with its code moved by 0, 8, ... bytes."""
    if offsets == 1:
        return [ROOT / "build" / "lib.o"]
    lib = (ROOT / "build" / "lib.s").read_text()
    i = lib.index("\n    .text\n")
    objects = []
    for k in range(offsets):
        obj = work / f"lib{k}.o"
        assemble(lib[:i] + "\n    .text\n" + "    nop\n" * (8 * k) + lib[i:], obj)
        objects.append(obj)
    return objects


def best_time(executable, runs):
    """The best wall-clock time of runs runs, and the output."""
    best = None
    for _ in range(runs):
        start = time.perf_counter()
        result = run([executable], capture_output=True, text=True, check=True)
        elapsed = time.perf_counter() - start
        best = elapsed if best is None else min(best, elapsed)
    return best, result.stdout.strip()


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("names", nargs="*", help="programs of bench/, without .ss (all by default)")
    parser.add_argument("-n", type=int, default=5, metavar="RUNS", help="runs of each program (5)")
    parser.add_argument("--offsets", type=int, default=1, metavar="K", help="library code offsets (1)")
    args = parser.parse_args()

    missing = lib_missing()
    if missing:
        sys.stderr.write(f"bench.py: {missing}\n")
        exit(1)

    paths = [BENCH / f"{name}.ss" for name in args.names] or sorted(BENCH.glob("*.ss"))
    with tempfile.TemporaryDirectory() as directory:
        work = Path(directory)
        libs = library_objects(args.offsets, work)
        for path in paths:
            try:
                obj = work / "out.o"
                assemble(compile_source(path.read_text()), obj)
                times = []
                for lib in libs:
                    executable = work / "out"
                    run_stage("ld", ["ld", "-o", executable, obj, lib])
                    seconds, output = best_time(executable, args.n)
                    times.append(seconds)
            except PipelineFailure as error:
                sys.stderr.write(f"{path.stem}: {error}\n{error.result.stderr}")
                exit(1)
            if len(output) > 40:
                output = output[:37] + "..."
            if len(times) == 1:
                print(f"{path.stem:10} {times[0]:7.3f} s  {output}")
            else:
                mean = sum(times) / len(times)
                print(f"{path.stem:10} {mean:7.3f} s ({min(times):.3f}-{max(times):.3f})  {output}")


if __name__ == "__main__":
    main()
