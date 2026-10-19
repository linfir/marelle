#!/usr/bin/env python3
"""Times the programs of bench/: each compiled once, then run several times; prints the best
time of each and its output."""

import argparse
import sys
import tempfile
import time
from pathlib import Path
from subprocess import run

from run import PipelineFailure, compile_source, lib_missing, run_stage

ROOT = Path(__file__).resolve().parent
BENCH = ROOT / "bench"


def build(source, work):
    """The executable of source in the directory work."""
    assembly = work / "out.s"
    obj = work / "out.o"
    executable = work / "out"
    assembly.write_text(compile_source(source))
    run_stage("as", ["as", "-o", obj, assembly])
    run_stage("ld", ["ld", "-o", executable, obj, ROOT / "build" / "lib.o"])
    return executable


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
    args = parser.parse_args()

    missing = lib_missing()
    if missing:
        sys.stderr.write(f"bench.py: {missing}\n")
        exit(1)

    paths = [BENCH / f"{name}.ss" for name in args.names] or sorted(BENCH.glob("*.ss"))
    for path in paths:
        with tempfile.TemporaryDirectory() as directory:
            try:
                executable = build(path.read_text(), Path(directory))
            except PipelineFailure as error:
                sys.stderr.write(f"{path.stem}: {error}\n{error.result.stderr}")
                exit(1)
            seconds, output = best_time(executable, args.n)
        if len(output) > 40:
            output = output[:37] + "..."
        print(f"{path.stem:10} {seconds:7.3f} s  {output}")


if __name__ == "__main__":
    main()
