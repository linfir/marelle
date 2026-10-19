#!/usr/bin/env python3
"""Static counts of the generated code, for the library (build/lib.s) and over every test that
compiles: instructions, Scheme calls (not tail calls), native calls (stubs and noalloc defs) and
frame words (the frame lengths of the functions)."""

import argparse
import re
import sys
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path

from run import PipelineFailure, compile_source, lib_missing

ROOT = Path(__file__).resolve().parent

FIELDS = ("instructions", "calls", "native", "frame words")


def counts(asm):
    """The counts of FIELDS in the assembly text asm."""
    instructions = calls = native = frame_words = 0
    text = False
    previous = ""
    for line in asm.splitlines():
        if line == "    .text" or line.startswith("    .section"):
            text = line == "    .text"
        elif line.startswith("    ") and not line.startswith("    ."):
            instructions += 1
            # A Scheme call stores its return label
            if re.match(r'    lea r11, qword ptr \[rip \+ "\.L.*_ret\d+"\]', line):
                calls += 1
            elif line.startswith("    call "):
                native += 1
        # A function's frame length is the quad just before its code
        elif text and line.endswith(":") and not line.startswith('".L'):
            m = re.match(r"    \.quad (\d+)$", previous)
            if m:
                frame_words += int(m.group(1))
        previous = line
    return (instructions, calls, native, frame_words)


def test_source(path):
    return path.read_text().split("\n---\n")[0]


def compile_counts(path):
    try:
        return counts(compile_source(test_source(path)))
    except PipelineFailure:
        return None


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("-v", "--verbose", action="store_true", help="also the counts of each test")
    args = parser.parse_args()

    missing = lib_missing()
    if missing:
        sys.stderr.write(f"stats.py: {missing}\n")
        exit(1)

    tests = sorted((ROOT / "tests").rglob("*.t"))
    with ThreadPoolExecutor() as executor:
        results = list(executor.map(compile_counts, tests))
    compiled = [(t, r) for t, r in zip(tests, results) if r is not None]

    print(f"{'':16}" + "".join(f"{f:>14}" for f in FIELDS))
    if args.verbose:
        for t, r in compiled:
            print(f"{str(t.relative_to(ROOT / 'tests')):16}" + "".join(f"{x:>14}" for x in r))
    print(f"{'library':16}" + "".join(f"{x:>14}" for x in counts((ROOT / "build" / "lib.s").read_text())))
    totals = [sum(r[i] for _, r in compiled) for i in range(len(FIELDS))]
    print(f"{f'{len(compiled)} tests':16}" + "".join(f"{x:>14}" for x in totals))


if __name__ == "__main__":
    main()
