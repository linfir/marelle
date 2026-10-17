#!/usr/bin/env python3

"""Compare the object file of ./assemble with the one GNU as produces, for each test case the
compiler accepts: ./assemble reads the output of `schemec --dump program`, as that of schemec.
The objects are compared through binutils listings: the disassembly of .text with its relocations,
the other sections' contents and relocations, and the symbols."""

import argparse
import difflib
import os
import re
import subprocess
import sys
import tempfile
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path

from run import PipelineFailure, compile_source, lib_missing, run_stage, use_stage1
from tests import TEST_ROOT, selected_cases


ROOT = Path(__file__).resolve().parent
ASSEMBLE = ROOT / "assemble"

# The lines of a failure's diff shown
MAX_DIFF = 40

HEADER = re.compile(r"Contents of section (\S+):|RELOCATION RECORDS FOR \[(\S+)\]:")


def tool(*command):
    """=> the lines of the output of command, but the one naming the file."""
    lines = run_stage(command[0], command).stdout.splitlines()
    return [line for line in lines if line.strip() and "file format" not in line]


def data_sections(lines):
    """=> the lines of objdump -sr but those of .text, in the disassembly: the blocks in section
    order, the relocations sorted, since the sections and the relocations of as and ./assemble do
    not come in the same order."""
    blocks = []
    for line in lines:
        match = HEADER.fullmatch(line)
        if match:
            blocks.append([match[1] or match[2], line, []])
        else:
            blocks[-1][2].append(line)
    result = []
    for name, header, body in sorted(blocks):
        if name != ".text":
            result += [header, *(body[:1] + sorted(body[1:]) if header.startswith("RELOC") else body)]
    return result


def listing(obj):
    """=> the lines of obj that asmcheck compares."""
    return [
        *tool("objdump", "-drw", "-M", "intel", obj),
        *data_sections(tool("objdump", "-sr", obj)),
        *(line for line in tool("nm", "--format=sysv", obj) if "|" in line),
    ]


def compare(ours, theirs):
    """=> the start of the diff from the object of GNU as to that of ./assemble."""
    diff = list(difflib.unified_diff(listing(theirs), listing(ours), "as", "ours", lineterm="", n=2))
    if len(diff) > MAX_DIFF:
        diff[MAX_DIFF:] = [f"... {len(diff) - MAX_DIFF} more lines"]
    return diff


SKIP = "skip"


def check_case(path):
    """=> None, SKIP when the compiler rejects the case, or (stage, details)."""
    source = path.read_text()
    try:
        assembly = compile_source(source)
        dump = compile_source(source, "program")
    except PipelineFailure:
        return SKIP
    with tempfile.TemporaryDirectory() as directory:
        work = Path(directory)
        (work / "out.s").write_text(assembly)
        try:
            run_stage("as", ["as", "-o", work / "as.o", work / "out.s"])
        except PipelineFailure as error:
            return "as", [error.result.stderr.strip()]
        result = subprocess.run([ASSEMBLE], input=dump.encode(), capture_output=True)
        if result.returncode != 0:
            return "assemble", [result.stderr.decode().strip()]
        (work / "ours.o").write_bytes(result.stdout)
        try:
            diffs = compare(work / "ours.o", work / "as.o")
        except PipelineFailure as error:
            return "encoding", [f"unreadable object: {error.result.stderr.strip()}"]
    return ("encoding", diffs) if diffs else None


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("patterns", nargs="*", help="test name patterns")
    parser.add_argument("-v", "--verbose", action="store_true")
    parser.add_argument("--stage1", action="store_true", help="compile with build/stage1")
    args = parser.parse_args()
    if args.stage1:
        use_stage1()

    paths = selected_cases(args.patterns)
    if not paths:
        print("No tests matched")
        exit(2)
    missing = lib_missing()
    if missing:
        sys.stderr.write(f"asmcheck.py: {missing}\n")
        exit(1)

    failures = []
    skipped = 0
    with ThreadPoolExecutor(max_workers=os.cpu_count()) as pool:
        for path, failure in zip(paths, pool.map(check_case, paths)):
            name = path.relative_to(TEST_ROOT).as_posix()
            if failure == SKIP:
                skipped += 1
            elif failure:
                failures.append((name, failure))
            if args.verbose:
                print(f"{'SKIP' if failure == SKIP else 'FAIL' if failure else 'PASS'} {name}")
            else:
                print("s" if failure == SKIP else "F" if failure else ".", end="", flush=True)

    if not args.verbose:
        print()
    for name, (stage, details) in failures:
        print(f"\nFAIL {name} [{stage}]")
        for detail in details:
            print(f"  {detail}")

    passed = len(paths) - len(failures) - skipped
    summary = f"\n{len(paths)} cases: {passed} passed, {len(failures)} failed"
    if skipped:
        summary += f", {skipped} skipped"
    print(summary)
    if failures:
        exit(1)


if __name__ == "__main__":
    main()
