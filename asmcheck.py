#!/usr/bin/env python3

"""Compare the object file of the compiler's encoder (`schemec --obj`) with the one GNU as
produces from its assembly, for each test case the compiler accepts, or for the library (--lib)
or a program file (--program, e.g. build/flat.ss).
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

import run
from run import PipelineFailure, compile_source, lib_missing, run_stage, use_stage1
from tests import TEST_ROOT, selected_cases


ROOT = Path(__file__).resolve().parent

# The lines of a failure's diff shown
MAX_DIFF = 40

HEADER = re.compile(r"Contents of section (\S+):|RELOCATION RECORDS FOR \[(\S+)\]:")


def tool(*command):
    """=> the lines of the output of command, but the one naming the file."""
    lines = run_stage(command[0], command).stdout.splitlines()
    return [line for line in lines if line.strip() and "file format" not in line]


def data_sections(lines):
    """=> the lines of objdump -sr but those of .text, in the disassembly: the blocks in section
    order, the relocations sorted, since the sections and the relocations of as and the encoder do
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
    """=> the start of the diff from the object of GNU as to that of the encoder."""
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
    except PipelineFailure:
        return SKIP
    with tempfile.TemporaryDirectory() as directory:
        obj = Path(directory) / "ours.o"
        try:
            run_stage("compiler", [*run.compiler, "--obj", obj], input=source, cwd=ROOT)
        except PipelineFailure as error:
            return "encoder", [error.result.stderr.strip()]
        return check_object(assembly, obj)


def check_lib():
    """=> None or (stage, details), for the library."""
    with tempfile.TemporaryDirectory() as directory:
        work = Path(directory)
        files = sorted(str(p) for p in (ROOT / "lib").glob("*.ss"))
        command = [*run.compiler, "--lib", directory, "--obj", work / "ours.o", *files]
        try:
            run_stage("compiler", command, cwd=ROOT)
        except PipelineFailure as error:
            return "compiler", [error.result.stderr.strip()]
        return check_object((work / "lib.s").read_text(), work / "ours.o")


def check_object(assembly, obj):
    """=> None or (stage, details): the object obj against that of GNU as for assembly."""
    with tempfile.TemporaryDirectory() as directory:
        work = Path(directory)
        (work / "out.s").write_text(assembly)
        try:
            run_stage("as", ["as", "-o", work / "as.o", work / "out.s"])
        except PipelineFailure as error:
            return "as", [error.result.stderr.strip()]
        try:
            diffs = compare(obj, work / "as.o")
        except PipelineFailure as error:
            return "encoding", [f"unreadable object: {error.result.stderr.strip()}"]
    return ("encoding", diffs) if diffs else None


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("patterns", nargs="*", help="test name patterns")
    parser.add_argument("-v", "--verbose", action="store_true")
    parser.add_argument("--stage1", action="store_true", help="compile with build/stage1")
    parser.add_argument("--lib", action="store_true", help="check the library")
    parser.add_argument("--program", metavar="FILE", help="check the program in FILE")
    args = parser.parse_args()
    if args.stage1:
        use_stage1()
    if args.lib or args.program:
        missing = lib_missing()
        if missing:
            sys.stderr.write(f"asmcheck.py: {missing}\n")
            exit(1)
        failed = False
        for name, check in [
            ("library", args.lib and check_lib),
            (args.program, args.program and (lambda: check_case(Path(args.program)))),
        ]:
            if not check:
                continue
            failure = check()
            if failure == SKIP:
                failure = "compiler", ["rejected by the compiler"]
            print(f"{'FAIL' if failure else 'PASS'} {name}")
            if failure:
                failed = True
                stage, details = failure
                print(f"  [{stage}]")
                for detail in details:
                    print(f"  {detail}")
        exit(1 if failed else 0)

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
