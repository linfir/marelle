#!/usr/bin/env python3

import argparse
import os
import sys
import tempfile
from pathlib import Path
from subprocess import CompletedProcess, run


ROOT = Path(__file__).resolve().parent
BUILD = ROOT / "build"
LIB = BUILD / "lib.o"
STAGE1 = BUILD / "stage1"
CHEZ_HOST = ["scheme", "--libdirs", "compiler::build/chez", "--program", ROOT / "schemec"]

# The compiler: the self-compiled build/stage1, or schemec under Chez (use_chez_host), loading the
# compiled compiler libraries in build/chez instead of the .sls
compiler = [STAGE1]

# Whether programs go through as and ld with build/lib.o (use_as), instead of the compiler's -o
assemble = False


def use_chez_host():
    global compiler
    compiler = CHEZ_HOST


def use_as():
    global assemble
    assemble = True


class PipelineFailure(Exception):
    def __init__(self, stage, command, result):
        super().__init__(f"{stage} failed with exit status {result.returncode}")
        self.stage = stage
        self.command = command
        self.result = result


def run_stage(stage, command, **kwargs):
    try:
        result = run(command, text=True, capture_output=True, **kwargs)
    except OSError as error:
        result = CompletedProcess(command, 127, "", str(error))
        raise PipelineFailure(stage, command, result) from error
    if result.returncode != 0:
        raise PipelineFailure(stage, command, result)
    return result


def lib_missing():
    """The message when make's output is missing: the library's image and interface, build/lib.o
    with as, and build/stage1 or the compiled compiler libraries, else None."""
    needed = [BUILD / "lib.img", BUILD / "lib.scm"]
    if assemble:
        needed.append(LIB)
    if compiler == CHEZ_HOST:
        needed.append(BUILD / "chez")
    elif not STAGE1.exists():
        return "missing build/stage1: run make stage1 first"
    missing = [p for p in needed if not p.exists()]
    if missing:
        return f"missing {', '.join(str(p.relative_to(ROOT)) for p in missing)}: run make first"
    return None


def compile_source(source, *options):
    """=> the compiler's output for source with options, e.g. -S or --dump PASS."""
    return run_stage("compiler", [*compiler, *options], input=source, cwd=ROOT).stdout


# The executable runs in the temporary directory, so programs may create files there
def run_pipeline(source, *, env=None, stdin=None):
    with tempfile.TemporaryDirectory() as directory:
        work = Path(directory)
        executable = work / "out"
        if assemble:
            assembly = work / "out.s"
            obj = work / "out.o"
            assembly.write_text(compile_source(source, "-S"))
            run_stage("as", ["as", "-o", obj, assembly])
            run_stage("ld", ["ld", "-o", executable, obj, LIB])
        else:
            compile_source(source, "-o", executable)
        return run_stage("executable", [executable], input=stdin, cwd=work, env=os.environ | (env or {}))


# chez.ss reads the program from a file, leaving stdin to it
def run_chez(source, *, stdin=None):
    with tempfile.TemporaryDirectory() as directory:
        program = Path(directory) / "program.scm"
        program.write_text(source)
        command = ["scheme", "--script", ROOT / "chez" / "chez.ss", program]
        return run_stage("chez", command, input=stdin, cwd=directory)


def parse_args():
    parser = argparse.ArgumentParser()
    source = parser.add_mutually_exclusive_group(required=True)
    source.add_argument("-e", metavar="STRING")
    source.add_argument("file", nargs="?", type=Path)
    parser.add_argument("--dump", metavar="PASS", help="print the IR after PASS instead of running")
    parser.add_argument("--chez-host", action="store_true", help="run the compiler under Chez")
    parser.add_argument("--as", dest="assemble", action="store_true", help="assemble and link with as and ld")
    return parser.parse_args()


def main():
    args = parse_args()
    if args.chez_host:
        use_chez_host()
    if args.assemble:
        use_as()
    source = args.e if args.e is not None else args.file.read_text()

    missing = lib_missing()
    if missing:
        sys.stderr.write(f"run.py: {missing}\n")
        exit(1)

    try:
        if args.dump:
            sys.stdout.write(compile_source(source, "--dump", args.dump))
            return
        result = run_pipeline(source)
    except PipelineFailure as error:
        sys.stdout.write(error.result.stdout)
        sys.stderr.write(f"run.py: {error}\n")
        sys.stderr.write(error.result.stderr)
        exit(error.result.returncode or 1)

    sys.stdout.write(result.stdout)
    sys.stderr.write(result.stderr)


if __name__ == "__main__":
    main()
