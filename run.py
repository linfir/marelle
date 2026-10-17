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


def build_lib():
    """Compiles runtime.ss and prelude.ss into build/lib.o and build/lib.scm when out of date."""
    sources = [ROOT / "runtime.ss", ROOT / "prelude.ss", ROOT / "run", *ROOT.glob("*.sls")]
    if LIB.exists() and all(s.stat().st_mtime < LIB.stat().st_mtime for s in sources):
        return
    BUILD.mkdir(exist_ok=True)
    run_stage("lib", ["scheme-script", ROOT / "run", "--lib", BUILD], cwd=ROOT)
    run_stage("lib", ["as", "-o", LIB, BUILD / "lib.s"])


def compile_source(source, low):
    stage, flags = ("low", ["--low"]) if low else ("compiler", [])
    return run_stage(stage, ["scheme-script", ROOT / "run", *flags], input=source, cwd=ROOT).stdout


def run_pipeline(source, *, env=None, low=False):
    with tempfile.TemporaryDirectory() as directory:
        work = Path(directory)
        assembly = work / "out.s"
        obj = work / "out.o"
        executable = work / "out"

        assembly.write_text(compile_source(source, low))

        run_stage("as", ["as", "-o", obj, assembly])
        run_stage("ld", ["ld", "-o", executable, obj, LIB])
        return run_stage("executable", [executable], env=os.environ | (env or {}))


def run_chez(source):
    return run_stage("chez", ["scheme", "--script", ROOT / "chez.ss"], input=source)


def parse_args():
    parser = argparse.ArgumentParser()
    source = parser.add_mutually_exclusive_group(required=True)
    source.add_argument("-e", metavar="STRING")
    source.add_argument("file", nargs="?", type=Path)
    parser.add_argument("--low", action="store_true", help="the source is a low program")
    return parser.parse_args()


def main():
    args = parse_args()
    source = args.e if args.e is not None else args.file.read_text()

    try:
        build_lib()
        result = run_pipeline(source, low=args.low)
    except PipelineFailure as error:
        sys.stdout.write(error.result.stdout)
        sys.stderr.write(f"run.py: {error}\n")
        sys.stderr.write(error.result.stderr)
        exit(error.result.returncode or 1)

    sys.stdout.write(result.stdout)
    sys.stderr.write(result.stderr)


if __name__ == "__main__":
    main()
