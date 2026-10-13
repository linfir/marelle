#!/usr/bin/env python3

import argparse
import os
import sys
import tempfile
from pathlib import Path
from subprocess import CompletedProcess, run


ROOT = Path(__file__).resolve().parent


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


def build_runtime():
    return run_stage("setup", ["make", "-s"], cwd=ROOT)


def compile_scheme(source):
    return run_stage("compiler", ["scheme", "--script", ROOT / "compiler.ss"], input=source).stdout


def compile_low(source):
    prelude = (ROOT / "prelude.low").read_text()
    ir = run_stage("low", ["scheme-script", ROOT / "low"], input=prelude + source, cwd=ROOT).stdout
    return run_stage("asm", ["scheme-script", ROOT / "asm"], input=ir, cwd=ROOT).stdout


def run_pipeline(source, *, build=True, env=None, low=False):
    if build:
        build_runtime()

    with tempfile.TemporaryDirectory() as directory:
        work = Path(directory)
        assembly = work / "out.s"
        executable = work / "out"

        assembly.write_text(compile_low(source) if low else compile_scheme(source))

        run_stage("cc", ["cc", "-o", executable, assembly, ROOT / "tmp" / "runtime.o"])
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
