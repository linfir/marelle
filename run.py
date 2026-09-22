#!/usr/bin/env python3

import argparse
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


def run_pipeline(source, *, build=True):
    if build:
        build_runtime()

    with tempfile.TemporaryDirectory() as directory:
        work = Path(directory)
        ssa = work / "out.ssa"
        assembly = work / "out.s"
        executable = work / "out"

        compiler = run_stage(
            "compiler",
            ["scheme", "--script", ROOT / "compiler.ss"],
            input=source,
        )
        ssa.write_text(compiler.stdout)

        run_stage("qbe", ["qbe", "-o", assembly, ssa])
        run_stage("cc", ["cc", "-o", executable, assembly, ROOT / "runtime.o"])
        return run_stage("executable", [executable])


def parse_args():
    parser = argparse.ArgumentParser()
    source = parser.add_mutually_exclusive_group(required=True)
    source.add_argument("-e", metavar="STRING")
    source.add_argument("file", nargs="?", type=Path)
    return parser.parse_args()


def main():
    args = parse_args()
    source = args.e if args.e is not None else args.file.read_text()

    try:
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
