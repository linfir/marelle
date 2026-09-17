#!/usr/bin/env python3

import argparse
from subprocess import run
import tempfile
from pathlib import Path


ROOT = Path(__file__).resolve().parent


def parse_args():
    parser = argparse.ArgumentParser()
    source = parser.add_mutually_exclusive_group(required=True)
    source.add_argument("-e", metavar="STRING")
    source.add_argument("file", nargs="?", type=Path)
    return parser.parse_args()


def main():
    args = parse_args()
    source = args.e if args.e is not None else args.file.read_text()

    run(["make", "-s"], cwd=ROOT, check=True)

    with tempfile.TemporaryDirectory() as directory:
        work = Path(directory)
        ssa = work / "out.ssa"
        assembly = work / "out.s"
        executable = work / "out"

        with ssa.open("w") as output:
            run(
                ["scheme", "--script", ROOT / "compiler.ss"],
                input=source,
                text=True,
                stdout=output,
                check=True,
            )

        run(["qbe", "-o", assembly, ssa], check=True)
        run(["cc", "-o", executable, assembly, ROOT / "runtime.o"], check=True)
        return run([executable]).returncode


if __name__ == "__main__":
    raise SystemExit(main())
