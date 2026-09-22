#!/usr/bin/env python3

import argparse
import fnmatch
import tomllib
from pathlib import Path

from run import PipelineFailure, build_runtime, run_pipeline


ROOT = Path(__file__).resolve().parent
TEST_ROOT = ROOT / "tests"
STAGES = {"compiler", "qbe", "cc", "executable", "result"}
OUTPUT_FIELDS = {"stdout", "stderr", "stdout_contains", "stderr_contains"}
EXPECTATION_FIELDS = {"stage", "exit", *OUTPUT_FIELDS}


class TestFormatError(Exception):
    pass


def read_case(path):
    lines = path.read_text().splitlines(keepends=True)
    separators = [i for i, line in enumerate(lines) if line.rstrip("\r\n") == "---"]
    if len(separators) != 1:
        raise TestFormatError("expected exactly one line containing ---")

    separator = separators[0]
    source = "".join(lines[:separator])
    try:
        expected = tomllib.loads("".join(lines[separator + 1 :]))
    except tomllib.TOMLDecodeError as error:
        raise TestFormatError(f"invalid expectation TOML: {error}") from error

    unknown = set(expected) - EXPECTATION_FIELDS
    if unknown:
        names = ", ".join(sorted(unknown))
        raise TestFormatError(f"unknown expectation field: {names}")

    stage = expected.setdefault("stage", "result")
    if not isinstance(stage, str):
        raise TestFormatError("stage must be a string")
    if stage not in STAGES:
        raise TestFormatError(f"unknown stage: {stage!r}")

    expected.setdefault("exit", 0 if stage == "result" else "nonzero")
    exit_status = expected["exit"]
    if not ((isinstance(exit_status, int) and not isinstance(exit_status, bool)) or exit_status == "nonzero"):
        raise TestFormatError('exit must be an integer or "nonzero"')

    for field in OUTPUT_FIELDS:
        if field in expected and not isinstance(expected[field], str):
            raise TestFormatError(f"{field} must be a string")

    return source, expected


def describe_stream(value):
    return repr(value) if "\n" not in value else "\n" + value


def check_case(path):
    try:
        source, expected = read_case(path)
    except (OSError, TestFormatError) as error:
        return "format", [str(error)]

    try:
        result = run_pipeline(source, build=False)
        actual_stage = "result"
    except PipelineFailure as error:
        result = error.result
        actual_stage = error.stage

    expected_stage = expected["stage"]
    if actual_stage != expected_stage:
        details = [
            "unexpected pipeline outcome",
            f"expected stage: {expected_stage}",
            f"actual stage:   {actual_stage}",
            f"exit: {result.returncode}",
        ]
        if result.stderr:
            details.append(f"stderr: {describe_stream(result.stderr)}")
        return actual_stage, details

    expected_exit = expected["exit"]
    if expected_exit == "nonzero":
        if result.returncode == 0:
            return actual_stage, ["expected a nonzero exit status"]
    elif result.returncode != expected_exit:
        return actual_stage, [
            "exit status differs",
            f"expected: {expected_exit}",
            f"actual:   {result.returncode}",
        ]

    for stream in ("stdout", "stderr"):
        actual = getattr(result, stream)
        if stream in expected and actual != expected[stream]:
            return actual_stage, [
                f"{stream} differs",
                f"expected: {describe_stream(expected[stream])}",
                f"actual:   {describe_stream(actual)}",
            ]

        contains = f"{stream}_contains"
        if contains in expected and expected[contains] not in actual:
            return actual_stage, [
                f"{stream} does not contain expected substring",
                f"expected: {describe_stream(expected[contains])}",
                f"actual:   {describe_stream(actual)}",
            ]

    return None


def parse_args():
    parser = argparse.ArgumentParser()
    parser.add_argument("patterns", nargs="*", help="test name patterns")
    parser.add_argument("-k", dest="patterns_k", action="append", default=[], metavar="PATTERN")
    parser.add_argument("-v", "--verbose", action="store_true")
    return parser.parse_args()


def selected_cases(patterns):
    paths = sorted(TEST_ROOT.rglob("*.t"))
    if not patterns:
        return paths

    globs = [pattern if any(c in pattern for c in "*?[") else f"*{pattern}*" for pattern in patterns]
    return [
        path
        for path in paths
        if any(fnmatch.fnmatchcase(path.relative_to(TEST_ROOT).as_posix(), pattern) for pattern in globs)
    ]


def print_failure(name, failure):
    stage, details = failure
    print(f"FAIL {name} [{stage}]")
    for detail in details:
        print(f"  {detail}")


def main():
    args = parse_args()
    paths = selected_cases([*args.patterns, *args.patterns_k])
    if not paths:
        print("No tests matched")
        exit(2)

    try:
        build_runtime()
    except PipelineFailure as error:
        details = [f"exit: {error.result.returncode}"]
        if error.result.stderr:
            details.append(f"stderr: {describe_stream(error.result.stderr)}")
        print_failure("setup", ("setup", details))
        exit(1)

    failures = []
    for path in paths:
        name = path.relative_to(TEST_ROOT).as_posix()
        failure = check_case(path)
        if args.verbose and not failure:
            print(f"PASS {name}")
        elif not args.verbose:
            print("F" if failure else ".", end="", flush=True)
        if failure:
            failures.append((name, failure))

    if not args.verbose:
        print()
    if failures:
        print()
        for index, (name, failure) in enumerate(failures):
            if index:
                print()
            print_failure(name, failure)

    passed = len(paths) - len(failures)
    noun = "test" if len(paths) == 1 else "tests"
    print(f"\n{len(paths)} {noun}: {passed} passed, {len(failures)} failed")
    if failures:
        exit(1)


if __name__ == "__main__":
    main()
