#!/usr/bin/env python3

import argparse
import fnmatch
import os
import sys
import tomllib
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path

from run import PipelineFailure, compile_source, lib_missing, run_chez, run_pipeline, use_as, use_chez_host


ROOT = Path(__file__).resolve().parent
TEST_ROOT = ROOT / "tests"
STAGES = {"compiler", "as", "ld", "executable", "result"}
OUTPUT_FIELDS = {"stdout", "stderr", "stdout_contains", "stderr_contains"}
INPUT_FIELDS = {"stdin"}
DUMP_FIELDS = {"dump", "cps_lacks"}
EXPECTATION_FIELDS = {"stage", "exit", "chez", "gc_stress", "env", "embed", "embed_datum", *OUTPUT_FIELDS, *INPUT_FIELDS, *DUMP_FIELDS}
SKIP = "skip"


class TestFormatError(Exception):
    pass


def read_case(path):
    source = path.read_text()
    try:
        expected = tomllib.loads(path.with_suffix(".toml").read_text())
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

    for field in ("env", "embed", "embed_datum"):
        table = expected.get(field, {})
        if not isinstance(table, dict) or not all(isinstance(v, str) for v in table.values()):
            raise TestFormatError(f"{field} must be a table of strings")

    for field in ("chez", "gc_stress"):
        if expected.get(field, SKIP) != SKIP:
            raise TestFormatError(f'{field} must be "skip"')

    for field in OUTPUT_FIELDS | INPUT_FIELDS | {"dump"}:
        if field in expected and not isinstance(expected[field], str):
            raise TestFormatError(f"{field} must be a string")

    if ("dump" in expected) != ("cps_lacks" in expected):
        raise TestFormatError("dump and cps_lacks go together")
    lacks = expected.get("cps_lacks", [])
    if isinstance(lacks, str):
        expected["cps_lacks"] = [lacks]
    elif not (isinstance(lacks, list) and all(isinstance(x, str) for x in lacks)):
        raise TestFormatError("cps_lacks must be a string or a list of strings")

    return source, expected


def describe_stream(value):
    return repr(value) if "\n" not in value else "\n" + value


def check_case(path, chez=False, gc_stress=None):
    try:
        source, expected = read_case(path)
    except (OSError, TestFormatError) as error:
        return "format", [str(error)]

    # Cases under tests/unsafe/ use unsafe defines, which Chez lacks
    unsafe = path.relative_to(TEST_ROOT).parts[0] == "unsafe"
    if chez and (unsafe or expected.get("chez") == SKIP):
        return SKIP
    if gc_stress and expected.get("gc_stress") == SKIP:
        return SKIP

    env = expected.get("env", {})
    if gc_stress:
        env = {"SCHEME_GC_STRESS": str(gc_stress)} | env

    try:
        stdin = expected.get("stdin", "")
        if chez:
            result = run_chez(source, stdin=stdin)
        else:
            result = run_pipeline(source, env=env, stdin=stdin, embed=expected.get("embed"),
                                  embed_datum=expected.get("embed_datum"))
        actual_stage = "result"
    except PipelineFailure as error:
        result = error.result
        actual_stage = error.stage

    expected_stage = expected["stage"]
    if chez and expected_stage != "result":
        # Chez has no separate stages and its own error messages: only check that it fails.
        if actual_stage == "result":
            return actual_stage, [
                "expected an error",
                f"stdout: {describe_stream(result.stdout)}",
            ]
        return None

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

    # The dump of a pass must not contain any string of cps_lacks
    if "dump" in expected and not chez:
        try:
            dump = compile_source(source, "--pass", expected["dump"])
        except PipelineFailure as error:
            return "compiler", [f"--pass {expected['dump']} failed", f"stderr: {describe_stream(error.result.stderr)}"]
        for lacked in expected["cps_lacks"]:
            if lacked in dump:
                return "compiler", [
                    f"--pass {expected['dump']} contains {lacked!r}",
                    f"dump: {describe_stream(dump)}",
                ]

    return None


def parse_args():
    parser = argparse.ArgumentParser()
    parser.add_argument("patterns", nargs="*", help="test name patterns")
    parser.add_argument("-k", dest="patterns_k", action="append", default=[], metavar="PATTERN")
    parser.add_argument("-v", "--verbose", action="store_true")
    parser.add_argument(
        "--chez",
        action="store_true",
        help="run the tests with Chez Scheme instead of the compiler",
    )
    parser.add_argument("--chez-host", action="store_true", help="run the compiler under Chez")
    parser.add_argument("--as", dest="assemble", action="store_true", help="assemble and link with as and ld")
    parser.add_argument(
        "--gc-stress",
        type=int,
        metavar="N",
        help="collect every N allocations (SCHEME_GC_STRESS)",
    )
    args = parser.parse_args()
    if args.gc_stress is not None and args.gc_stress < 1:
        parser.error("--gc-stress must be at least 1")
    if args.chez and (args.chez_host or args.assemble):
        parser.error("--chez runs no compiler: it excludes --chez-host and --as")
    return args


def selected_cases(patterns):
    paths = sorted(TEST_ROOT.rglob("*.scm"))
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
    if args.chez_host:
        use_chez_host()
    if args.assemble:
        use_as()
    paths = selected_cases([*args.patterns, *args.patterns_k])
    if not paths:
        print("No tests matched")
        exit(2)

    missing = None if args.chez else lib_missing()
    if missing:
        sys.stderr.write(f"tests.py: {missing}\n")
        exit(1)

    failures = []
    skipped = 0
    with ThreadPoolExecutor(max_workers=os.cpu_count()) as pool:
        results = pool.map(lambda path: check_case(path, args.chez, args.gc_stress), paths)
        for path, failure in zip(paths, results):
            name = path.relative_to(TEST_ROOT).as_posix()
            if failure == SKIP:
                skipped += 1
                if args.verbose:
                    print(f"SKIP {name}")
                else:
                    print("s", end="", flush=True)
                continue
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

    passed = len(paths) - len(failures) - skipped
    noun = "test" if len(paths) == 1 else "tests"
    summary = f"\n{len(paths)} {noun}: {passed} passed, {len(failures)} failed"
    if skipped:
        summary += f", {skipped} skipped"
    print(summary)
    if failures:
        exit(1)


if __name__ == "__main__":
    main()
