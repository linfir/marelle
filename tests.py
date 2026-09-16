#!/usr/bin/env python3

import subprocess
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parent


class RunnerTests(unittest.TestCase):
    def assert_expression_output(self, expression, expected):
        result = subprocess.run(
            [ROOT / "run.py", "-e", expression],
            cwd=ROOT,
            text=True,
            capture_output=True,
            check=True,
        )
        self.assertEqual(result.stdout, expected)

    def assert_compile_error(self, program, expected):
        result = subprocess.run(
            ["csi", "-s", ROOT / "compiler.ss"],
            cwd=ROOT,
            input=program,
            text=True,
            capture_output=True,
        )
        self.assertNotEqual(result.returncode, 0)
        self.assertIn(expected, result.stderr)

    def test_atomic_top_level_expressions(self):
        cases = {
            "0": "0",
            "42": "42",
            "-7": "-7",
        }

        for expression, expected in cases.items():
            with self.subTest(expression=expression):
                self.assert_expression_output(expression, expected)

    def test_expressions(self):
        cases = {
            "(+ 2 (* 3 4))": "14",
            "(if 0 11 22)": "22",
            "(if 1 11 22)": "11",
            "(if 1 (if 0 33 44) 55)": "44",
            "(let () 42)": "42",
            "(let ((x 40)) (+ x 2))": "42",
            "(let ((x 10)) (let ((x 20) (y x)) (+ x y)))": "30",
            "(let ((x 10) (y 20)) (let ((x y) (y x)) (- x y)))": "10",
            "(let ((x (if 1 40 0)) (y (if 0 0 2))) (+ x y))": "42",
            "(< 1 2)": "1",
            "(< 2 1)": "0",
            "(= 2 2)": "1",
            "(= 1 2)": "0",
            "(> -1 -2)": "1",
            "(<= 2 2)": "1",
            "(>= 1 2)": "0",
            "(if (< 1 2) 42 0)": "42",
        }

        for expression, expected in cases.items():
            with self.subTest(expression=expression):
                self.assert_expression_output(expression, expected)

    def test_named_functions(self):
        cases = {
            "(define (answer) 42) (answer)": "42",
            "(define (add x y) (+ x y)) (add 20 22)": "42",
            "(define (square x) (* x x)) (square 7)": "49",
            "(define (square x) (* x x)) (+ 1 (square (+ (square 2) 1)))": "26",
            "(define (fact n) (if (= n 0) 1 (* n (fact (- n 1))))) (fact 5)": "120",
            "(define (even n) (if (= n 0) 1 (odd (- n 1)))) " "(define (odd n) (if (= n 0) 0 (even (- n 1)))) (even 10)": "1",
            "(define (main) 42) (main)": "42",
        }

        for program, expected in cases.items():
            with self.subTest(program=program):
                self.assert_expression_output(program, expected)

    def test_named_function_errors(self):
        cases = {
            "(define (f) 1) (define (f) 2) (f)": "redefinition-not-allowed",
            "(define (f x) x) (f)": "function-call-invalid-arity",
        }

        for program, expected in cases.items():
            with self.subTest(program=program):
                self.assert_compile_error(program, expected)


if __name__ == "__main__":
    unittest.main()
