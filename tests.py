#!/usr/bin/env python3

import subprocess
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parent


class RunnerTests(unittest.TestCase):
    def test_expressions(self):
        cases = {
            "42": "42",
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
                result = subprocess.run(
                    [ROOT / "run.py", "-e", expression],
                    cwd=ROOT,
                    text=True,
                    capture_output=True,
                    check=True,
                )
                self.assertEqual(result.stdout, expected)


if __name__ == "__main__":
    unittest.main()
