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
