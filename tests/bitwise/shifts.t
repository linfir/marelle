(list (bitwise-arithmetic-shift 1 10) (bitwise-arithmetic-shift 1 60) (bitwise-arithmetic-shift 1 64)
      (bitwise-arithmetic-shift -1 100) (bitwise-arithmetic-shift 1024 -3) (bitwise-arithmetic-shift -5 -1)
      (bitwise-arithmetic-shift -1 -100) (bitwise-arithmetic-shift 7 0)
      (bitwise-arithmetic-shift (bitwise-arithmetic-shift 3 200) -199)
      (bitwise-arithmetic-shift (- 0 (bitwise-arithmetic-shift 3 200)) -199)
      (bitwise-arithmetic-shift (- 1 (bitwise-arithmetic-shift 1 130)) -129)
      (bitwise-arithmetic-shift 1152921504606846975 1) (bitwise-arithmetic-shift -1152921504606846976 -60)
      (bitwise-arithmetic-shift-left 3 2) (bitwise-arithmetic-shift-right -16 2))
---
stdout = "(1024 1152921504606846976 18446744073709551616 -1267650600228229401496703205376 128 -3 -1 7 6 -6 -2 2305843009213693950 -1 12 -4)"
