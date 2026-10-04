(let ((x 1)) (car x) x)
---
stage = "executable"
stderr_contains = "Error: car: expected pair: 1\n"
