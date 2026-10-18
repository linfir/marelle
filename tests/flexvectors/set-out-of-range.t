(flexvector-set! (flexvector 1 2) 3 0)
---
stage = "executable"
stderr_contains = "Error: `flexvector-set!` index out of range: 3\n"
