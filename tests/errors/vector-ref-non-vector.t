(vector-ref (cons 1 2) 0)
---
stage = "executable"
stderr_contains = "Error: `vector-ref` expected a vector: (1 . 2)\n"
