(vector-ref #(1 2) -1)
---
stage = "executable"
stderr_contains = "Error: `vector-ref` index out of range: -1\n"
