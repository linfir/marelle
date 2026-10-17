(vector-ref #(1 2) 2)
---
stage = "executable"
stderr_contains = "Error: `vector-ref` index out of range: 2\n"
