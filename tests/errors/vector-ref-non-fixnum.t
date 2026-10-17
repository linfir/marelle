(vector-ref #(1 2) #\a)
---
stage = "executable"
stderr_contains = "Error: `vector-ref` index out of range: #\\a\n"
