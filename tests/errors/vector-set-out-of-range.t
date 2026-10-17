(vector-set! (make-vector 1 0) 1 0)
---
stage = "executable"
stderr_contains = "Error: `vector-set!` index out of range: 1\n"
