(cond () (else 1))
---
stage = "compiler"
stderr_contains = "invalid cond clause"
chez = "skip"
