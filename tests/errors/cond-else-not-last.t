(cond (else 1) (#t 2))
---
stage = "compiler"
stderr_contains = "invalid else clause"
chez = "skip"
