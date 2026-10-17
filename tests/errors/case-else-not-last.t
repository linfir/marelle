(case 1 (else 1) ((1) 2))
---
stage = "compiler"
stderr_contains = "invalid else clause"
chez = "skip"
