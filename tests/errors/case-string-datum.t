(case 1 (("a") 1))
---
stage = "compiler"
stderr_contains = "unsupported case datum"
chez = "skip"
