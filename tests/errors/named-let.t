(let loop ((i 0)) i)
---
stage = "compiler"
stderr_contains = "named let is only supported in the library"
chez = "skip"
