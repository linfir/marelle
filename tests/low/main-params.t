(def main ((val x)) -> (val) ()
     x)
---
stage = "low"
stderr_contains = "main must be a non-inline def"
