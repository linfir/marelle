(data alloc 1 ())

(def main () -> (val) ()
     1)
---
stage = "low"
stderr_contains = "def named like a keyword"
