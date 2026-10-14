(def main () -> (val) ()
     (let ((undef 1))
       undef))
---
stage = "low"
stderr_contains = "local variable named like a keyword"
