;; A string in an error message is written like a result
(car "a\"b")
---
stage = "executable"
stderr = "Error: `car` expected a pair: \"a\\\"b\"\n"
