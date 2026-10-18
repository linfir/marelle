(list (string-append) (string-append "a") (string-append "ab" "" "cd") (string-append "a" "é" "😀"))
---
stdout = '("" "a" "abcd" "aé😀")'
