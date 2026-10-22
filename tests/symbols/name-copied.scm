;; string->symbol keeps an immutable copy of a mutable name: mutating the string later changes
;; neither the symbol nor the lookup of its name
(define s (string-append "ab" "c"))
(define sym (string->symbol s))
(string-set! s 0 #\z)
(string-set! s 1 #\λ)
(list sym (symbol->string sym) s (eq? sym (string->symbol "abc")) (eq? sym (string->symbol s)))
