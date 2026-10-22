;; Literal symbols are static: the same object as string->symbol's, also for a symbol the
;; library has too (pair, an error kind), and for a new one
(list (eq? 'pair (string->symbol "pair"))
      (eq? 'zzz-fresh-symbol (string->symbol "zzz-fresh-symbol"))
      (eq? (string->symbol (symbol->string 'car)) 'car))
