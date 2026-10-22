;; Collections leave the static datum alone, and keep what points to it
(let loop ((i 0) (keep '()))
  (if (< i 10000)
      (loop (+ i 1) (cons (embedded-datum "d") (make-list 3 i)))
      (list (car keep) (length keep) (eq? (caar keep) 'k))))
