;; More raws than registers in a noalloc loop: some live in raw slots
(define (rotate (raw n))
  (declare unsafe noalloc (result raw))
  (let loop ((i n) (a (raw 1)) (b (raw 2)) (c (raw 3)) (d (raw 4)) (e (raw 5)) (f (raw 6)) (g (raw 7)))
    (if (raw= i (raw 0))
        (raw+ a (raw+ (raw* b (raw 2)) (raw+ (raw* c (raw 3)) (raw+ (raw* d (raw 4))
          (raw+ (raw* e (raw 5)) (raw+ (raw* f (raw 6)) (raw* g (raw 7))))))))
        (loop (raw- i (raw 1)) b c d e f g a))))

(define (entry)
  (declare unsafe)
  (make-fixnum (rotate (raw 10))))
(entry)
