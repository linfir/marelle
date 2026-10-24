(define (order l) (if (apply < l) 'up (if (apply > l) 'down 'neither)))
(map order (list (list 1 2 3) (list 3 2 1) (list 1 3 2) (list 1)))
