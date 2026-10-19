;; After a read, the rest of the input is buffered
(let* ((c (read-char))
       (ready (char-ready? (current-input-port))))
  (list c ready (char-ready?)))
