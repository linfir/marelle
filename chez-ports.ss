;; The R7RS port procedures Chez lacks
(define write-string
  (case-lambda
   ((s) (put-string (current-output-port) s))
   ((s port) (put-string port s))
   ((s port start) (put-string port s start))
   ((s port start end) (put-string port s start (- end start)))))
