;; Parameters, without converters: parameterize is expanded by the front pass

;; Its value when called with no argument, else set to the argument
(define (make-parameter value)
  (declare public)
  (case-lambda
   (() value)
   ((v) (set! value v))))
