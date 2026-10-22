;; Evaluates the test program in the file given as argument, after the shims of what Chez lacks,
;; and writes its last value
(let ((dir (path-parent (car (command-line)))))
  (for-each (lambda (file)
              (load (if (string=? dir "") file (string-append dir "/" file))))
            '("chez-bytevectors.ss" "chez-flexvectors.ss" "chez-lists.ss" "chez-numbers.ss" "chez-ports.ss" "chez-strings.ss")))

(let ((in (open-input-file (cadr (command-line)))))
  (let loop ((last (void)))
    (let ((x (read in)))
      (if (eof-object? x)
          (write last)
          (loop (eval x))))))
