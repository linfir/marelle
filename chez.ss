;; Evaluates a test program, after the shims of what Chez lacks, and writes its last value
(let ((dir (path-parent (car (command-line)))))
  (for-each (lambda (file)
              (load (if (string=? dir "") file (string-append dir "/" file))))
            '("chez-flexvectors.ss" "chez-lists.ss")))

(let loop ((last (void)))
  (let ((x (read)))
    (if (eof-object? x)
        (write last)
        (loop (eval x)))))
