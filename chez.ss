;; Evaluates a test program, after the shims of what Chez lacks, and writes its last value
(let ((dir (path-parent (car (command-line)))))
  (load (if (string=? dir "") "chez-flexvectors.ss" (string-append dir "/chez-flexvectors.ss"))))

(let loop ((last (void)))
  (let ((x (read)))
    (if (eof-object? x)
        (write last)
        (loop (eval x)))))
