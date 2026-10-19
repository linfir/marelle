(with-input-from-string "abc"
  (lambda ()
    (let* ((a (read-char))
           (b (read-line)))
      (list a b (read-char)))))
