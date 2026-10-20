(let ((args (command-line)))
  (list (pair? args) (for-all string? args) (> (string-length (car args)) 0)))
