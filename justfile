test:
    ./tests.py

fmt:
    black --line-length 120 *.py
    clang-format -i *.c
    emacs -Q --batch compiler.ss --eval '(progn (setq make-backup-files nil) (scheme-mode) (indent-region (point-min) (point-max)) (save-buffer))'
