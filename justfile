test:
    ./tests.py

test-chez:
    ./tests.py --chez

fmt:
    black --line-length 120 *.py
    clang-format -i *.c
    emacs -Q --batch --eval '(progn (setq make-backup-files nil) (setq-default indent-tabs-mode nil) (dolist (f command-line-args-left) (with-current-buffer (find-file f) (scheme-mode) (indent-region (point-min) (point-max)) (save-buffer))) (setq command-line-args-left nil))' *.ss *.sls compile low asm
