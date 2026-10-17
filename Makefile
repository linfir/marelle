SOURCES = runtime.ss prelude.ss run $(wildcard *.sls)

# The precompiled runtime and prelude, linked with every program
build/lib.o: build/lib.s
	as -o $@ $<

# One recipe writes both
build/lib.s build/lib.scm &: $(SOURCES)
	mkdir -p build
	scheme-script run --lib build

test:
	./tests.py

test-chez:
	./tests.py --chez

fmt:
	black --line-length 120 *.py
	emacs -Q --batch --eval '(progn (setq make-backup-files nil) (setq-default indent-tabs-mode nil) (dolist (f command-line-args-left) (with-current-buffer (find-file f) (scheme-mode) (indent-region (point-min) (point-max)) (save-buffer))) (setq command-line-args-left nil))' *.ss *.sls compile low asm run

clean:
	rm -rf build

.PHONY: test test-chez fmt clean
