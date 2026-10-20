# For time in the test recipes
SHELL = /bin/bash

# A missing source is not a prerequisite, so a built library works without its sources
SOURCES = $(wildcard lib/*.ss run compiler/*.sls)
CHEZ_LIBS = $(wildcard compiler/*.sls)
CHEZ_OBJS = $(patsubst compiler/%.sls,build/chez/%.so,$(CHEZ_LIBS))
# Chez with the compiled compiler libraries, which it loads instead of a .sls when not older
CHEZ = scheme --libdirs compiler::build/chez

all: chez prebuild

chez: $(CHEZ_OBJS)

prebuild: build/lib.o build/lib.scm

# All recompiled when one changes, since a library is compiled against those it imports
$(CHEZ_OBJS) &: $(CHEZ_LIBS)
	rm -rf build/chez
	mkdir -p build/chez
	printf '(reset-handler (lambda () (exit 1)))\n(import (utils) (library) (asm))\n' | \
	  $(CHEZ) -q --compile-imported-libraries

# The precompiled library, linked with every program
build/lib.o: build/lib.s
	as -o $@ $<

# One recipe writes both
build/lib.s build/lib.scm &: $(SOURCES) $(CHEZ_OBJS)
	mkdir -p build
	$(CHEZ) --program run --lib build $(sort $(wildcard lib/*.ss))

test: all
	time ./tests.py

# The benchmarks, against Chez Scheme
bench: all
	./bench.py --chez

test-chez:
	time ./tests.py --chez

fmt:
	black -q --line-length 120 *.py
	emacs -Q --batch --eval '(let ((inhibit-message t)) (setq make-backup-files nil) (setq-default indent-tabs-mode nil) (dolist (f command-line-args-left) (with-current-buffer (find-file f) (scheme-mode) (indent-region (point-min) (point-max)) (save-buffer))) (setq command-line-args-left nil))' *.ss lib/*.ss compiler/*.sls compile asm run flatten $$(find tests -name '*.scm')

clean:
	rm -rf build

.PHONY: all chez prebuild test bench test-chez fmt clean
