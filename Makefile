# For time in the test recipes
SHELL = /bin/bash

# A missing source is not a prerequisite, so a built library works without its sources
SOURCES = $(wildcard lib/*.ss schemec compiler/*.sls)
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
	$(CHEZ) --program schemec --lib build $(sort $(wildcard lib/*.ss))

# Self-hosting: the compiler as one program, compiled by schemec under Chez (stage 1), then by
# stage 1 (stage 2)
stage1: build/stage1

stage2: build/stage2

build/flat.ss: flatten schemec $(CHEZ_LIBS)
	mkdir -p build
	./flatten > $@

build/stage1.s: build/flat.ss $(CHEZ_OBJS) build/lib.scm
	$(CHEZ) --program schemec < $< > $@

build/stage2.s: build/flat.ss build/stage1 build/lib.scm
	build/stage1 < $< > $@

build/stage1.o build/stage2.o: build/%.o: build/%.s
	as -o $@ $<

# Stripped: ld's symbol table names the inputs (stage1.o), so the two stages would differ
build/stage1 build/stage2: build/%: build/%.o build/lib.o
	ld -o $@ $^
	strip $@

# The fixed point: stage 1 compiles the library and the compiler as Chez did
fixpoint: build/stage2
	mkdir -p build/stage1-lib
	build/stage1 --lib build/stage1-lib $(sort $(wildcard lib/*.ss))
	cmp build/lib.s build/stage1-lib/lib.s
	cmp build/lib.scm build/stage1-lib/lib.scm
	cmp build/stage1.s build/stage2.s
	cmp build/stage1 build/stage2

test: all
	time ./tests.py

# The benchmarks, against Chez Scheme
bench: all
	./bench.py --chez

test-chez:
	time ./tests.py --chez

fmt:
	black -q --line-length 120 *.py
	emacs -Q --batch --eval '(let ((inhibit-message t)) (setq make-backup-files nil) (setq-default indent-tabs-mode nil) (dolist (f command-line-args-left) (with-current-buffer (find-file f) (scheme-mode) (indent-region (point-min) (point-max)) (save-buffer))) (setq command-line-args-left nil))' *.ss lib/*.ss compiler/*.sls schemec flatten $$(find tests -name '*.scm')

clean:
	rm -rf build

# A failed recipe leaves no partial output
.DELETE_ON_ERROR:

.PHONY: all chez prebuild stage1 stage2 fixpoint test bench test-chez fmt clean
