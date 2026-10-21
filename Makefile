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

prebuild: build/lib.img build/lib.scm

# All recompiled when one changes, since a library is compiled against those it imports
$(CHEZ_OBJS) &: $(CHEZ_LIBS)
	rm -rf build/chez
	mkdir -p build/chez
	printf '(reset-handler (lambda () (exit 1)))\n(import (utils) (library) (asm) (encode) (object) (layout) (elf) (host))\n' | \
	  $(CHEZ) -q --compile-imported-libraries

# The library as an object, for as and ld (tests.py --as, asmcheck.py, bench.py)
build/lib.o: build/lib.s
	as -o $@ $<

# One recipe writes all three
build/lib.s build/lib.img build/lib.scm &: $(SOURCES) $(CHEZ_OBJS)
	mkdir -p build
	$(CHEZ) --program schemec --lib build $(sort $(wildcard lib/*.ss))

# Self-hosting: the compiler as one program, compiled by schemec under Chez (stage 1), then by
# stage 1 (stage 2); only Chez is needed. Each embeds the library's image and interface.
stage1: build/stage1

stage2: build/stage2

EMBED_LIB = --embed lib.img=build/lib.img --embed lib.scm=build/lib.scm

build/flat.ss: flatten schemec $(CHEZ_LIBS)
	mkdir -p build
	./flatten > $@

build/stage1: build/flat.ss $(CHEZ_OBJS) build/lib.img build/lib.scm
	$(CHEZ) --program schemec $(EMBED_LIB) -o $@ $<

build/stage2: build/flat.ss build/stage1 build/lib.img build/lib.scm
	build/stage1 $(EMBED_LIB) -o $@ $<

# The compiler, as marelle
PREFIX ?= $(HOME)/.local

install: build/stage1
	mkdir -p $(PREFIX)/bin
	cp build/stage1 $(PREFIX)/bin/marelle

# The fixed point: stage 1 compiles the library and the compiler as Chez did
fixpoint: build/stage2
	mkdir -p build/stage1-lib
	build/stage1 --lib build/stage1-lib $(sort $(wildcard lib/*.ss))
	cmp build/lib.s build/stage1-lib/lib.s
	cmp build/lib.img build/stage1-lib/lib.img
	cmp build/lib.scm build/stage1-lib/lib.scm
	cmp build/stage1 build/stage2

test: all build/stage1
	time ./tests.py

# Through as and ld
test-as: all build/stage1 build/lib.o
	time ./tests.py --as

# The benchmarks, against Chez Scheme
bench: all build/stage1 build/lib.o
	./bench.py --chez

test-chez:
	time ./tests.py --chez

# The encoder against GNU as: the cases, the library and the compiler
test-asm: all build/stage1 build/lib.o build/flat.ss
	time ./asmcheck.py
	./asmcheck.py --lib --image --program build/flat.ss

fmt:
	black -q --line-length 120 *.py
	emacs -Q --batch --eval '(let ((inhibit-message t)) (setq make-backup-files nil) (setq-default indent-tabs-mode nil) (dolist (f command-line-args-left) (with-current-buffer (find-file f) (scheme-mode) (indent-region (point-min) (point-max)) (save-buffer))) (setq command-line-args-left nil))' $(wildcard *.ss) lib/*.ss compiler/*.sls schemec flatten $$(find tests -name '*.scm')

clean:
	rm -rf build

# A failed recipe leaves no partial output
.DELETE_ON_ERROR:

.PHONY: all chez prebuild stage1 stage2 install fixpoint test test-as bench test-chez test-asm fmt clean
