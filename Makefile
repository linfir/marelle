CFLAGS ?= -O2 -Werror -Wall -Wextra -Wpedantic -Wconversion -Wshadow

.DELETE_ON_ERROR:

tmp/runtime.o: tmp/runtime-c.o tmp/runtime-asm.o tmp/prelude.o
	$(LD) -r -o $@ $^

tmp/runtime-c.o: runtime.c | tmp
	$(CC) $(CFLAGS) -c -o $@ $<

tmp/runtime-asm.o: runtime.s | tmp
	$(CC) -c -o $@ $<

tmp/prelude.o: tmp/prelude.s
	$(CC) -c -o $@ $<

tmp/prelude.s: $(filter-out chez.ss,$(wildcard *.ss)) | tmp
	scheme --script compiler.ss --prelude > $@

tmp:
	mkdir -p $@
