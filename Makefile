CFLAGS ?= -O2 -Werror -Wall -Wextra -Wpedantic -Wconversion -Wshadow

.DELETE_ON_ERROR:

tmp/runtime.o: tmp/runtime-c.o tmp/runtime-asm.o
	$(LD) -r -o $@ $^

tmp/runtime-c.o: runtime.c | tmp
	$(CC) $(CFLAGS) -c -o $@ $<

tmp/runtime-asm.o: runtime.s | tmp
	$(CC) -c -o $@ $<

tmp:
	mkdir -p $@
