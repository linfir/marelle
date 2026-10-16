CFLAGS ?= -O2 -Werror -Wall -Wextra -Wpedantic -Wconversion -Wshadow

.DELETE_ON_ERROR:

tmp/runtime.o: runtime.c | tmp
	$(CC) $(CFLAGS) -c -o $@ $<

tmp:
	mkdir -p $@
