CFLAGS ?= -O2 -Wall -Wextra -Wpedantic -Wconversion -Wshadow

runtime.o: runtime.c
	$(CC) $(CFLAGS) -c -o $@ $<
