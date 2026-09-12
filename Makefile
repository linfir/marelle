runtime.o: runtime.c
	$(CC) $(CFLAGS) -O2 -c -o $@ $<
