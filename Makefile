.PHONY: run

run: out
	./out

out.ssa: compiler.ss
	csi -s $< > $@

%.s: %.ssa
	qbe -o $@ $<

%.o: %.c
	gcc -O2 -c -o $@ $<

out: out.s runtime.o
	cc -o $@ $^
