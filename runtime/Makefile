.PHONY: clean

runtime.ll: runtime.c runtime.h
	clang -O3 -c -S -emit-llvm -o $@ $<

clean:
	-rm *.ll