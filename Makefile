all: a.out

a.out: build/code.o print_double.c
	clang -O3 print_double.c build/code.o

run: a.out
	# Emits 160 (which is probably the integral representation of 33 as a float).
	./a.out

build:
	mkdir -p build

build/code.ll: build compile.native
	./compile.native 2> build/code.ll

build/code.s: build/code.ll
	cd build && clang -O3 -S code.ll

build/code.o: build/code.s
	cd build && clang -O3 -c code.s

compile.native: *.ml
	rm -f code.o
	ocamlbuild -pkgs llvm,llvm.analysis compile.native

clean:
	rm -rf _build compile.native build a.out

.PHONY: all clean run build