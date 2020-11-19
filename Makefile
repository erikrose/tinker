all: a.out

a.out: main.c build/code.o
	clang -O3 main.c build/code.o

run: a.out
	@./a.out

build/code.o: compile.native
	mkdir -p build
	./compile.native 2> build/code.ll
	cd build && clang -O3 -S code.ll
	cd build && clang -O3 -c code.s

compile.native: *.ml
	rm -f code.o
	ocamlbuild -pkgs llvm,llvm.analysis compile.native

clean:
	rm -rf _build compile.native build a.out

.PHONY: all clean run
