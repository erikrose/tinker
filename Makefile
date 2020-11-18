all: main

main: main.c code.o
	clang -O3 main.c code.o

code.o: compile.native
	./compile.native 2> code.ll
	clang -O3 -S code.ll
	clang -O3 -c code.s

compile.native:
	ocamlbuild -pkgs llvm,llvm.analysis compile.native

clean:
	rm -rf _build compile.native code.ll code.s code.o

.PHONY: all clean
