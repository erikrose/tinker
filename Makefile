all: a.out

a.out: build/code.o
	clang -O3 build/code.o

run: a.out
	# Emits the return value of puts()
	./a.out

build:
	mkdir -p build

build/code.ll: build compile.native
	./compile.native 2> build/code.ll

# Turn bitcode into native asm:
build/code.s: build/code.ll
	llc build/code.ll

build/code.o: build/code.s
	cd build && clang -O3 -c code.s

compile.native: *.ml
	rm -f build/code.o
	ocamlbuild -pkgs llvm,llvm.analysis compile.native

clean:
	rm -rf _build compile.native build a.out test.native

test: _build/test.native
	./test.native -ci true

_build/test.native: *.ml
	ocamlbuild -pkgs llvm,llvm.analysis,ounit2 test.native

.PHONY: all clean run build test