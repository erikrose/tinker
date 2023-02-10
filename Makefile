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
	rm -rf _build compile.native build a.out

test:
	dune build test.exe
	_build/default/test.exe -ci true

debug_test:
	dune build test.bc
	# Do "break @dune__exe__Infer 76" to set a breakpoint. Dune mangles names.
	# OUnit2 somehow keeps breakpoints from working, so call the test routine directly rather than running run_test_tt_main.
	ocamldebug _build/default/test.bc

.PHONY: all clean run build test debug_test
