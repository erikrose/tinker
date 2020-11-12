all:
	ocamlbuild -pkgs llvm,llvm.bitreader test.native

clean:
	rm -rf _build test.native
