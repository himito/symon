all:
	ocaml setup.ml -configure --enable-tests > /dev/null
	ocaml setup.ml -build
	ocaml setup.ml -test

clean:
	rm -rf _build myocamlbuild.ml setup.data setup.ml setup.log _tags
	rm -rf symbolicMC.byte tests.native
	rm -f src/lib/symon_lib.* 

deps:
	opam install menhir oasis ounit 

.PHONY: all clean deps