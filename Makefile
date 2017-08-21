CONFIGUREFLAGS = --enable-tests

SETUP = ocaml setup.ml

default: test

configure:
	oasis setup
	$(SETUP) -configure $(CONFIGUREFLAGS) > /dev/null

build: configure
	$(SETUP) -build

test: build
	$(SETUP) -test

doc: build
	$(SETUP) -doc

all: 
	oasis setup
	$(SETUP) -all 

clean:
	$(SETUP) -clean
	rm -rf myocamlbuild.ml setup.data setup.ml _tags
	rm -rf src/lib/symon_lib.*

deps:
	opam install menhir oasis ounit ocamlgraph

.PHONY: all build clean configure deps doc test 