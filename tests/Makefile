OCAMLBUILD=ocamlbuild -classic-display  -pkgs oUnit,ocamlgraph

FLAGS=-Is src/application,src/lib

TARGET:=test.ml

BIN_NAT:=$(patsubst %.ml, %.native, $(TARGET))

all: native

native:
	@ln -s ../src .
	$(OCAMLBUILD) $(FLAGS) $(BIN_NAT)
	@rm src
	./test.native

clean:
	$(OCAMLBUILD) -clean

.PHONY: all native
