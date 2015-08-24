OCAMLC=ocamlc
OCAMLBUILD=ocamlbuild -classic-display -use-menhir -menhir "menhir -v --external-tokens Lexer" -use-ocamlfind -package ocamlgraph -package str 
JSBUILD=js_of_ocaml

FLAGS=-Is application,lib
JS_FLAGS=-use-ocamlfind -pkg js_of_ocaml -pkg js_of_ocaml.syntax -syntax camlp4o

TARGET:=application/symbolicMC.ml
JS_TARGET:=application/JSClient.ml

BIN_NAT:=$(patsubst %.ml, %.native, $(TARGET))
BIN_BYTE:=$(patsubst %.ml, %.byte, $(JS_TARGET))
JS_FILE:=$(patsubst %.ml, %.js, $(JS_TARGET))

# all: byte
all: native 

byte:
									$(OCAMLBUILD) $(JS_FLAGS) $(FLAGS) $(BIN_BYTE)
									cp $(addprefix _build/, $(BIN_BYTE)) ..
									$(JSBUILD) JSClient.byte
									mv JSClient.js application

native:
									$(OCAMLBUILD) $(FLAGS) $(BIN_NAT)
									cp $(addprefix _build/, $(BIN_NAT)) ..

clean:
									$(OCAMLBUILD) -clean

cleanall: clean
									find . -iname *~ -exec rm {} \;

realclean: cleanall

.PHONY: all byte native clean cleanall
