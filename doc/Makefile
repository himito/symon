OCAMLBUILD=ocamlbuild -classic-display -pkgs ocamlgraph

FLAGS=-Is src/application,src/lib

doc_html:
	@if [ ! -e "src" ]; then ln -s ../src . ; fi	
	$(OCAMLBUILD) $(FLAGS) api.docdir/index.html
	@rm src

clean:
	$(OCAMLBUILD) -clean