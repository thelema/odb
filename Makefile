odb: odb.ml
	ocamlfind ocamlopt -linkpkg -package batteries,netclient -g -w Z odb.ml -o odb
