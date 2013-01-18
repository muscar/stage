all: common frontend backend stage.ml
	ocamlc utils.cmo common.cmo syntax.cmo lexer.cmo parser.cmo compiler.cmo stage.ml -o stage

common: utils.ml common.ml
	ocamlc -c utils.ml common.ml

frontend: parser lexer

parser: common syntax.ml parser.mly
	ocamlc -c syntax.ml
	ocamlyacc parser.mly
	ocamlc -c parser.mli parser.ml

lexer: common lexer.mll
	ocamllex lexer.mll
	ocamlc -c lexer.ml

backend: common bytecode.ml compiler.ml
	ocamlc -c bytecode.ml
	ocamlfind ocamlc \
		-package bitstring,bitstring.syntax -syntax bitstring.syntax \
		-syntax camlp4o \
		-linkpkg -c binaryfmt.ml
	ocamlc -c compiler.ml

clean:
	rm -f parser.ml parser.mli lexer.ml
	rm -f *.cmi *.cmo
	rm -f stage
	rm -f *~
