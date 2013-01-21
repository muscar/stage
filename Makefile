all: common frontend backend stage.ml
	ocamlfind ocamlc \
		-package bitstring,extlib \
		-linkpkg utils.cmo common.cmo syntax.cmo lexer.cmo parser.cmo emit.cmo compiler.cmo stage.ml -o stage

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

backend: common emit.ml compiler.ml
	ocamlfind ocamlc -package extlib -c emit.ml
	ocamlfind ocamlc \
		-package bitstring,bitstring.syntax,monad-custom -syntax bitstring.syntax,monad-custom,camlp4o \
		-linkpkg -c compiler.ml

clean:
	rm -f parser.ml parser.mli lexer.ml
	rm -f *.cm[iox]
	rm -f stage
	rm -f *~
