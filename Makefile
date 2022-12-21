
all: lambda parser lexer main
	ocamlc -o top str.cma lambda.cmo parser.cmo lexer.cmo main.cmo

lambda: lambda.ml lambda.mli
	ocamlc -c lambda.mli lambda.ml

parser: parser.mly
	ocamlyacc parser.mly
	ocamlc -c parser.mli parser.ml

lexer: lexer.mll
	ocamllex lexer.mll
	ocamlc -c lexer.ml

main: main.ml
	ocamlc -c main.ml

clean:
	rm -f lexer.ml parser.mli parser.ml *.cmi *.cmo *~

