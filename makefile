test: problem_47.ml
	ocaml problem_47.ml

a.out: problem_47.ml
	ocamlc problem_47.ml

run:
	./a.out

clean:
	rm *.cmi *.cmo *.out
