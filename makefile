test: problem_48.ml
	ocaml problem_48.ml

a.out: problem_48.ml
	ocamlc problem_48.ml

run:
	./a.out

clean:
	rm *.cmi *.cmo *.out
