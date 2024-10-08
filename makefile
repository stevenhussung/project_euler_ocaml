test: problem_49.ml
	ocaml problem_49.ml

a.out: problem_49.ml
	ocamlc problem_49.ml

run:
	./a.out

clean:
	rm *.cmi *.cmo *.out
