test: problem_50.ml
	ocaml problem_50.ml

a.out: problem_50.ml
	ocamlc problem_50.ml

run:
	./a.out

clean:
	rm *.cmi *.cmo *.out
