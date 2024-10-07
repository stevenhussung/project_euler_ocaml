test: problem_46.ml
	ocaml problem_46.ml

a.out: problem_46.ml
	ocamlc problem_46.ml

run:
	./a.out

clean:
	rm *.cmi *.cmo *.out
