deps:
	opam install . --deps-only --locked
build:
	dune build
run:
	dune exec real_estate_appraisal_sim