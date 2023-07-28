deps:
	opam install . --deps-only --locked
build:
	dune build
test:
	dune test