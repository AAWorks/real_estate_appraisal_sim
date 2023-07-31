deps:
	opam install . --deps-only --locked
build:
	dune build
run:
	dune exec ./bin/main.exe
test:
	dune test