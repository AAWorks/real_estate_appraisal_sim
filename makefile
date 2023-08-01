deps:
	opam install . --deps-only --locked
build:
	dune build
run_base:
	dune exec ./bin/main.exe
test:
	dune test