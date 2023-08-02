deps:
	opam update
	opam install postgres_async
	opam install ppx_jsonaf_conv 
build:
	dune build
run_base:
	dune exec ./bin/main.exe
test:
	dune test